import type { Title } from "./database.types";

export interface MatchupOptions {
  /** Pool of titles to draw from (already filtered by kind / etc.). */
  pool: Title[];
  /** The previous matchup, to avoid an immediate repeat. */
  previous?: { a: string; b: string } | null;
  /** Soft cap on Elo distance for the second pick within a bucket. */
  ratingWindow?: number;
}

type WeightedItem<T> = { item: T; weight: number };

function weightedPick<T>(items: WeightedItem<T>[]): T {
  const total = items.reduce((s, i) => s + i.weight, 0);
  if (total <= 0) return items[Math.floor(Math.random() * items.length)].item;
  let r = Math.random() * total;
  for (const it of items) {
    r -= it.weight;
    if (r <= 0) return it.item;
  }
  return items[items.length - 1].item;
}

function uniformPick<T>(items: T[]): T {
  return items[Math.floor(Math.random() * items.length)];
}

/**
 * Pick a tie-resolving matchup.
 *
 * The pool is assumed to already be a single kind (movies, series, ...).
 * Within that pool we *strongly* prefer pairs where `your_rating` matches —
 * those are the ties you actually want resolved. Bigger buckets get a
 * proportionally bigger share of comparisons so an 8/10 bucket of 150 titles
 * isn't starved by a 10/10 bucket of 18.
 *
 * When the first pick has no other titles in its star bucket (or repeats),
 * we fall back to neighbouring buckets so the user still sees a question.
 */
export function pickMatchup({
  pool,
  previous = null,
  ratingWindow = 100
}: MatchupOptions): [Title, Title] | null {
  if (pool.length < 2) return null;

  // Group by your_rating; only buckets with >= 2 titles can produce a tie.
  const buckets = new Map<number, Title[]>();
  for (const t of pool) {
    const arr = buckets.get(t.your_rating) ?? [];
    arr.push(t);
    buckets.set(t.your_rating, arr);
  }

  // Weighted pick of bucket — more titles in a bucket means more comparisons
  // to do, so weight by (n choose 2) ≈ n^2.
  const tiedBuckets = Array.from(buckets.entries()).filter(
    ([, arr]) => arr.length >= 2
  );

  if (tiedBuckets.length > 0) {
    const bucket = weightedPick(
      tiedBuckets.map(([, arr]) => ({ item: arr, weight: arr.length * arr.length }))
    );
    const pair = pickInBucket(bucket, previous, ratingWindow);
    if (pair) return pair;
  }

  // Degenerate fallback: every star bucket has exactly one title (or after
  // filtering out the previous matchup we lose all of them). Pick any two
  // close-by titles by Elo.
  return pickAnyClose(pool, previous, ratingWindow);
}

function pickInBucket(
  bucket: Title[],
  previous: { a: string; b: string } | null,
  ratingWindow: number
): [Title, Title] | null {
  if (bucket.length < 2) return null;

  // First pick: avoid both members of the previous matchup if possible.
  const firstCandidates =
    previous == null
      ? bucket
      : bucket.filter((t) => t.id !== previous.a && t.id !== previous.b);
  const a = uniformPick(firstCandidates.length >= 1 ? firstCandidates : bucket);

  // Second pick: prefer titles whose current Elo is close to a, so we end up
  // forcing ambiguous decisions instead of obvious blowouts.
  const others = bucket.filter((t) => t.id !== a.id);
  if (others.length === 0) return null;

  const nearby = others.filter(
    (t) => Math.abs(t.rating - a.rating) <= ratingWindow
  );
  let partnerPool = nearby.length > 0 ? nearby : others;

  if (previous) {
    const filtered = partnerPool.filter(
      (t) =>
        !(
          (t.id === previous.a && a.id === previous.b) ||
          (t.id === previous.b && a.id === previous.a)
        )
    );
    if (filtered.length > 0) partnerPool = filtered;
  }

  const b = uniformPick(partnerPool);
  return [a, b];
}

function pickAnyClose(
  pool: Title[],
  previous: { a: string; b: string } | null,
  ratingWindow: number
): [Title, Title] | null {
  if (pool.length < 2) return null;
  const candidates =
    previous == null
      ? pool
      : pool.filter((t) => t.id !== previous.a && t.id !== previous.b);
  const firstPool = candidates.length >= 2 ? candidates : pool;
  const a = uniformPick(firstPool);

  const others = pool.filter((t) => t.id !== a.id);
  // Prefer same star bucket first, then nearby star bucket, then anywhere.
  let pref = others.filter((t) => t.your_rating === a.your_rating);
  if (pref.length === 0) {
    pref = others.filter((t) => Math.abs(t.your_rating - a.your_rating) <= 1);
  }
  if (pref.length === 0) pref = others;

  const nearby = pref.filter(
    (t) => Math.abs(t.rating - a.rating) <= ratingWindow
  );
  let partnerPool = nearby.length > 0 ? nearby : pref;

  if (previous) {
    const filtered = partnerPool.filter(
      (t) =>
        !(
          (t.id === previous.a && a.id === previous.b) ||
          (t.id === previous.b && a.id === previous.a)
        )
    );
    if (filtered.length > 0) partnerPool = filtered;
  }

  return [a, uniformPick(partnerPool)];
}
