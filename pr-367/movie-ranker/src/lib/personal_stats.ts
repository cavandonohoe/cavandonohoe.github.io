import type { Comparison, ManualOverride, Title } from "./database.types";
import { bucketAnchor, computeElo } from "./elo";

export interface PersonalStat {
  rating: number;
  wins: number;
  losses: number;
  /** True if the latest authoritative source is a manual drag, not a vote. */
  manual?: boolean;
}

/**
 * Re-derive personal Elo state from a user's comparison history. Each title
 * starts at its star-bucket anchor, then comparisons replay in chronological
 * order. Manual drag-to-reorder overrides are applied last and clobber the
 * Elo rating for that title only — wins/losses stay accurate.
 */
export function derivePersonalStats(
  comparisons: Comparison[],
  titles: Title[],
  overrides: ManualOverride[] = []
): Map<string, PersonalStat> {
  const stats = new Map<string, PersonalStat>();
  for (const t of titles) {
    stats.set(t.id, {
      rating: bucketAnchor(t.your_rating),
      wins: 0,
      losses: 0
    });
  }

  const ordered = [...comparisons].sort((a, b) =>
    a.created_at.localeCompare(b.created_at)
  );

  for (const c of ordered) {
    const w = stats.get(c.winner_title_id);
    const l = stats.get(c.loser_title_id);
    if (!w || !l) continue;
    const { newWinnerRating, newLoserRating } = computeElo(w.rating, l.rating);
    stats.set(c.winner_title_id, {
      rating: newWinnerRating,
      wins: w.wins + 1,
      losses: w.losses
    });
    stats.set(c.loser_title_id, {
      rating: newLoserRating,
      wins: l.wins,
      losses: l.losses + 1
    });
  }

  for (const o of overrides) {
    const cur = stats.get(o.title_id);
    if (!cur) continue;
    stats.set(o.title_id, { ...cur, rating: o.rating, manual: true });
  }

  return stats;
}

/** Annotate titles with personal stats and sort by personal rating desc. */
export function applyPersonalStats(
  titles: Title[],
  stats: Map<string, PersonalStat>
): Title[] {
  return titles
    .map((t) => {
      const s = stats.get(t.id);
      if (!s) {
        return {
          ...t,
          rating: bucketAnchor(t.your_rating),
          wins: 0,
          losses: 0
        };
      }
      return { ...t, rating: s.rating, wins: s.wins, losses: s.losses };
    })
    .sort((a, b) => b.rating - a.rating);
}
