import type { Title } from "./database.types";

/**
 * Compute the rating to assign a title that the user just dragged into a new
 * position, so that sorting by `rating desc` keeps it at that position.
 *
 * `sorted` is the current leaderboard array, BEFORE the move. The dragged
 * title sits at index `from`; the user wants it at index `to`.
 *
 * Strategy: pick a value strictly between the post-move neighbors. We bias
 * the value toward an integer midpoint and fall back to ±16 when at the
 * edges (~one Elo K-step worth, plenty of headroom).
 */
export function ratingForDrop(
  sorted: Title[],
  from: number,
  to: number
): number {
  if (from === to) return sorted[from].rating;
  // Build the post-move list to identify neighbors.
  const after = sorted.slice();
  const [moved] = after.splice(from, 1);
  after.splice(to, 0, moved);

  const above = to > 0 ? after[to - 1] : null;
  const below = to < after.length - 1 ? after[to + 1] : null;

  if (above && below) {
    // Standard: midpoint between neighbors. If they're equal (rare but
    // possible right after seeding), nudge a fraction so sort is stable.
    if (above.rating === below.rating) {
      return above.rating; // ties broken by stable sort; +0 keeps the move
    }
    return Math.floor((above.rating + below.rating) / 2);
  }
  if (above && !below) {
    // Dropped at very bottom. Stay below `above`.
    return above.rating - 16;
  }
  if (!above && below) {
    // Dropped at very top. Stay above `below`.
    return below.rating + 16;
  }
  // Single-item list — no-op.
  return moved.rating;
}
