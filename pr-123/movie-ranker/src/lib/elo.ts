export const STARTING_RATING = 1200;
export const K_FACTOR = 32;

export interface EloUpdate {
  newWinnerRating: number;
  newLoserRating: number;
  winnerDelta: number;
  loserDelta: number;
}

/**
 * Standard Elo with K = 32. Identical to food-ranker so personal and global
 * leaderboards stay comparable mentally.
 */
export function computeElo(
  winnerRating: number,
  loserRating: number,
  k: number = K_FACTOR
): EloUpdate {
  const expectedWinner =
    1 / (1 + Math.pow(10, (loserRating - winnerRating) / 400));
  const expectedLoser =
    1 / (1 + Math.pow(10, (winnerRating - loserRating) / 400));

  const newWinnerRating = winnerRating + k * (1 - expectedWinner);
  const newLoserRating = loserRating + k * (0 - expectedLoser);

  return {
    newWinnerRating: Math.round(newWinnerRating),
    newLoserRating: Math.round(newLoserRating),
    winnerDelta: Math.round(newWinnerRating - winnerRating),
    loserDelta: Math.round(newLoserRating - loserRating)
  };
}

/**
 * Per-bucket starting Elo. Mirrors scripts/build_seed.mjs:ANCHOR so a
 * personal leaderboard built from comparisons alone agrees with the seeded
 * global Elo on day one (everything in bucket N starts at the same anchor).
 */
export function bucketAnchor(yourRating: number): number {
  return 800 + yourRating * 50;
}
