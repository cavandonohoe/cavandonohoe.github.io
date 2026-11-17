import type { Kind, Title } from "./database.types";

/**
 * The visible "what kind of comparison are we doing?" axis on the home /
 * compare / leaderboard pages. Mini-series get bundled into series so that
 * Severance and Chernobyl can fight each other.
 */
export type CompareKind = "movie" | "series" | "short" | "video_game" | "other";

export const COMPARE_KIND_LABEL: Record<CompareKind, string> = {
  movie: "Movies",
  series: "Series",
  short: "Shorts",
  video_game: "Video games",
  other: "Other"
};

const KIND_TO_COMPARE: Record<Kind, CompareKind> = {
  movie: "movie",
  tv_movie: "movie",
  series: "series",
  mini_series: "series",
  short: "short",
  tv_short: "short",
  tv_episode: "short",
  video: "short",
  tv_special: "short",
  video_game: "video_game",
  other: "other"
};

export function compareKindOf(t: Pick<Title, "kind">): CompareKind {
  return KIND_TO_COMPARE[t.kind] ?? "other";
}

export const COMPARE_KINDS: CompareKind[] = [
  "movie",
  "series",
  "short",
  "video_game",
  "other"
];
