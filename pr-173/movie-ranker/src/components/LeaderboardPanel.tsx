"use client";

import { Reorder, motion, AnimatePresence } from "framer-motion";
import type { Title } from "@/lib/database.types";

export interface LeaderboardPanelProps {
  /** Already sorted by rating desc. */
  titles: Title[];
  /** When set, highlight these ids (e.g. the current matchup). */
  highlightIds?: ReadonlySet<string>;
  /**
   * If provided, rows are draggable. Called when the user drops a row
   * at a new position. `from` and `to` are 0-indexed positions in the
   * provided `titles` array.
   */
  onReorder?: (movedId: string, from: number, to: number) => void;
  /** Compact = sidekick rendering on /compare. Default = full /leaderboard rows. */
  compact?: boolean;
  /** Limit rows shown. */
  limit?: number;
  /** Rank label shown for the first row (default 1). Useful when rendering a window. */
  startRank?: number;
}

export function LeaderboardPanel({
  titles,
  highlightIds,
  onReorder,
  compact = false,
  limit,
  startRank = 1
}: LeaderboardPanelProps) {
  const visible = limit != null ? titles.slice(0, limit) : titles;
  const draggable = onReorder != null;

  if (visible.length === 0) {
    return (
      <p className="py-10 text-center text-sm text-mist">
        Nothing to show yet.
      </p>
    );
  }

  // Drag-enabled: Reorder.Group manages the drag handle and visual reorder.
  // We translate Framer's "new order array" into a (from, to) callback.
  if (draggable) {
    return (
      <Reorder.Group
        axis="y"
        values={visible}
        onReorder={(next) => {
          // Find the index that changed.
          for (let i = 0; i < visible.length; i++) {
            if (visible[i].id !== next[i]?.id) {
              const moved = next[i];
              const from = visible.findIndex((t) => t.id === moved.id);
              if (from !== -1 && from !== i) onReorder(moved.id, from, i);
              return;
            }
          }
        }}
        className="space-y-2"
      >
        <AnimatePresence initial={false}>
          {visible.map((t, i) => (
            <Reorder.Item
              key={t.id}
              value={t}
              layout
              initial={{ opacity: 0, y: 8 }}
              animate={{ opacity: 1, y: 0 }}
              exit={{ opacity: 0 }}
              whileDrag={{
                scale: 1.02,
                boxShadow: "0 18px 40px -12px rgba(0,0,0,0.7)",
                zIndex: 10
              }}
              transition={{ type: "spring", stiffness: 500, damping: 38 }}
              className="cursor-grab active:cursor-grabbing"
            >
              <Row
                title={t}
                rank={startRank + i}
                compact={compact}
                highlighted={highlightIds?.has(t.id)}
                draggable
              />
            </Reorder.Item>
          ))}
        </AnimatePresence>
      </Reorder.Group>
    );
  }

  // Read-only list — still use motion.li layout so external rating
  // changes animate the rows into their new positions.
  return (
    <ol className="space-y-2">
      <AnimatePresence initial={false}>
        {visible.map((t, i) => (
          <motion.li
            key={t.id}
            layout
            initial={{ opacity: 0, y: 8 }}
            animate={{ opacity: 1, y: 0 }}
            exit={{ opacity: 0 }}
            transition={{ type: "spring", stiffness: 500, damping: 38 }}
          >
            <Row
              title={t}
              rank={startRank + i}
              compact={compact}
              highlighted={highlightIds?.has(t.id)}
            />
          </motion.li>
        ))}
      </AnimatePresence>
    </ol>
  );
}

function Row({
  title,
  rank,
  compact,
  highlighted,
  draggable
}: {
  title: Title;
  rank: number;
  compact: boolean;
  highlighted?: boolean;
  draggable?: boolean;
}) {
  const total = title.wins + title.losses;
  const winPct = total === 0 ? 0 : Math.round((title.wins / total) * 100);

  return (
    <div
      className={[
        "flex items-center gap-3 rounded-2xl border bg-slate transition-colors",
        compact ? "p-2 sm:p-2.5" : "p-3 sm:p-4",
        highlighted
          ? "border-gold/60 bg-gold/[0.04]"
          : "border-white/5 hover:border-white/10"
      ].join(" ")}
    >
      <div
        className={[
          "shrink-0 text-center font-display font-semibold tabular-nums",
          compact ? "w-6 text-base text-mist" : "w-8 text-xl text-mist"
        ].join(" ")}
      >
        {rank}
      </div>
      {draggable && (
        <span
          aria-hidden
          className="shrink-0 select-none text-xs leading-none text-mist/60"
          title="Drag to reorder"
        >
          ⋮⋮
        </span>
      )}
      <div className="min-w-0 flex-1">
        <div className="flex items-baseline gap-2">
          <h3
            className={[
              "truncate font-display font-semibold",
              compact ? "text-sm sm:text-base" : "text-lg sm:text-xl"
            ].join(" ")}
          >
            {title.title}
          </h3>
          {!compact && (
            <span className="shrink-0 text-xs text-mist">
              {title.year ?? ""}
            </span>
          )}
        </div>
        {!compact && (
          <div className="mt-0.5 flex flex-wrap gap-x-3 text-[11px] uppercase tracking-wider text-mist">
            <span>{title.your_rating}/10</span>
            {title.primary_genre && <span>{title.primary_genre}</span>}
            {title.directors[0] && <span>· {title.directors[0]}</span>}
            {title.imdb_rating != null && <span>· IMDb {title.imdb_rating}</span>}
          </div>
        )}
        {compact && (
          <div className="text-[10px] uppercase tracking-wider text-mist">
            {title.your_rating}/10
            {title.year ? ` · ${title.year}` : ""}
          </div>
        )}
      </div>
      <div className="text-right">
        <div
          className={[
            "font-display font-semibold tabular-nums",
            compact ? "text-base" : "text-2xl"
          ].join(" ")}
        >
          {title.rating}
        </div>
        {!compact && (
          <div className="text-[11px] uppercase tracking-wider text-mist">
            {title.wins}W · {title.losses}L · {winPct}%
          </div>
        )}
      </div>
    </div>
  );
}
