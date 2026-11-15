"use client";

import { useEffect, useMemo, useState, useTransition } from "react";
import { LeaderboardPanel } from "@/components/LeaderboardPanel";
import type { ManualOverride, Title } from "@/lib/database.types";
import { ratingForDrop } from "@/lib/drag";
import { createClient } from "@/lib/supabase/client";

export interface LeaderboardClientProps {
  initialTitles: Title[];
  initialOverrides: ManualOverride[];
  /** When false (e.g. global view, signed-out user), drag is disabled. */
  draggable: boolean;
  userId: string | null;
  compact?: boolean;
  limit?: number;
}

/**
 * Client-side leaderboard list with drag-to-reorder. Owns the local sort so
 * a drop reorders instantly; the override is persisted to Supabase in the
 * background. Rerenders animate via framer-motion in the panel itself.
 */
export function LeaderboardClient({
  initialTitles,
  initialOverrides,
  draggable,
  userId,
  compact = false,
  limit
}: LeaderboardClientProps) {
  const supabase = useMemo(() => createClient(), []);
  const [titles, setTitles] = useState<Title[]>(initialTitles);
  const [overrides, setOverrides] = useState<Map<string, number>>(() => {
    const m = new Map<string, number>();
    for (const o of initialOverrides) m.set(o.title_id, o.rating);
    return m;
  });
  const [, startTransition] = useTransition();
  const [error, setError] = useState<string | null>(null);

  // If the parent re-fetches and passes new titles down, sync.
  useEffect(() => {
    setTitles(initialTitles);
  }, [initialTitles]);

  const handleReorder = (movedId: string, from: number, to: number) => {
    if (!draggable || !userId || from === to) return;
    const target = ratingForDrop(titles, from, to);

    // Optimistic local update.
    const next = titles.slice();
    const [moved] = next.splice(from, 1);
    const movedWithRating = { ...moved, rating: target };
    next.splice(to, 0, movedWithRating);
    next.sort((a, b) => b.rating - a.rating);
    setTitles(next);
    setOverrides((prev) => {
      const m = new Map(prev);
      m.set(movedId, target);
      return m;
    });

    startTransition(async () => {
      const { error: upsertErr } = await supabase
        .from("manual_overrides")
        .upsert(
          {
            user_id: userId,
            title_id: movedId,
            rating: target
          },
          { onConflict: "user_id,title_id" }
        );
      if (upsertErr) setError(upsertErr.message);
    });
  };

  return (
    <div>
      {error && (
        <p className="mb-3 rounded-xl border border-ember/30 bg-ember/10 p-3 text-sm text-ember">
          Couldn&apos;t save that move: {error}
        </p>
      )}
      <LeaderboardPanel
        titles={titles}
        onReorder={draggable ? handleReorder : undefined}
        compact={compact}
        limit={limit}
      />
    </div>
  );
}
