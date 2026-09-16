"use client";

import { Suspense, useCallback, useEffect, useMemo, useRef, useState } from "react";
import { useRouter, useSearchParams } from "next/navigation";
import Link from "next/link";
import { LeaderboardPanel } from "@/components/LeaderboardPanel";
import { TitleCard } from "@/components/TitleCard";
import { createClient } from "@/lib/supabase/client";
import type { ManualOverride, Title } from "@/lib/database.types";
import { ratingForDrop } from "@/lib/drag";
import { computeElo } from "@/lib/elo";
import {
  COMPARE_KIND_LABEL,
  COMPARE_KINDS,
  type CompareKind,
  compareKindOf
} from "@/lib/kinds";
import { pickMatchup } from "@/lib/matching";
import { applyPersonalStats, derivePersonalStats } from "@/lib/personal_stats";

type Status = "loading" | "ready" | "submitting" | "empty" | "error";
type Scope = "personal" | "global";

const STAR_VALUES = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1] as const;

function isCompareKind(v: string | null): v is CompareKind {
  return v != null && (COMPARE_KINDS as string[]).includes(v);
}

function parseStars(v: string | null): number | null {
  if (v == null) return null;
  const n = Number(v);
  return (STAR_VALUES as readonly number[]).includes(n) ? n : null;
}

function ComparePageInner() {
  const supabase = useMemo(() => createClient(), []);
  const router = useRouter();
  const searchParams = useSearchParams();

  const kind: CompareKind = isCompareKind(searchParams.get("kind"))
    ? (searchParams.get("kind") as CompareKind)
    : "movie";
  const scope: Scope = searchParams.get("scope") === "global" ? "global" : "personal";
  const stars: number | null = parseStars(searchParams.get("stars"));

  // Pool tracks the *active* (kind-filtered) titles with up-to-date personal Elo.
  const [pool, setPool] = useState<Title[]>([]);
  const [matchup, setMatchup] = useState<[Title, Title] | null>(null);
  const [status, setStatus] = useState<Status>("loading");
  const [error, setError] = useState<string | null>(null);
  const [highlight, setHighlight] = useState<"a" | "b" | null>(null);
  const [userId, setUserId] = useState<string | null>(null);
  const [comparisonsCount, setComparisonsCount] = useState(0);
  const previousRef = useRef<{ a: string; b: string } | null>(null);

  useEffect(() => {
    let cancelled = false;
    setStatus("loading");
    setError(null);
    previousRef.current = null;
    (async () => {
      const [{ data: titlesData, error: titlesError }, { data: userData }] =
        await Promise.all([
          supabase.from("titles").select("*"),
          supabase.auth.getUser()
        ]);
      if (cancelled) return;
      if (titlesError) {
        setError(titlesError.message);
        setStatus("error");
        return;
      }
      if (!userData.user) {
        setError("You need to sign in to compare.");
        setStatus("error");
        return;
      }
      setUserId(userData.user.id);

      const allTitles = (titlesData ?? []) as Title[];
      const kindTitles = allTitles
        .filter((t) => compareKindOf(t) === kind)
        .filter((t) => stars == null || t.your_rating === stars);

      let workingPool = kindTitles;
      let countForKind = 0;
      if (scope === "personal") {
        const [{ data: cmpData, error: cmpErr }, { data: ovData, error: ovErr }] =
          await Promise.all([
            supabase
              .from("comparisons")
              .select("*")
              .eq("user_id", userData.user.id),
            supabase
              .from("manual_overrides")
              .select("*")
              .eq("user_id", userData.user.id)
          ]);
        if (cancelled) return;
        if (cmpErr) {
          setError(cmpErr.message);
          setStatus("error");
          return;
        }
        if (ovErr) {
          setError(ovErr.message);
          setStatus("error");
          return;
        }
        const allComparisons = cmpData ?? [];
        const overrides = (ovData ?? []) as ManualOverride[];
        const stats = derivePersonalStats(allComparisons, allTitles, overrides);
        workingPool = applyPersonalStats(kindTitles, stats);

        const kindIds = new Set(kindTitles.map((t) => t.id));
        countForKind = allComparisons.filter(
          (c) => kindIds.has(c.winner_title_id) || kindIds.has(c.loser_title_id)
        ).length;
      }
      setComparisonsCount(countForKind);

      if (workingPool.length < 2) {
        setPool(workingPool);
        setStatus("empty");
        return;
      }
      setPool(workingPool);
      setMatchup(pickMatchup({ pool: workingPool }));
      setStatus("ready");
    })();
    return () => {
      cancelled = true;
    };
  }, [supabase, kind, scope, stars]);

  const advance = useCallback(
    (currentPool: Title[], lastPair: [Title, Title]) => {
      previousRef.current = { a: lastPair[0].id, b: lastPair[1].id };
      const next = pickMatchup({
        pool: currentPool,
        previous: previousRef.current
      });
      setMatchup(next);
      setHighlight(null);
      setStatus("ready");
    },
    []
  );

  const handleSelect = useCallback(
    async (winnerSide: "a" | "b") => {
      if (!matchup || status !== "ready" || !userId) return;
      setStatus("submitting");
      setHighlight(winnerSide);

      const [a, b] = matchup;
      const winner = winnerSide === "a" ? a : b;
      const loser = winnerSide === "a" ? b : a;

      const { newWinnerRating, newLoserRating } = computeElo(
        winner.rating,
        loser.rating
      );

      // Optimistically advance personal pool. Global Elo writes happen in
      // the background but the user-facing pool here is the personal one.
      const updatedWinner: Title = {
        ...winner,
        rating: newWinnerRating,
        wins: winner.wins + 1
      };
      const updatedLoser: Title = {
        ...loser,
        rating: newLoserRating,
        losses: loser.losses + 1
      };

      const nextPool = pool
        .map((t) => {
          if (t.id === updatedWinner.id) return updatedWinner;
          if (t.id === updatedLoser.id) return updatedLoser;
          return t;
        })
        .sort((x, y) => y.rating - x.rating);
      setPool(nextPool);

      const writes: PromiseLike<{ error: { message: string } | null }>[] = [
        supabase.from("comparisons").insert({
          user_id: userId,
          winner_title_id: winner.id,
          loser_title_id: loser.id,
          winner_rating_before: winner.rating,
          loser_rating_before: loser.rating,
          winner_rating_after: updatedWinner.rating,
          loser_rating_after: updatedLoser.rating
        })
      ];
      if (scope === "global") {
        // When voting on the shared global view, also write back the global
        // Elo on the shared `titles` row. In personal scope we leave the
        // global Elo alone — your personal vote shouldn't move the global
        // leaderboard if you didn't ask for it to.
        writes.push(
          supabase
            .from("titles")
            .update({
              rating: updatedWinner.rating,
              wins: updatedWinner.wins
            })
            .eq("id", winner.id)
        );
        writes.push(
          supabase
            .from("titles")
            .update({
              rating: updatedLoser.rating,
              losses: updatedLoser.losses
            })
            .eq("id", loser.id)
        );
      }

      const results = await Promise.all(writes);
      const failure = results.map((r) => r.error).find(Boolean);
      if (failure) {
        setError(failure.message);
        setStatus("error");
        return;
      }

      setComparisonsCount((c) => c + 1);
      setTimeout(() => advance(nextPool, [a, b]), 280);
    },
    [advance, matchup, pool, scope, status, supabase, userId]
  );

  const handleReorder = useCallback(
    async (movedId: string, from: number, to: number) => {
      if (scope !== "personal" || !userId || from === to) return;
      const target = ratingForDrop(pool, from, to);

      const next = pool.slice();
      const [moved] = next.splice(from, 1);
      next.splice(to, 0, { ...moved, rating: target });
      next.sort((a, b) => b.rating - a.rating);
      setPool(next);

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
    },
    [pool, scope, supabase, userId]
  );

  const setKind = (k: CompareKind) => {
    const params = new URLSearchParams();
    params.set("kind", k);
    if (scope !== "personal") params.set("scope", scope);
    if (stars != null) params.set("stars", String(stars));
    router.replace(`/compare?${params.toString()}`);
  };

  const setScope = (s: Scope) => {
    const params = new URLSearchParams();
    params.set("kind", kind);
    if (s !== "personal") params.set("scope", s);
    if (stars != null) params.set("stars", String(stars));
    router.replace(`/compare?${params.toString()}`);
  };

  const setStars = (s: number | null) => {
    const params = new URLSearchParams();
    params.set("kind", kind);
    if (scope !== "personal") params.set("scope", scope);
    if (s != null) params.set("stars", String(s));
    router.replace(`/compare?${params.toString()}`);
  };

  return (
    <section className="pt-2">
      <div className="mb-6 flex flex-col gap-3 sm:flex-row sm:items-center sm:justify-between">
        <KindTabs current={kind} onChange={setKind} />
        <ScopeTabs current={scope} onChange={setScope} />
      </div>

      <div className="mb-6">
        <StarTabs current={stars} onChange={setStars} />
      </div>

      <div className="mb-4 flex items-center justify-between text-xs uppercase tracking-[0.18em] text-mist">
        <span>
          {scope === "personal" ? "Your ranking" : "Shared ranking"} · {COMPARE_KIND_LABEL[kind]}
          {stars != null ? ` · ${stars}★ only` : ""}
        </span>
        <span>
          {comparisonsCount} comparison{comparisonsCount === 1 ? "" : "s"} · K = 32
        </span>
      </div>

      {status === "loading" && <Centered>Loading the catalog…</Centered>}

      {status === "empty" && (
        <Centered>
          <p className="font-display text-2xl">Nothing to compare here.</p>
          <p className="mt-2 text-mist">
            There aren&apos;t at least two {COMPARE_KIND_LABEL[kind].toLowerCase()} in
            the catalog yet.
          </p>
          <Link
            href="/leaderboard"
            className="mt-6 rounded-full border border-white/10 bg-slate px-5 py-2.5 text-sm text-ink hover:border-white/30"
          >
            See leaderboard
          </Link>
        </Centered>
      )}

      {status === "error" && (
        <Centered>
          <p className="font-display text-2xl text-ember">Something broke.</p>
          <p className="mt-2 text-mist">{error}</p>
        </Centered>
      )}

      {(status === "ready" || status === "submitting") && matchup && (
        <div className="grid gap-6 lg:grid-cols-[minmax(0,1fr)_360px] lg:gap-8">
          <div>
            <PromptLine matchup={matchup} />
            <div className="grid gap-4 sm:grid-cols-2 sm:gap-6">
              <TitleCard
                title={matchup[0]}
                onSelect={() => handleSelect("a")}
                disabled={status === "submitting"}
                highlight={
                  highlight === "a"
                    ? "winner"
                    : highlight === "b"
                      ? "loser"
                      : null
                }
              />
              <TitleCard
                title={matchup[1]}
                onSelect={() => handleSelect("b")}
                disabled={status === "submitting"}
                highlight={
                  highlight === "b"
                    ? "winner"
                    : highlight === "a"
                      ? "loser"
                      : null
                }
              />
            </div>
            <div className="mt-6 text-center text-xs text-mist">vs.</div>
          </div>
          <aside className="lg:sticky lg:top-6 lg:self-start">
            <SidePanel
              pool={pool}
              matchup={matchup}
              draggable={scope === "personal" && !!userId}
              onReorder={handleReorder}
            />
          </aside>
        </div>
      )}
    </section>
  );
}

function SidePanel({
  pool,
  matchup,
  draggable,
  onReorder
}: {
  pool: Title[];
  matchup: [Title, Title];
  draggable: boolean;
  onReorder: (movedId: string, from: number, to: number) => void;
}) {
  // Show a window of titles centered on the current matchup so the user can
  // see them shift in real time. ~15 rows: matchup ± neighbors.
  const ids = new Set([matchup[0].id, matchup[1].id]);
  const indices = pool
    .map((t, i) => (ids.has(t.id) ? i : -1))
    .filter((i) => i !== -1);
  const center =
    indices.length > 0
      ? Math.round(indices.reduce((a, b) => a + b, 0) / indices.length)
      : 0;
  const WINDOW = 15;
  const half = Math.floor(WINDOW / 2);
  let start = Math.max(0, center - half);
  const end = Math.min(pool.length, start + WINDOW);
  start = Math.max(0, end - WINDOW);
  const slice = pool.slice(start, end);
  const startRank = start + 1;

  return (
    <div className="rounded-3xl border border-white/5 bg-slate/40 p-4">
      <div className="mb-3 flex items-baseline justify-between">
        <h2 className="font-display text-lg font-semibold tracking-tight">
          Live ranking
        </h2>
        <Link
          href="/leaderboard"
          className="text-[11px] uppercase tracking-[0.18em] text-mist hover:text-ink"
        >
          Full board →
        </Link>
      </div>
      <p className="mb-3 text-[11px] uppercase tracking-[0.18em] text-mist">
        Ranks {startRank}–{startRank + slice.length - 1} of {pool.length}
        {draggable ? " · drag to reorder" : ""}
      </p>
      <LeaderboardPanel
        titles={slice}
        highlightIds={ids}
        onReorder={
          draggable
            ? (id, fromLocal, toLocal) =>
                onReorder(id, start + fromLocal, start + toLocal)
            : undefined
        }
        compact
        startRank={startRank}
      />
    </div>
  );
}

function PromptLine({ matchup }: { matchup: [Title, Title] }) {
  void matchup;
  return (
    <div className="mb-6 flex items-center justify-center gap-2 text-center text-sm text-mist">
      Pick the one you&apos;d watch again first.
    </div>
  );
}

function KindTabs({
  current,
  onChange
}: {
  current: CompareKind;
  onChange: (k: CompareKind) => void;
}) {
  return (
    <div className="inline-flex flex-wrap gap-1 rounded-full border border-white/10 bg-slate p-1">
      {COMPARE_KINDS.map((k) => {
        const active = k === current;
        return (
          <button
            key={k}
            type="button"
            onClick={() => onChange(k)}
            className={[
              "rounded-full px-3 py-1.5 text-sm font-medium transition",
              active
                ? "bg-gold text-cream"
                : "text-ink/70 hover:bg-white/5 hover:text-ink"
            ].join(" ")}
          >
            {COMPARE_KIND_LABEL[k]}
          </button>
        );
      })}
    </div>
  );
}

function ScopeTabs({
  current,
  onChange
}: {
  current: Scope;
  onChange: (s: Scope) => void;
}) {
  return (
    <div className="inline-flex rounded-full border border-white/10 bg-slate p-1">
      {(["personal", "global"] as Scope[]).map((s) => {
        const active = s === current;
        return (
          <button
            key={s}
            type="button"
            onClick={() => onChange(s)}
            className={[
              "rounded-full px-3 py-1.5 text-sm font-medium transition",
              active
                ? "bg-ink text-cream"
                : "text-ink/70 hover:bg-white/5 hover:text-ink"
            ].join(" ")}
          >
            {s === "personal" ? "Personal" : "Global"}
          </button>
        );
      })}
    </div>
  );
}

function StarTabs({
  current,
  onChange
}: {
  current: number | null;
  onChange: (s: number | null) => void;
}) {
  return (
    <div className="inline-flex flex-wrap gap-1 rounded-full border border-white/10 bg-slate p-1">
      <button
        type="button"
        onClick={() => onChange(null)}
        className={[
          "rounded-full px-3 py-1.5 text-sm font-medium transition",
          current == null
            ? "bg-gold text-cream"
            : "text-ink/70 hover:bg-white/5 hover:text-ink"
        ].join(" ")}
      >
        All
      </button>
      {STAR_VALUES.map((s) => {
        const active = s === current;
        return (
          <button
            key={s}
            type="button"
            onClick={() => onChange(s)}
            className={[
              "rounded-full px-3 py-1.5 text-sm font-medium transition",
              active
                ? "bg-gold text-cream"
                : "text-ink/70 hover:bg-white/5 hover:text-ink"
            ].join(" ")}
          >
            {s}★
          </button>
        );
      })}
    </div>
  );
}

function Centered({ children }: { children: React.ReactNode }) {
  return (
    <div className="flex min-h-[50vh] flex-col items-center justify-center text-center">
      {children}
    </div>
  );
}

export default function ComparePage() {
  return (
    <Suspense fallback={null}>
      <ComparePageInner />
    </Suspense>
  );
}
