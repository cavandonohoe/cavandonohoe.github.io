import Link from "next/link";
import { LeaderboardClient } from "@/components/LeaderboardClient";
import { createClient } from "@/lib/supabase/server";
import type { Comparison, ManualOverride, Title } from "@/lib/database.types";
import {
  COMPARE_KIND_LABEL,
  COMPARE_KINDS,
  type CompareKind,
  compareKindOf
} from "@/lib/kinds";
import { applyPersonalStats, derivePersonalStats } from "@/lib/personal_stats";

type View = "personal" | "global";
type Group = "bucket" | "flat";

type SearchParams = {
  kind?: string;
  view?: View;
  group?: Group;
  bucket?: string;
};

export const dynamic = "force-dynamic";

function isCompareKind(v: string | undefined): v is CompareKind {
  return v != null && (COMPARE_KINDS as string[]).includes(v);
}

export default async function LeaderboardPage({
  searchParams
}: {
  searchParams: SearchParams;
}) {
  const view: View = searchParams.view === "global" ? "global" : "personal";
  const group: Group = searchParams.group === "flat" ? "flat" : "bucket";
  const kind: CompareKind = isCompareKind(searchParams.kind)
    ? (searchParams.kind as CompareKind)
    : "movie";
  const bucketFilter = searchParams.bucket
    ? parseInt(searchParams.bucket, 10)
    : null;

  const supabase = createClient();

  const [{ data: titlesData, error: titlesError }, { data: userData }] =
    await Promise.all([
      supabase.from("titles").select("*").order("rating", { ascending: false }),
      supabase.auth.getUser()
    ]);

  if (titlesError) {
    return (
      <p className="py-20 text-center text-ember">
        Failed to load leaderboard: {titlesError.message}
      </p>
    );
  }

  const allTitles = (titlesData ?? []) as Title[];
  const user = userData.user;

  let titles: Title[] = allTitles;
  let personalCount = 0;
  let overrides: ManualOverride[] = [];

  if (view === "personal" && user) {
    const [{ data: cmpData, error: cmpErr }, { data: ovData, error: ovErr }] =
      await Promise.all([
        supabase
          .from("comparisons")
          .select("*")
          .eq("user_id", user.id)
          .order("created_at", { ascending: true }),
        supabase
          .from("manual_overrides")
          .select("*")
          .eq("user_id", user.id)
      ]);
    if (cmpErr) {
      return (
        <p className="py-20 text-center text-ember">
          Failed to load your comparisons: {cmpErr.message}
        </p>
      );
    }
    if (ovErr) {
      return (
        <p className="py-20 text-center text-ember">
          Failed to load your overrides: {ovErr.message}
        </p>
      );
    }
    const comparisons = (cmpData ?? []) as Comparison[];
    overrides = (ovData ?? []) as ManualOverride[];
    personalCount = comparisons.length;
    const stats = derivePersonalStats(comparisons, allTitles, overrides);
    titles = applyPersonalStats(allTitles, stats);
  }

  const kindFiltered = titles.filter((t) => compareKindOf(t) === kind);
  const filtered =
    bucketFilter != null
      ? kindFiltered.filter((t) => t.your_rating === bucketFilter)
      : kindFiltered;

  const buckets = Array.from(
    new Set(kindFiltered.map((t) => t.your_rating))
  ).sort((a, b) => b - a);

  const draggable = view === "personal" && !!user;
  const visibleIds = new Set(filtered.map((t) => t.id));
  const overridesForView = overrides.filter((o) => visibleIds.has(o.title_id));

  return (
    <section>
      <div className="flex flex-col gap-4 pb-6 sm:flex-row sm:items-end sm:justify-between">
        <div>
          <h1 className="font-display text-4xl font-semibold tracking-tight sm:text-5xl">
            Leaderboard
          </h1>
          <p className="mt-2 text-mist">
            {view === "personal"
              ? user
                ? personalCount === 0 && overrides.length === 0
                  ? "Drag rows to reorder, or jump to compare. Until then, every title sits at its star-bucket anchor."
                  : `Your rankings — ${personalCount} comparison${personalCount === 1 ? "" : "s"}${overrides.length ? `, ${overrides.length} manual placement${overrides.length === 1 ? "" : "s"}` : ""}.`
                : "Sign in to see your personal rankings."
              : "Global Elo, shared across everyone signed in."}
          </p>
        </div>
        <Link
          href={`/compare?kind=${kind}${view === "global" ? "&scope=global" : ""}`}
          className="self-start rounded-full bg-gold px-5 py-2.5 text-sm font-semibold text-cream transition hover:bg-ember hover:text-ink sm:self-auto"
        >
          Compare {COMPARE_KIND_LABEL[kind].toLowerCase()} →
        </Link>
      </div>

      <ViewTabs view={view} kind={kind} group={group} bucket={bucketFilter} />

      {view === "personal" && !user && (
        <div className="my-10 rounded-2xl border border-white/5 bg-slate p-6 text-center">
          <p className="font-display text-xl font-semibold">
            Sign in to start your list
          </p>
          <p className="mt-2 text-sm text-mist">
            Magic link, no password. Your ranks stay yours.
          </p>
          <Link
            href={`/login?next=${encodeURIComponent("/leaderboard")}`}
            className="mt-4 inline-flex rounded-full bg-gold px-5 py-2.5 text-sm font-semibold text-cream transition hover:bg-ember hover:text-ink"
          >
            Sign in →
          </Link>
        </div>
      )}

      <KindRow current={kind} view={view} group={group} bucket={bucketFilter} />

      <div className="mt-4 mb-6 flex flex-wrap items-center gap-2">
        <GroupTabs view={view} kind={kind} group={group} bucket={bucketFilter} />
        {bucketFilter != null && (
          <Link
            href={qs("/leaderboard", { kind, view, group, bucket: null })}
            className="rounded-full border border-white/10 bg-slate px-3 py-1.5 text-xs text-mist hover:border-white/30"
          >
            All buckets
          </Link>
        )}
      </div>

      {bucketFilter != null && (
        <div className="mb-4 text-xs uppercase tracking-[0.18em] text-gold">
          Filtering: {bucketFilter}/10 only
        </div>
      )}

      {filtered.length === 0 ? (
        <p className="py-20 text-center text-mist">
          Nothing to show with these filters.
        </p>
      ) : group === "bucket" && bucketFilter == null ? (
        <BucketGroups
          titles={filtered}
          buckets={buckets}
          kind={kind}
          view={view}
          draggable={draggable}
          userId={user?.id ?? null}
          overrides={overridesForView}
        />
      ) : (
        <LeaderboardClient
          initialTitles={filtered}
          initialOverrides={overridesForView}
          draggable={draggable}
          userId={user?.id ?? null}
        />
      )}
    </section>
  );
}

function BucketGroups({
  titles,
  buckets,
  kind,
  view,
  draggable,
  userId,
  overrides
}: {
  titles: Title[];
  buckets: number[];
  kind: CompareKind;
  view: View;
  draggable: boolean;
  userId: string | null;
  overrides: ManualOverride[];
}) {
  const byBucket = new Map<number, Title[]>();
  for (const t of titles) {
    const arr = byBucket.get(t.your_rating) ?? [];
    arr.push(t);
    byBucket.set(t.your_rating, arr);
  }
  return (
    <div className="space-y-12">
      {buckets.map((b) => {
        const items = (byBucket.get(b) ?? []).slice();
        if (items.length === 0) return null;
        const top = items.slice(0, 10);
        const ids = new Set(top.map((t) => t.id));
        const ovForBucket = overrides.filter((o) => ids.has(o.title_id));
        return (
          <div key={b}>
            <div className="mb-3 flex items-end justify-between">
              <h2 className="font-display text-2xl font-semibold tracking-tight">
                <span className="text-gold">{b}</span>
                <span className="text-mist"> / 10</span>
                <span className="ml-3 text-sm font-normal text-mist">
                  {items.length} title{items.length === 1 ? "" : "s"}
                </span>
              </h2>
              <Link
                href={qs("/leaderboard", {
                  kind,
                  view,
                  group: "flat",
                  bucket: String(b)
                })}
                className="text-xs uppercase tracking-[0.18em] text-mist hover:text-ink"
              >
                See all in {b}/10 →
              </Link>
            </div>
            <LeaderboardClient
              initialTitles={top}
              initialOverrides={ovForBucket}
              draggable={draggable}
              userId={userId}
            />
            {items.length > top.length && (
              <Link
                href={qs("/leaderboard", {
                  kind,
                  view,
                  group: "flat",
                  bucket: String(b)
                })}
                className="mt-3 inline-block text-xs text-mist hover:text-ink"
              >
                {items.length - top.length} more in this bucket →
              </Link>
            )}
          </div>
        );
      })}
    </div>
  );
}

function ViewTabs({
  view,
  kind,
  group,
  bucket
}: {
  view: View;
  kind: CompareKind;
  group: Group;
  bucket: number | null;
}) {
  const tab = (active: boolean) =>
    [
      "rounded-full px-4 py-1.5 text-sm font-medium transition",
      active
        ? "bg-ink text-cream"
        : "text-ink/70 hover:bg-white/5 hover:text-ink"
    ].join(" ");
  return (
    <div className="mb-6 inline-flex rounded-full border border-white/10 bg-slate p-1">
      <Link
        href={qs("/leaderboard", { kind, view: "personal", group, bucket })}
        className={tab(view === "personal")}
      >
        Your rankings
      </Link>
      <Link
        href={qs("/leaderboard", { kind, view: "global", group, bucket })}
        className={tab(view === "global")}
      >
        Global popularity
      </Link>
    </div>
  );
}

function KindRow({
  current,
  view,
  group,
  bucket
}: {
  current: CompareKind;
  view: View;
  group: Group;
  bucket: number | null;
}) {
  return (
    <div className="mb-2 inline-flex flex-wrap gap-1 rounded-full border border-white/10 bg-slate p-1">
      {COMPARE_KINDS.map((k) => {
        const active = k === current;
        return (
          <Link
            key={k}
            href={qs("/leaderboard", { kind: k, view, group, bucket: null })}
            className={[
              "rounded-full px-3 py-1.5 text-sm font-medium transition",
              active
                ? "bg-gold text-cream"
                : "text-ink/70 hover:bg-white/5 hover:text-ink"
            ].join(" ")}
          >
            {COMPARE_KIND_LABEL[k]}
          </Link>
        );
      })}
    </div>
  );
}

function GroupTabs({
  view,
  kind,
  group,
  bucket
}: {
  view: View;
  kind: CompareKind;
  group: Group;
  bucket: number | null;
}) {
  const tab = (active: boolean) =>
    [
      "rounded-full px-3 py-1.5 text-xs uppercase tracking-[0.18em] transition",
      active
        ? "bg-ink text-cream"
        : "text-mist hover:bg-white/5 hover:text-ink"
    ].join(" ");
  return (
    <div className="inline-flex rounded-full border border-white/10 bg-slate p-1">
      <Link
        href={qs("/leaderboard", { kind, view, group: "bucket", bucket: null })}
        className={tab(group === "bucket" && bucket == null)}
      >
        By star bucket
      </Link>
      <Link
        href={qs("/leaderboard", { kind, view, group: "flat", bucket })}
        className={tab(group === "flat" || bucket != null)}
      >
        Flat list
      </Link>
    </div>
  );
}

function qs(
  path: string,
  params: {
    kind: CompareKind;
    view: View;
    group: Group;
    bucket: number | string | null;
  }
): string {
  const sp = new URLSearchParams();
  sp.set("kind", params.kind);
  if (params.view !== "personal") sp.set("view", params.view);
  if (params.group !== "bucket") sp.set("group", params.group);
  if (params.bucket != null && params.bucket !== "")
    sp.set("bucket", String(params.bucket));
  const s = sp.toString();
  return s ? `${path}?${s}` : path;
}
