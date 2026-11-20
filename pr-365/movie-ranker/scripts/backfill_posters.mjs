#!/usr/bin/env node
// Back-fill `titles.image_url` with TMDB poster URLs.
//
// For every title whose image_url is still null, look the IMDb id (tconst) up
// via TMDB's /find endpoint and store the poster (or, for TV episodes, the
// episode still). Titles TMDB doesn't know stay null and are retried on the
// next run, so this is safe to run repeatedly.
//
// Zero dependencies (Node 22 fetch). Env:
//   SUPABASE_URL (or NEXT_PUBLIC_SUPABASE_URL)
//   SUPABASE_SERVICE_ROLE_KEY   service role: bypasses RLS to write titles
//   TMDB_API_KEY                TMDB v3 API key
//
// Usage:
//   node scripts/backfill_posters.mjs              # write image_url
//   node scripts/backfill_posters.mjs --dry-run    # look up only, no writes
//   node scripts/backfill_posters.mjs --limit 20   # first N missing titles

const SUPABASE_URL = process.env.SUPABASE_URL || process.env.NEXT_PUBLIC_SUPABASE_URL;
const SERVICE_KEY = process.env.SUPABASE_SERVICE_ROLE_KEY;
const TMDB_API_KEY = process.env.TMDB_API_KEY;

const IMAGE_BASE = "https://image.tmdb.org/t/p/w500";
const CONCURRENCY = 8;
const PAGE_SIZE = 1000;

const args = process.argv.slice(2);
const dryRun = args.includes("--dry-run");
const limitIdx = args.indexOf("--limit");
const limit = limitIdx >= 0 ? Number(args[limitIdx + 1]) : Infinity;

const missing = [
  ["SUPABASE_URL", SUPABASE_URL],
  ["SUPABASE_SERVICE_ROLE_KEY", SERVICE_KEY],
  ["TMDB_API_KEY", TMDB_API_KEY]
].filter(([, v]) => !v).map(([k]) => k);
if (missing.length) {
  console.error(`Missing env: ${missing.join(", ")}`);
  process.exit(1);
}

const sbHeaders = {
  apikey: SERVICE_KEY,
  Authorization: `Bearer ${SERVICE_KEY}`,
  "Content-Type": "application/json"
};

const sleep = (ms) => new Promise((r) => setTimeout(r, ms));

async function fetchWithRetry(url, init = {}, attempts = 5) {
  for (let i = 1; ; i++) {
    const res = await fetch(url, init);
    if (res.status !== 429 && res.status < 500) return res;
    if (i >= attempts) return res;
    const retryAfter = Number(res.headers.get("retry-after")) || 2 ** i;
    await sleep(retryAfter * 1000);
  }
}

async function loadMissingTitles() {
  const rows = [];
  for (let from = 0; ; from += PAGE_SIZE) {
    const url =
      `${SUPABASE_URL}/rest/v1/titles?select=id,tconst,title` +
      "&image_url=is.null&order=tconst";
    const res = await fetchWithRetry(url, {
      headers: { ...sbHeaders, Range: `${from}-${from + PAGE_SIZE - 1}` }
    });
    if (!res.ok) {
      throw new Error(`Supabase read failed: HTTP ${res.status} ${await res.text()}`);
    }
    const page = await res.json();
    rows.push(...page);
    if (page.length < PAGE_SIZE) return rows;
  }
}

// Returns a full image URL, null if TMDB has no art, or throws on auth errors.
async function lookupPoster(tconst) {
  const url =
    `https://api.themoviedb.org/3/find/${encodeURIComponent(tconst)}` +
    `?external_source=imdb_id&api_key=${encodeURIComponent(TMDB_API_KEY)}`;
  const res = await fetchWithRetry(url);
  if (res.status === 401) throw new Error("TMDB rejected the API key (HTTP 401).");
  if (!res.ok) return null;
  const body = await res.json();
  const path =
    body.movie_results?.[0]?.poster_path ||
    body.tv_results?.[0]?.poster_path ||
    body.tv_season_results?.[0]?.poster_path ||
    body.tv_episode_results?.[0]?.still_path ||
    null;
  return path ? `${IMAGE_BASE}${path}` : null;
}

async function writeImageUrl(id, imageUrl) {
  const res = await fetchWithRetry(
    `${SUPABASE_URL}/rest/v1/titles?id=eq.${encodeURIComponent(id)}`,
    {
      method: "PATCH",
      headers: { ...sbHeaders, Prefer: "return=minimal" },
      body: JSON.stringify({ image_url: imageUrl })
    }
  );
  if (!res.ok) {
    throw new Error(`Supabase write failed for ${id}: HTTP ${res.status} ${await res.text()}`);
  }
}

async function main() {
  const titles = (await loadMissingTitles()).slice(0, limit);
  console.log(`${titles.length} title(s) missing image_url${dryRun ? " (dry run)" : ""}.`);

  let found = 0;
  let notFound = 0;
  let next = 0;
  const unmatched = [];

  async function worker() {
    while (next < titles.length) {
      const t = titles[next++];
      const imageUrl = await lookupPoster(t.tconst);
      if (!imageUrl) {
        notFound++;
        unmatched.push(`${t.tconst} ${t.title}`);
        continue;
      }
      if (!dryRun) await writeImageUrl(t.id, imageUrl);
      found++;
    }
  }

  await Promise.all(Array.from({ length: CONCURRENCY }, worker));

  console.log(`${dryRun ? "Would update" : "Updated"} ${found}; no TMDB art for ${notFound}.`);
  if (unmatched.length) {
    console.log("No art found for:");
    for (const line of unmatched.slice(0, 50)) console.log(`  ${line}`);
    if (unmatched.length > 50) console.log(`  ...and ${unmatched.length - 50} more`);
  }
}

main().catch((err) => {
  console.error(err.message || err);
  process.exit(1);
});
