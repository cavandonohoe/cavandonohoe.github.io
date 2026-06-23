#!/usr/bin/env node
// Build supabase/seed.sql from ~/website/data/imdb_ratings.csv.
//
// Each row becomes a `titles` insert. The starting Elo is anchored to your
// IMDb-export rating so a 9/10 is above an 8/10 *before any pairwise voting*.
// Within a star bucket, every title starts at the same anchor — that's the
// whole point: you'll resolve those ties via head-to-head.
//
// Usage:
//   node scripts/build_seed.mjs            # writes supabase/seed.sql
//   node scripts/build_seed.mjs --csv path # custom csv input

import fs from "node:fs";
import path from "node:path";
import { fileURLToPath } from "node:url";

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const ROOT = path.resolve(__dirname, "..");
const DEFAULT_CSV = path.resolve(ROOT, "..", "data", "imdb_ratings.csv");

const args = process.argv.slice(2);
let csvPath = DEFAULT_CSV;
for (let i = 0; i < args.length; i++) {
  if (args[i] === "--csv" && args[i + 1]) {
    csvPath = path.resolve(args[i + 1]);
    i++;
  }
}

if (!fs.existsSync(csvPath)) {
  console.error(`csv not found: ${csvPath}`);
  process.exit(1);
}

// ---------- minimal csv parser (RFC-4180-ish) ----------
function parseCsv(text) {
  const rows = [];
  let cur = [];
  let cell = "";
  let inQuotes = false;
  for (let i = 0; i < text.length; i++) {
    const c = text[i];
    if (inQuotes) {
      if (c === '"') {
        if (text[i + 1] === '"') {
          cell += '"';
          i++;
        } else {
          inQuotes = false;
        }
      } else {
        cell += c;
      }
    } else {
      if (c === '"') {
        inQuotes = true;
      } else if (c === ",") {
        cur.push(cell);
        cell = "";
      } else if (c === "\n") {
        cur.push(cell);
        rows.push(cur);
        cur = [];
        cell = "";
      } else if (c === "\r") {
        // ignore
      } else {
        cell += c;
      }
    }
  }
  if (cell.length > 0 || cur.length > 0) {
    cur.push(cell);
    rows.push(cur);
  }
  return rows;
}

// ---------- normalization ----------
const KIND_BY_TITLE_TYPE = {
  Movie: "movie",
  "TV Series": "series",
  "TV Mini Series": "mini_series",
  "TV Movie": "tv_movie",
  Short: "short",
  "Video Game": "video_game",
  "TV Special": "tv_special",
  Video: "video",
  "TV Episode": "tv_episode",
  "TV Short": "tv_short"
};

function escSql(s) {
  if (s === null || s === undefined || s === "") return "null";
  return "'" + String(s).replace(/'/g, "''") + "'";
}

function escInt(s) {
  if (s === null || s === undefined || s === "") return "null";
  const n = parseInt(String(s), 10);
  return Number.isFinite(n) ? String(n) : "null";
}

function escNum(s) {
  if (s === null || s === undefined || s === "") return "null";
  const n = Number(s);
  return Number.isFinite(n) ? String(n) : "null";
}

function escDate(s) {
  if (!s) return "null";
  // expect YYYY-MM-DD; reject anything else
  return /^\d{4}-\d{2}-\d{2}$/.test(s) ? `'${s}'` : "null";
}

function escTextArr(items) {
  if (!items.length) return "'{}'";
  const inner = items.map((s) => '"' + s.replace(/\\/g, "\\\\").replace(/"/g, '\\"') + '"');
  // Inside the Postgres array literal we have to escape single quotes for the
  // surrounding SQL string by doubling them.
  return "'{" + inner.join(",").replace(/'/g, "''") + "}'";
}

function splitList(raw) {
  if (!raw) return [];
  return raw
    .split(",")
    .map((s) => s.trim())
    .filter((s) => s.length > 0);
}

// Anchor each star bucket so the floor of N is above the ceiling of N-1.
// Range chosen so a perfectly-bottom 1 is comfortably below 1200 and a
// perfectly-top 10 is comfortably above. K=32 means typical comparison
// swings ~16 points, so a 100-point gap between buckets is enough that no
// realistic amount of voting flips an 8 below a 7.
const ANCHOR = (yourRating) => 800 + yourRating * 50; // 1: 850, 5: 1050, 10: 1300

// ---------- read & build ----------
const text = fs.readFileSync(csvPath, "utf8");
const rows = parseCsv(text);
const header = rows.shift();
if (!header) {
  console.error("empty csv");
  process.exit(1);
}
const idx = Object.fromEntries(header.map((h, i) => [h.trim(), i]));
const need = [
  "Const",
  "Your Rating",
  "Title",
  "Title Type",
  "Year",
  "Genres",
  "Directors",
  "URL",
  "IMDb Rating",
  "Num Votes",
  "Date Rated",
  "Release Date",
  "Original Title",
  "Runtime (mins)"
];
for (const k of need) {
  if (!(k in idx)) {
    console.error(`missing column: ${k}`);
    process.exit(1);
  }
}

const seen = new Set();
const inserts = [];
let dropped = 0;
let kept = 0;
const byKind = {};

for (const r of rows) {
  if (!r || r.length === 0 || r.every((c) => !c)) continue;
  const tconst = (r[idx.Const] ?? "").trim();
  const yrStr = (r[idx["Your Rating"]] ?? "").trim();
  if (!tconst || !yrStr) {
    dropped++;
    continue;
  }
  if (seen.has(tconst)) {
    dropped++;
    continue;
  }
  seen.add(tconst);

  const yourRating = parseInt(yrStr, 10);
  if (!Number.isFinite(yourRating) || yourRating < 1 || yourRating > 10) {
    dropped++;
    continue;
  }

  const titleType = (r[idx["Title Type"]] ?? "").trim();
  const kind = KIND_BY_TITLE_TYPE[titleType] ?? "other";

  const genres = splitList(r[idx.Genres] ?? "");
  const directors = splitList(r[idx.Directors] ?? "");
  const primaryGenre = genres[0] ?? null;

  const startingRating = ANCHOR(yourRating);

  const cols = [
    escSql(tconst),
    escSql((r[idx.Title] ?? "").trim()),
    escSql((r[idx["Original Title"]] ?? "").trim() || null),
    escSql(titleType || null),
    escSql(kind),
    escInt(r[idx.Year]),
    escInt(r[idx["Runtime (mins)"]]),
    escTextArr(genres),
    escSql(primaryGenre),
    escTextArr(directors),
    escSql((r[idx.URL] ?? "").trim() || null),
    escNum(r[idx["IMDb Rating"]]),
    escInt(r[idx["Num Votes"]]),
    escDate((r[idx["Release Date"]] ?? "").trim()),
    escDate((r[idx["Date Rated"]] ?? "").trim()),
    String(yourRating),
    String(startingRating)
  ];

  inserts.push(`  (${cols.join(", ")})`);
  kept++;
  byKind[kind] = (byKind[kind] ?? 0) + 1;
}

const headerSql = `-- Generated by scripts/build_seed.mjs from data/imdb_ratings.csv.
-- ${kept} titles, ${dropped} skipped (no rating / duplicate / malformed).
-- Run AFTER schema.sql. Idempotent on re-run via tconst conflict.

insert into public.titles (
  tconst, title, original_title, title_type, kind, year, runtime_min,
  genres, primary_genre, directors, imdb_url, imdb_rating, num_votes,
  release_date, date_rated, your_rating, rating
) values
${inserts.join(",\n")}
on conflict (tconst) do update set
  title         = excluded.title,
  original_title= excluded.original_title,
  title_type    = excluded.title_type,
  kind          = excluded.kind,
  year          = excluded.year,
  runtime_min   = excluded.runtime_min,
  genres        = excluded.genres,
  primary_genre = excluded.primary_genre,
  directors     = excluded.directors,
  imdb_url      = excluded.imdb_url,
  imdb_rating   = excluded.imdb_rating,
  num_votes     = excluded.num_votes,
  release_date  = excluded.release_date,
  date_rated    = excluded.date_rated,
  your_rating   = excluded.your_rating;
-- Note: existing rows keep their global Elo (rating/wins/losses) on re-import;
-- only new rows get the per-bucket anchor.
`;

const out = path.join(ROOT, "supabase", "seed.sql");
fs.writeFileSync(out, headerSql);
console.log(`wrote ${out}`);
console.log(`kept ${kept}, dropped ${dropped}`);
console.log("by kind:", byKind);
