-- movie-ranker schema
-- Run in the Supabase SQL Editor against a clean project.
--
-- Each row in `titles` is a movie / TV series / etc., imported from your
-- IMDb ratings export. The headline question is: "within everything I rated
-- 8/10, what's the actual order?" — so the catalog is closed (you don't add
-- titles in the app, you re-import the CSV) and ratings are anchored to
-- `your_rating` so an 8 always starts above a 7.

create extension if not exists "pgcrypto";

-- ---------- titles ----------
create table if not exists public.titles (
  id            uuid        primary key default gen_random_uuid(),
  tconst        text        not null unique,                 -- imdb const, e.g. tt0816692
  title         text        not null,
  original_title text,
  title_type    text,                                        -- Movie / TV Series / TV Mini Series / ...
  kind          text        not null,                        -- normalized: movie, series, mini_series, short, video_game, tv_movie, tv_special, video, tv_episode, tv_short
  year          integer,
  runtime_min   integer,
  genres        text[]      not null default '{}',
  primary_genre text,
  directors     text[]      not null default '{}',
  imdb_url      text,
  imdb_rating   numeric(3,1),
  num_votes     integer,
  release_date  date,
  date_rated    date,
  your_rating   integer     not null check (your_rating between 1 and 10),
  image_url     text,                                        -- poster, optional; left null for now
  rating        integer     not null default 1200,           -- global Elo
  wins          integer     not null default 0,
  losses        integer     not null default 0,
  created_at    timestamptz not null default now()
);

create index if not exists titles_rating_idx       on public.titles (rating desc);
create index if not exists titles_your_rating_idx  on public.titles (your_rating desc);
create index if not exists titles_kind_idx         on public.titles (kind);
create index if not exists titles_kind_yr_idx      on public.titles (kind, your_rating desc);
create index if not exists titles_primary_genre_idx on public.titles (primary_genre);

-- ---------- comparisons ----------
create table if not exists public.comparisons (
  id                    uuid        primary key default gen_random_uuid(),
  user_id               uuid        not null references auth.users(id) on delete cascade,
  winner_title_id       uuid        not null references public.titles(id) on delete cascade,
  loser_title_id        uuid        not null references public.titles(id) on delete cascade,
  winner_rating_before  integer     not null,
  loser_rating_before   integer     not null,
  winner_rating_after   integer     not null,
  loser_rating_after    integer     not null,
  created_at            timestamptz not null default now(),
  check (winner_title_id <> loser_title_id)
);

create index if not exists comparisons_user_id_idx
  on public.comparisons (user_id, created_at desc);
create index if not exists comparisons_winner_idx on public.comparisons (winner_title_id);
create index if not exists comparisons_loser_idx  on public.comparisons (loser_title_id);

-- ---------- RLS ----------
alter table public.titles      enable row level security;
alter table public.comparisons enable row level security;

drop policy if exists "titles_read_all" on public.titles;
create policy "titles_read_all"
  on public.titles for select
  using (true);

-- Global Elo updates flow through Server Actions running with the user's
-- session. Only the owner account may move the shared global Elo, so a
-- stranger who signs up cannot rewrite the global leaderboard. Their own
-- personal comparisons/overrides are still isolated by the policies below.
-- Inserts/deletes are managed offline via the seed/import script with the
-- service role key. Replace the uuid with your own auth.users id.
drop policy if exists "titles_update_authed" on public.titles;
drop policy if exists "titles_update_owner" on public.titles;
create policy "titles_update_owner"
  on public.titles for update
  to authenticated
  using (auth.uid() = '48bd53ba-ad7b-4291-82c5-13a34b989b52'::uuid)
  with check (auth.uid() = '48bd53ba-ad7b-4291-82c5-13a34b989b52'::uuid);

drop policy if exists "comparisons_read_own" on public.comparisons;
create policy "comparisons_read_own"
  on public.comparisons for select
  using (auth.uid() = user_id);

drop policy if exists "comparisons_insert_own" on public.comparisons;
create policy "comparisons_insert_own"
  on public.comparisons for insert
  with check (auth.uid() = user_id);

-- ---------- manual_overrides ----------
-- When a user drags a title into a specific spot on the leaderboard, we
-- record an override here so the move sticks across reloads. We always
-- keep just the latest override per (user, title) so re-dragging the
-- same title is idempotent. Personal stats apply overrides AFTER the
-- comparison replay, snapping the title to the recorded rating.
create table if not exists public.manual_overrides (
  user_id    uuid        not null references auth.users(id) on delete cascade,
  title_id   uuid        not null references public.titles(id) on delete cascade,
  rating     integer     not null,
  created_at timestamptz not null default now(),
  primary key (user_id, title_id)
);

create index if not exists manual_overrides_user_idx
  on public.manual_overrides (user_id, created_at desc);

alter table public.manual_overrides enable row level security;

drop policy if exists "manual_overrides_read_own" on public.manual_overrides;
create policy "manual_overrides_read_own"
  on public.manual_overrides for select
  using (auth.uid() = user_id);

drop policy if exists "manual_overrides_upsert_own" on public.manual_overrides;
create policy "manual_overrides_upsert_own"
  on public.manual_overrides for insert
  with check (auth.uid() = user_id);

drop policy if exists "manual_overrides_update_own" on public.manual_overrides;
create policy "manual_overrides_update_own"
  on public.manual_overrides for update
  using (auth.uid() = user_id)
  with check (auth.uid() = user_id);

drop policy if exists "manual_overrides_delete_own" on public.manual_overrides;
create policy "manual_overrides_delete_own"
  on public.manual_overrides for delete
  using (auth.uid() = user_id);
