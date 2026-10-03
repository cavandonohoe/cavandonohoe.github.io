"""Reproducible IMDb exceptional-episode analysis; stdlib only.
Run after download.py; official inputs are kept outside version control.
"""
import csv, gzip, json, pathlib, collections, re, hashlib, datetime, argparse
ROOT = pathlib.Path(__file__).resolve().parent
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--data-dir', type=pathlib.Path, default=ROOT / 'inputs')
parser.add_argument('--output-dir', type=pathlib.Path, default=ROOT / 'results')
parser.add_argument('--repo-dir', type=pathlib.Path, default=ROOT.parents[1])
args = parser.parse_args()
REPO = args.repo_dir
DATA = args.data_dir
OUT = args.output_dir
OUT.mkdir(parents=True, exist_ok=True)

def rows(name):
    with gzip.open(DATA / (name + '.tsv.gz'), 'rt', encoding='utf-8') as f:
        yield from csv.DictReader(f, delimiter='\t', quoting=csv.QUOTE_NONE)

def write(name, rs):
    rs = list(rs)
    if not rs:
        return
    with (OUT / name).open('w', newline='') as f:
        w = csv.DictWriter(f, fieldnames=list(rs[0]))
        w.writeheader()
        w.writerows(rs)
ratings = {r['tconst']: (float(r['averageRating']), int(r['numVotes'])) for r in rows('title.ratings')}
print('Ratings loaded', len(ratings), flush=True)
saved = list(csv.DictReader((REPO / 'data/top250_imdb_series.csv').open()))
saved_ids = {r['imdb_id'] for r in saved}
series = {}
epnames = {}
for r in rows('title.basics'):
    tid = r['tconst']
    if r['titleType'] in ('tvSeries', 'tvMiniSeries'):
        series[tid] = {'series_id': tid, 'series_title': r['primaryTitle'], 'title_type': r['titleType'], 'start_year': r['startYear'], 'genres': r['genres'], 'series_rating': ratings.get(tid, (None, None))[0], 'series_votes': ratings.get(tid, (None, None))[1]}
    elif r['titleType'] == 'tvEpisode' and ratings.get(tid, (0, 0))[1] >= 1000:
        epnames[tid] = (r['primaryTitle'], r['startYear'])
eligible = {k for k, v in series.items() if (v['series_votes'] or 0) >= 25000}
print('Eligible series', len(eligible), flush=True)
source = (REPO / 'web_scraping/imdb_season_episode_ratings_plot.R').read_text()
mapping = {slug: tid for tid, slug in re.findall('"(tt\\d+)"\\s*,\\s*"([a-z0-9_]+)"', source)}
mapping['star_wars_visions'] = 'tt13622982'
tracked = eligible | saved_ids | set(mapping.values())
episodes = []
bykey = collections.defaultdict(list)
coverage = collections.Counter()
rated = collections.Counter()
for r in rows('title.episode'):
    p = r['parentTconst']
    t = r['tconst']
    rat = ratings.get(t)
    if p not in tracked:
        continue
    coverage[p] += 1
    if rat:
        rated[p] += 1
    bykey[p, r['seasonNumber'], r['episodeNumber']].append((t, rat))
    if rat and rat[0] >= 9.7 and (rat[1] >= 1000):
        episodes.append({'series_id': p, 'series_title': series.get(p, {}).get('series_title', p), 'episode_id': t, 'season': r['seasonNumber'], 'episode': r['episodeNumber'], 'episode_title': epnames.get(t, ('', None))[0], 'episode_year': epnames.get(t, ('', None))[1], 'rating': rat[0], 'votes': rat[1], 'eligible_series': p in eligible, 'saved_cohort': p in saved_ids})
assert len({e['episode_id'] for e in episodes}) == len(episodes)
counts = {n: collections.Counter((e['series_id'] for e in episodes if e['votes'] >= n)) for n in (1000, 5000, 10000)}
summary = []
for label, ids in [('broader_eligible', eligible), ('saved_cohort_all', saved_ids), ('saved_cohort_eligible', saved_ids & eligible), ('outside_saved_cohort', eligible - saved_ids)]:
    for n in counts:
        show_count = sum((counts[n][i] > 0 for i in ids))
        summary.append({'cohort': label, 'episode_vote_threshold': n, 'cohort_shows': len(ids), 'shows_with_exceptional_episodes': show_count, 'percent_shows': round(100 * show_count / len(ids), 2) if ids else None, 'exceptional_episodes': sum((counts[n][i] for i in ids))})
write('cohort_summary.csv', summary)
write('primary_exceptional_episodes.csv', sorted((e for e in episodes if e['eligible_series'] and e['votes'] >= 5000), key=lambda e: (e['series_title'], e['season'], e['episode'])))
write('exceptional_episodes.csv', sorted(episodes, key=lambda e: (-e['votes'], e['series_title'])))
leader = []
for i in eligible:
    leader.append({**series[i], 'saved_cohort': i in saved_ids, 'linked_episodes': coverage[i], 'rated_episodes': rated[i], **{'exceptional_' + str(n): counts[n][i] for n in counts}})
leader.sort(key=lambda r: (-r['exceptional_5000'], -r['exceptional_10000'], -r['series_votes'], r['series_id']))
write('eligible_series.csv', leader)
validation = []
detail = []
files = [('top250_imdb_series.csv', saved, None)]
for path in sorted((REPO / 'data').glob('*_ep_ratings.csv')):
    files.append((path.name, list(csv.DictReader(path.open())), mapping.get(path.name.removesuffix('_ep_ratings.csv'))))
for filename, rs, parent in files:
    stats = collections.Counter()
    for r in rs:
        p = r.get('imdb_id', parent)
        key = (p, r.get('season'), r.get('episode'))
        matches = bykey.get(key, [])
        stats['rows'] += 1
        if p is None:
            stats['unmapped_series'] += 1
            continue
        if len(matches) != 1:
            stats['missing_or_ambiguous_key'] += 1
            continue
        t, rat = matches[0]
        if rat is None:
            stats['no_official_rating'] += 1
            continue
        stats['matched'] += 1
        try:
            old = float(r['rating'])
            ov = int(float(r['votes']))
        except (ValueError, KeyError):
            stats['invalid_csv_rating_or_votes'] += 1
            continue
        stats['same_rating'] += old == rat[0]
        stats['same_votes'] += ov == rat[1]
        stats['primary_classification_changes'] += (old >= 9.7 and ov >= 5000) != (rat[0] >= 9.7 and rat[1] >= 5000)
        detail.append({'file': filename, 'series_id': p, 'episode_id': t, 'season': key[1], 'episode': key[2], 'csv_rating': old, 'official_rating': rat[0], 'csv_votes': ov, 'official_votes': rat[1]})
    validation.append({'file': filename, **{k: stats[k] for k in ['rows', 'matched', 'same_rating', 'same_votes', 'primary_classification_changes', 'unmapped_series', 'missing_or_ambiguous_key', 'no_official_rating', 'invalid_csv_rating_or_votes']}})
write('validation_summary.csv', validation)
write('validation_episode_matches.csv', detail)
manifest = {'analyzed_utc': datetime.datetime.now(datetime.timezone.utc).isoformat(), 'repo_commit': __import__('subprocess').check_output(['git', '-C', str(REPO), 'rev-parse', 'HEAD'], text=True).strip(), 'cohort_note': 'Live Top 250 blocked (403); saved cohort is historical, not verified current.', 'saved_cohort_unique_shows': len(saved_ids), 'eligible_title_types': dict(collections.Counter((series[i]['title_type'] for i in eligible))), 'eligible_without_linked_episodes': len([i for i in eligible if not coverage[i]]), 'eligible_without_rated_episodes': len([i for i in eligible if not rated[i]]), 'datasets': {n: {'sha256': hashlib.sha256((DATA / (n + '.tsv.gz')).read_bytes()).hexdigest(), 'headers': json.loads((DATA / (n + '.headers.json')).read_text())} for n in ['title.basics', 'title.episode', 'title.ratings']}}
(OUT / 'manifest.json').write_text(json.dumps(manifest, indent=2))
print(json.dumps(summary, indent=2))
print('Top shows', json.dumps(leader[:15], indent=2))
print('Validation', json.dumps(validation, indent=2))
