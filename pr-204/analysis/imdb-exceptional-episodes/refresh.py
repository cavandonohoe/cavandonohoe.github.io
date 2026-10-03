"""Refresh a complete snapshot, validate it, then replace the published results."""
import argparse
import collections
import csv
import json
import math
import pathlib
import subprocess
import sys
import tempfile

ROOT = pathlib.Path(__file__).resolve().parent
FILES = (
    'eligible_series.csv', 'exceptional_episodes.csv', 'primary_exceptional_episodes.csv',
    'cohort_summary.csv', 'validation_summary.csv', 'validation_episode_matches.csv',
    'manifest.json',
)


def read_csv(directory, name):
    with (directory / name).open(newline='', encoding='utf-8') as stream:
        return list(csv.DictReader(stream))


def validate(directory, previous=None):
    for name in FILES:
        if not (directory / name).is_file():
            raise ValueError(f'Missing output: {name}')
    series = read_csv(directory, 'eligible_series.csv')
    episodes = read_csv(directory, 'exceptional_episodes.csv')
    primary = read_csv(directory, 'primary_exceptional_episodes.csv')
    if not series or not episodes or not primary:
        raise ValueError('Empty series or exceptional-episode output; refusing publication')
    ids = {s['series_id'] for s in series}
    if len(ids) != len(series):
        raise ValueError('Duplicate series IDs')
    if len({e['episode_id'] for e in episodes}) != len(episodes):
        raise ValueError('Duplicate episode IDs')
    if any(float(e['rating']) < 9.7 or int(e['votes']) < 1000 for e in episodes):
        raise ValueError('Exceptional episodes violate rating/vote cutoffs')
    summaries = read_csv(directory, 'cohort_summary.csv')
    cohorts = {
        'broader_eligible': ids,
        'saved_cohort_eligible': {s['series_id'] for s in series if s['saved_cohort'] == 'True'},
        'outside_saved_cohort': {s['series_id'] for s in series if s['saved_cohort'] == 'False'},
    }
    for cutoff in (1000, 5000, 10000):
        counts = collections.Counter(e['series_id'] for e in episodes if int(e['votes']) >= cutoff)
        for show in series:
            total, hits = int(show['linked_episodes']), counts[show['series_id']]
            if int(show['series_votes']) < 25000 or show['title_type'] not in ('tvSeries', 'tvMiniSeries'):
                raise ValueError('Ineligible series')
            if hits > total or int(show[f'exceptional_{cutoff}']) != hits:
                raise ValueError('Episode counts do not reconcile')
            percent = show[f'percent_exceptional_{cutoff}']
            if total and (not math.isfinite(float(percent)) or abs(float(percent) - 100 * hits / total) > 0.000001):
                raise ValueError('Incorrect all-episode percentage')
        for cohort, members in cohorts.items():
            rows = [r for r in summaries if r['cohort'] == cohort and int(r['episode_vote_threshold']) == cutoff]
            if len(rows) != 1:
                raise ValueError('Missing or duplicate cohort summary')
            row = rows[0]
            if (int(row['cohort_shows']) != len(members)
                    or int(row['exceptional_episodes']) != sum(counts[i] for i in members)
                    or int(row['shows_with_exceptional_episodes']) != sum(counts[i] > 0 for i in members)):
                raise ValueError('Cohort summary does not reconcile')
    expected = {e['episode_id'] for e in episodes if e['series_id'] in ids and int(e['votes']) >= 5000}
    if len(primary) != len(expected) or {e['episode_id'] for e in primary} != expected:
        raise ValueError('Primary episode export does not reconcile')
    manifest = json.loads((directory / 'manifest.json').read_text())
    if not (manifest.get('analyzed_utc') or manifest.get('retrieved_utc')):
        raise ValueError('Snapshot timestamp missing')
    for name in ('title.basics', 'title.episode', 'title.ratings'):
        if len(manifest['datasets'][name]['sha256']) != 64:
            raise ValueError('Input hash missing')
    if previous and (previous / 'eligible_series.csv').exists():
        old_count = len(read_csv(previous, 'eligible_series.csv'))
        if len(series) < old_count * 0.8:
            raise ValueError('Eligible series count fell by more than 20%; review before publishing')
    return len(series), len(primary)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data-dir', type=pathlib.Path, help='Reuse local inputs instead of downloading')
    parser.add_argument('--validate-only', type=pathlib.Path, help='Validate an existing result directory')
    args = parser.parse_args()
    if args.validate_only:
        print('Validated shows / primary episodes:', validate(args.validate_only))
        return
    with tempfile.TemporaryDirectory(prefix='imdb-refresh-') as temporary:
        temp = pathlib.Path(temporary)
        inputs = args.data_dir or temp / 'inputs'
        if not args.data_dir:
            subprocess.run([sys.executable, str(ROOT / 'download.py'), '--data-dir', str(inputs)], check=True)
        results = temp / 'results'
        subprocess.run([sys.executable, str(ROOT / 'analyze.py'), '--data-dir', str(inputs),
                        '--output-dir', str(results)], check=True)
        counts = validate(results, ROOT / 'results')
        # Validation happens before any checked-in result is replaced.
        for name in FILES:
            (ROOT / 'results' / name).write_bytes((results / name).read_bytes())
        print('Published shows / primary episodes:', counts)


if __name__ == '__main__':
    main()
