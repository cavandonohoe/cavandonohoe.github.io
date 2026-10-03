"""Regression tests using tiny official-format datasets, without HTTP requests."""
import csv
import gzip
import importlib.util
import json
import pathlib
import shutil
import subprocess
import sys
import tempfile
import unittest
from unittest import mock

PROJECT = pathlib.Path(__file__).resolve().parents[1] / 'analysis/imdb-exceptional-episodes'


def load_module(name):
    spec = importlib.util.spec_from_file_location(name, PROJECT / (name + '.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


refresh = load_module('refresh')
download = load_module('download')


class RefreshTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = pathlib.Path(self.temp.name)
        self.inputs = self.root / 'inputs'
        self.inputs.mkdir()
        self.results = self.root / 'results'
        repo = self.root / 'repo'
        (repo / 'data').mkdir(parents=True)
        (repo / 'web_scraping').mkdir()
        (repo / 'data/top250_imdb_series.csv').write_text(
            'imdb_id,season,episode,rating,votes\na,1,1,9.7,5000\n')
        (repo / 'web_scraping/imdb_season_episode_ratings_plot.R').write_text('')
        subprocess.run(['git', 'init', '-q', str(repo)], check=True)
        subprocess.run(['git', '-C', str(repo), 'add', '.'], check=True)
        subprocess.run(['git', '-C', str(repo), '-c', 'user.name=Test', '-c',
                        'user.email=test@example.com', 'commit', '-qm', 'Fixture'], check=True)
        self.write_input('title.ratings', ['tconst', 'averageRating', 'numVotes'], [
            ['a', 9, 25000], ['b', 8, 50000], ['low', 10, 24999],
            ['e1', 9.7, 5000], ['e2', 9.6, 1000], ['e4', 9.7, 1000],
        ])
        self.write_input('title.basics', ['tconst', 'titleType', 'primaryTitle', 'startYear', 'genres'], [
            ['a', 'tvSeries', 'A', 2020, 'Drama'], ['b', 'tvMiniSeries', 'B', 2021, 'Drama'],
            ['low', 'tvSeries', 'Below cutoff', 2021, 'Drama'],
            *[[e, 'tvEpisode', e, 2020, 'Drama'] for e in ['e1', 'e2', 'e3', 'e4']],
        ])
        self.write_input('title.episode', ['tconst', 'parentTconst', 'seasonNumber', 'episodeNumber'], [
            ['e1', 'a', 1, 1], ['e2', 'a', 1, 2], ['e3', 'a', 0, 1], ['e4', 'b', 1, 1],
        ])
        subprocess.run([sys.executable, str(PROJECT / 'analyze.py'), '--repo-dir', str(repo),
                        '--data-dir', str(self.inputs), '--output-dir', str(self.results)],
                       check=True, stdout=subprocess.DEVNULL)

    def write_input(self, name, fields, rows):
        with gzip.open(self.inputs / (name + '.tsv.gz'), 'wt') as stream:
            writer = csv.writer(stream, delimiter='\t')
            writer.writerow(fields)
            writer.writerows(rows)
        (self.inputs / (name + '.headers.json')).write_text('{}')

    def corrupt_percentage(self, results):
        file = results / 'eligible_series.csv'
        file.write_text(file.read_text().replace('33.333333', '99.000000'))

    def test_complete_analysis_and_all_episode_denominator(self):
        self.assertEqual(refresh.validate(self.results), (2, 1))
        series = refresh.read_csv(self.results, 'eligible_series.csv')
        a = next(s for s in series if s['series_id'] == 'a')
        self.assertEqual(int(a['linked_episodes']), 3)  # Includes an unrated special.
        self.assertAlmostEqual(float(a['percent_exceptional_5000']), 100 / 3, places=5)

    def test_corrupt_percentage_is_rejected(self):
        self.corrupt_percentage(self.results)
        with self.assertRaisesRegex(ValueError, 'percentage'):
            refresh.validate(self.results)

    def test_duplicate_primary_export_is_rejected(self):
        file = self.results / 'primary_exceptional_episodes.csv'
        lines = file.read_text().splitlines()
        file.write_text('\n'.join(lines + [lines[1]]) + '\n')
        with self.assertRaisesRegex(ValueError, 'Primary'):
            refresh.validate(self.results)

    def test_unexpected_series_collapse_is_rejected(self):
        old = self.root / 'previous'
        old.mkdir()
        (old / 'eligible_series.csv').write_text('series_id\n1\n2\n3\n4\n')
        with self.assertRaisesRegex(ValueError, '20%'):
            refresh.validate(self.results, old)

    def test_failed_refresh_does_not_replace_published_files(self):
        published = self.root / 'published'
        shutil.copytree(self.results, published / 'results')
        before = {p.name: p.read_bytes() for p in (published / 'results').iterdir()}

        def analyze(command, **kwargs):
            output = pathlib.Path(command[command.index('--output-dir') + 1])
            shutil.copytree(self.results, output)
            self.corrupt_percentage(output)

        with mock.patch.object(refresh, 'ROOT', published), mock.patch.object(sys, 'argv',
                ['refresh.py', '--data-dir', str(self.inputs)]), mock.patch.object(refresh.subprocess, 'run', analyze):
            with self.assertRaises(ValueError):
                refresh.main()
        self.assertEqual(before, {p.name: p.read_bytes() for p in (published / 'results').iterdir()})

    def test_partial_download_never_replaces_previous_input(self):
        target = self.inputs / 'title.ratings.tsv.gz'
        before = target.read_bytes()
        response = mock.MagicMock()
        response.__enter__.return_value = response
        response.headers = {'Content-Length': '100'}
        response.read.side_effect = [b'short', b''] * 3
        with mock.patch.object(download.urllib.request, 'urlopen', return_value=response), mock.patch.object(download.time, 'sleep'):
            with self.assertRaisesRegex(ValueError, 'Incomplete'):
                download.get('title.ratings', self.inputs)
        self.assertEqual(target.read_bytes(), before)
        self.assertFalse(target.with_suffix('.gz.part').exists())


if __name__ == '__main__':
    unittest.main()
