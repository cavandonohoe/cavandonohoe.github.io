"""Synthetic-fixture tests for the podcast listening-history importer."""
import datetime
import importlib.util
import json
import pathlib
import tempfile
import unittest

TOOL = pathlib.Path(__file__).resolve().parents[1] / 'podcast-dashboard/tools/import_history.py'
spec = importlib.util.spec_from_file_location('import_history', TOOL)
import_history = importlib.util.module_from_spec(spec)
spec.loader.exec_module(import_history)
LIBRARY_TOOL = TOOL.with_name('import_library.py')
spec = importlib.util.spec_from_file_location('import_library', LIBRARY_TOOL)
import_library = importlib.util.module_from_spec(spec)
spec.loader.exec_module(import_library)


def play(ts, episode, ms, **extra):
    return {'ts': ts, 'spotify_episode_uri': f'spotify:episode:{episode}', 'ms_played': ms,
            'reason_start': 'clickrow', 'reason_end': 'endplay', **extra}


class ImportHistoryTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = pathlib.Path(self.temp.name)

    def write(self, name, rows):
        (self.root / name).write_text(json.dumps(rows))

    def test_same_play_with_different_incidental_fields_counts_once(self):
        self.write('Streaming_History_Audio_2024.json', [
            play('2024-01-01T10:00:00Z', 'a', 60000, ip_addr='x', platform='ios'),
            play('2024-01-02T10:00:00Z', 'a', 30000),
        ])
        # Overlapping export: same play, different IP/platform fields.
        self.write('Streaming_History_Audio_2024_1.json', [
            play('2024-01-01T10:00:00Z', 'a', 60000, ip_addr='y', platform='iOS 17'),
        ])
        result = import_history.summarize(self.root, {'a'})
        (row,) = result['episodes']
        self.assertEqual(row['events'], 2)
        self.assertAlmostEqual(row['listened_min'], 1.5)

    def test_distinct_plays_at_same_time_are_kept(self):
        self.write('Streaming_History_Audio_2024.json', [
            play('2024-01-01T10:00:00Z', 'a', 60000),
            play('2024-01-01T10:00:00Z', 'a', 1000),
            play('2024-01-01T10:00:00Z', 'b', 60000),
        ])
        result = import_history.summarize(self.root, {'a', 'b'})
        self.assertEqual({r['id']: r['events'] for r in result['episodes']}, {'a': 2, 'b': 1})

    def test_reunzipped_copy_in_subfolder_is_ignored(self):
        rows = [play('2024-01-01T10:00:00Z', 'a', 60000)]
        self.write('Streaming_History_Audio_2024.json', rows)
        (self.root / 'copy').mkdir()
        (self.root / 'copy/Streaming_History_Audio_2024.json').write_text(json.dumps(rows))
        result = import_history.summarize(self.root, {'a'})
        self.assertEqual(result['episodes'][0]['events'], 1)


class ImportLibraryTests(unittest.TestCase):
    def test_export_count_is_recorded_for_api_split(self):
        library = {'episodes': [
            {'uri': 'spotify:episode:a', 'name': 'A', 'show': 'S'},
            {'uri': 'spotify:episode:a', 'name': 'A', 'show': 'S'},
            {'uri': 'spotify:episode:b', 'name': 'B', 'show': 'S'},
        ]}
        previous = {'episodes': [{'id': 'a', 'dur_min': 30}, {'id': 'gone'}]}
        result = import_library.merge_library(library, previous)
        self.assertEqual(result['meta']['export_n_episodes'], 2)
        self.assertEqual(result['meta']['n_episodes'], 2)
        self.assertEqual(result['meta']['previous_only_count'], 1)
        self.assertEqual(result['episodes'][0]['dur_min'], 30)

    def test_import_clears_api_removal_markers(self):
        library = {'episodes': [{'uri': 'spotify:episode:a', 'name': 'A', 'show': 'S'}]}
        previous = {'episodes': [{'id': 'a', 'api_seen': True, 'api_misses': 1,
                                  'removed_at': '2026-10-22'}]}
        row = import_library.merge_library(library, previous)['episodes'][0]
        self.assertNotIn('removed_at', row)
        self.assertNotIn('api_misses', row)
        self.assertTrue(row['api_seen'])



SET_CUTOFF = pathlib.Path(__file__).resolve().parents[1] / 'podcast-dashboard/tools/set_cutoff.py'
cutoff_spec = importlib.util.spec_from_file_location('set_cutoff', SET_CUTOFF)
set_cutoff = importlib.util.module_from_spec(cutoff_spec)
cutoff_spec.loader.exec_module(set_cutoff)


class SetCutoffTests(unittest.TestCase):
    EPISODES = [
        {'id': 'a', 'show': 'Planet Money', 'name': 'The Boundary', 'release': '2026-03-14'},
        {'id': 'b', 'show': 'Planet Money', 'name': 'Twin', 'release': '2026-01-01'},
        {'id': 'c', 'show': 'Planet Money', 'name': 'Twin', 'release': '2026-02-01'},
        {'id': 'd', 'show': 'Unexplainable', 'name': 'Undated', 'release': None},
    ]

    def test_match_ignores_case_and_spacing(self):
        ep = set_cutoff.find_boundary(self.EPISODES, 'planet  money', 'the boundary',
                                      '2026-03-14')
        self.assertEqual(ep['id'], 'a')

    def test_release_mismatch_is_rejected(self):
        with self.assertRaisesRegex(ValueError, 'released 2026-03-14'):
            set_cutoff.find_boundary(self.EPISODES, 'Planet Money', 'The Boundary',
                                     '2026-03-15')

    def test_missing_and_ambiguous_titles_are_rejected(self):
        with self.assertRaisesRegex(ValueError, 'No saved episode'):
            set_cutoff.find_boundary(self.EPISODES, 'Planet Money', 'Nope', '2026-03-14')
        with self.assertRaisesRegex(ValueError, '2 saved episodes'):
            set_cutoff.find_boundary(self.EPISODES, 'Planet Money', 'Twin', '2026-01-01')

    def test_undated_boundary_uses_the_given_date(self):
        ep = set_cutoff.find_boundary(self.EPISODES, 'Unexplainable', 'Undated', '2026-05-05')
        config = set_cutoff.build_cutoff(ep, '2026-05-05', today=datetime.date(2026, 10, 23))
        self.assertEqual(config['cutoff'], '2026-05-05')
        self.assertEqual(config['boundary']['id'], 'd')
        self.assertEqual(config['confirmed_at'], '2026-10-23')


if __name__ == '__main__':
    unittest.main()
