"""Synthetic-fixture tests for the podcast listening-history importer."""
import importlib.util
import json
import pathlib
import tempfile
import unittest
import zipfile

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
    AS_OF = '2026-10-25'

    def export(self, *ids):
        return {'episodes': [{'uri': f'spotify:episode:{i}', 'name': i.upper(), 'show': 'S'}
                             for i in ids]}

    def test_export_count_is_recorded_for_api_split(self):
        library = self.export('a', 'a', 'b')
        previous = {'episodes': [{'id': 'a', 'dur_min': 30}, {'id': 'gone'}]}
        result = import_library.merge_library(library, previous, self.AS_OF)
        self.assertEqual(result['meta']['export_n_episodes'], 2)
        self.assertEqual(result['meta']['n_episodes'], 2)
        self.assertEqual(result['meta']['previous_only_count'], 1)
        self.assertEqual(result['meta']['export_as_of'], self.AS_OF)
        self.assertEqual(result['episodes'][0]['dur_min'], 30)

    def test_removal_before_snapshot_is_cleared(self):
        previous = {'episodes': [{'id': 'a', 'api_seen': True, 'api_misses': 1,
                                  'removed_at': '2026-10-22'}]}
        row = import_library.merge_library(self.export('a'), previous, self.AS_OF)['episodes'][0]
        self.assertNotIn('removed_at', row)
        self.assertNotIn('api_misses', row)
        self.assertTrue(row['api_seen'])

    def test_removal_after_snapshot_stays_hidden(self):
        previous = {'episodes': [{'id': 'a', 'api_seen': True, 'removed_at': '2026-10-27'}]}
        result = import_library.merge_library(self.export('a'), previous, self.AS_OF)
        self.assertEqual(result['episodes'][0]['removed_at'], '2026-10-27')
        self.assertEqual(result['meta']['n_removed'], 1)
        self.assertEqual(result['meta']['kept_removed_after_export'], 1)

    def test_save_after_snapshot_is_kept(self):
        previous = {'episodes': [
            {'id': 'new', 'name': 'New', 'added_at': '2026-10-26'},
            {'id': 'same_day', 'name': 'Same', 'added_at': '2026-10-25'},
            {'id': 'old', 'name': 'Old', 'added_at': '2026-10-01'},
            {'id': 'undated', 'name': 'Undated', 'added_at': None},
        ]}
        result = import_library.merge_library(self.export('a'), previous, self.AS_OF)
        ids = [r['id'] for r in result['episodes']]
        self.assertEqual(ids, ['a', 'new'])
        self.assertEqual(result['meta']['export_n_episodes'], 1)
        self.assertEqual(result['meta']['kept_after_export'], 1)
        self.assertEqual(result['meta']['previous_only_count'], 3)

    def test_snapshot_date_reads_zip_timestamp(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = pathlib.Path(tmp) / 'export.zip'
            with zipfile.ZipFile(path, 'w') as archive:
                info = zipfile.ZipInfo(import_library.LIBRARY_PATH, (2026, 10, 25, 9, 0, 0))
                archive.writestr(info, '{"episodes": []}')
            with zipfile.ZipFile(path) as archive:
                self.assertEqual(import_library.snapshot_date(archive), '2026-10-25')

if __name__ == '__main__':
    unittest.main()
