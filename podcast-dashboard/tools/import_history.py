"""Reduce a Spotify extended-history export to aggregates for saved episodes only."""
import argparse
import collections
import json
from pathlib import Path


def play_key(row):
    """Identity of one playback event: timestamp, item URI, and ms played."""
    uri = (row.get('spotify_episode_uri') or row.get('spotify_track_uri')
           or row.get('audiobook_chapter_uri') or row.get('audiobook_uri'))
    return row.get('ts'), uri, row.get('ms_played')


def summarize(directory, saved_ids):
    grouped = collections.defaultdict(list)
    timestamps = []
    seen = set()
    # Top level only: a re-unzipped copy in a subfolder must not double count.
    for path in sorted(Path(directory).glob('Streaming_History_*.json')):
        for row in json.loads(path.read_text()):
            timestamps.append(row['ts'])
            episode_id = (row.get('spotify_episode_uri') or '').removeprefix('spotify:episode:')
            if episode_id not in saved_ids:
                continue
            # The same play can appear in overlapping or re-downloaded exports
            # with differing incidental fields (IP, platform string, offline
            # timestamp), so dedup on the identity of the play itself.
            key = play_key(row)
            if key in seen:
                continue
            seen.add(key)
            grouped[episode_id].append(row)
    if not timestamps:
        raise ValueError('No streaming-history records found')
    episodes = []
    for episode_id, rows in sorted(grouped.items()):
        def reasons(field):
            counts = collections.Counter(r.get(field) or 'unknown' for r in rows)
            return '; '.join(f'{key}: {value}' for key, value in sorted(counts.items()))
        episodes.append(dict(
            id=episode_id, events=len(rows),
            listened_min=round(sum(r['ms_played'] for r in rows) / 60000, 3),
            first_played=min(r['ts'] for r in rows)[:10],
            last_played=max(r['ts'] for r in rows)[:10],
            start_reasons=reasons('reason_start'), end_reasons=reasons('reason_end'),
        ))
    return {'meta': {'source': 'Spotify Extended Streaming History',
                     'coverage_start': min(timestamps)[:10],
                     'coverage_end': max(timestamps)[:10],
                     'matched_episodes': len(episodes)}, 'episodes': episodes}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('export_directory', type=Path)
    args = parser.parse_args()
    app = Path(__file__).resolve().parents[1]
    saved = json.loads((app / 'data/saved_episodes.json').read_text())
    result = summarize(args.export_directory, {r['id'] for r in saved['episodes']})
    (app / 'data/listening_history.json').write_text(json.dumps(result, indent=2) + '\n')
    print(f"Matched {result['meta']['matched_episodes']} saved episodes")
