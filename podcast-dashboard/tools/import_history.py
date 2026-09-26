"""Reduce a Spotify extended-history export to aggregates for saved episodes only."""
import argparse
import collections
import json
from pathlib import Path


def summarize(directory, saved_ids):
    grouped = collections.defaultdict(list)
    timestamps = []
    seen = set()
    for path in sorted(Path(directory).glob('Streaming_History_*.json')):
        for row in json.loads(path.read_text()):
            timestamps.append(row['ts'])
            episode_id = (row.get('spotify_episode_uri') or '').removeprefix('spotify:episode:')
            if episode_id not in saved_ids:
                continue
            # Exact duplicates across export files should not inflate totals.
            key = json.dumps(row, sort_keys=True)
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
