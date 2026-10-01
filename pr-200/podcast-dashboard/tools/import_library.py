"""Import saved podcast membership from Spotify Account Data, retaining known metadata."""
import argparse
import datetime
import json
from pathlib import Path
import zipfile


def merge_library(library, previous):
    known = {r['id']: r for r in previous['episodes']}
    episodes = []
    seen = set()
    for item in library['episodes']:
        episode_id = item['uri'].removeprefix('spotify:episode:')
        if episode_id in seen:
            continue
        seen.add(episode_id)
        row = dict(known.get(episode_id, {}))
        for field in ('added_at', 'dur_min', 'release', 'publisher', 'desc'):
            row.setdefault(field, None)
        row.update(id=episode_id, name=item['name'], show=item['show'],
                   url=f'https://open.spotify.com/episode/{episode_id}')
        episodes.append(row)
    return {'meta': {'generated_at': datetime.date.today().isoformat(),
                     'source': 'Spotify Account Data / YourLibrary.json',
                     'n_episodes': len(episodes),
                     'previous_only_count': len(set(known) - seen)},
            'episodes': episodes}


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('archive', type=Path)
    args = parser.parse_args()
    destination = Path(__file__).resolve().parents[1] / 'data/saved_episodes.json'
    previous = json.loads(destination.read_text())
    with zipfile.ZipFile(args.archive) as archive:
        library = json.loads(archive.read('Spotify Account Data/YourLibrary.json'))
    result = merge_library(library, previous)
    destination.write_text(json.dumps(result, indent=2, ensure_ascii=False) + '\n')
    print(f"Imported {len(result['episodes'])} saved episodes")
