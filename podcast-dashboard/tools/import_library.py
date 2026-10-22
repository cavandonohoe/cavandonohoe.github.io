"""Import saved podcast membership from Spotify Account Data, retaining known metadata.

The export is the source of truth as of the day Spotify generated it (`as_of`).
API evidence from after that day still wins:

- an episode the daily refresh marked `removed_at` after `as_of` stays hidden,
  even though the export lists it (it was finished or unsaved after the export);
- an episode the API saved after `as_of` (`added_at` > `as_of`) is kept, even
  though the export does not list it.

Everything else follows the export: rows it omits are dropped, and removal
markers from before `as_of` are cleared for rows it lists.
"""
import argparse
import datetime
import json
from pathlib import Path
import zipfile

LIBRARY_PATH = 'Spotify Account Data/YourLibrary.json'


def _date(value):
    """Return the YYYY-MM-DD prefix of an ISO date/datetime string, or None."""
    return value[:10] if isinstance(value, str) and len(value) >= 10 else None


def merge_library(library, previous, as_of):
    """Merge an export's YourLibrary.json into the previous dashboard JSON.

    `as_of` is the export snapshot date as a YYYY-MM-DD string.
    """
    known = {r['id']: r for r in previous['episodes']}
    episodes = []
    seen = set()
    kept_removed = 0
    for item in library['episodes']:
        episode_id = item['uri'].removeprefix('spotify:episode:')
        if episode_id in seen:
            continue
        seen.add(episode_id)
        row = dict(known.get(episode_id, {}))
        removed_at = _date(row.get('removed_at'))
        if removed_at and removed_at > as_of:
            # Dropped out of the API after the snapshot: keep it hidden.
            kept_removed += 1
        else:
            # The export says this episode is saved. `api_seen` is kept so
            # later absences are still tracked.
            row.pop('removed_at', None)
            row.pop('api_misses', None)
        for field in ('added_at', 'dur_min', 'release', 'publisher', 'desc'):
            row.setdefault(field, None)
        row.update(id=episode_id, name=item['name'], show=item['show'],
                   url=f'https://open.spotify.com/episode/{episode_id}')
        episodes.append(row)
    n_export = len(episodes)

    # Saves the API saw after the snapshot are newer than the export.
    after = [r for r in previous['episodes']
             if r['id'] not in seen and (_date(r.get('added_at')) or '') > as_of]
    episodes.extend(dict(r) for r in after)
    dropped = len(set(known) - seen) - len(after)

    # generated_at is the import date; export_as_of is the snapshot date the
    # export describes. export_n_episodes stays fixed until the next export so
    # the dashboard can split export membership from later API additions.
    return {'meta': {'generated_at': datetime.date.today().isoformat(),
                     'export_as_of': as_of,
                     'export_n_episodes': n_export,
                     'source': 'Spotify Account Data / YourLibrary.json',
                     'n_episodes': len(episodes),
                     'n_removed': sum(1 for r in episodes if r.get('removed_at')),
                     'previous_only_count': dropped,
                     'kept_after_export': len(after),
                     'kept_removed_after_export': kept_removed},
            'episodes': episodes}


def snapshot_date(archive):
    """Best-effort export snapshot date: YourLibrary.json's timestamp in the zip."""
    year, month, day = archive.getinfo(LIBRARY_PATH).date_time[:3]
    return datetime.date(year, month, day).isoformat()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('archive', type=Path)
    parser.add_argument('--as-of', type=datetime.date.fromisoformat,
                        help='export snapshot date (YYYY-MM-DD); defaults to the '
                             'YourLibrary.json timestamp inside the zip')
    args = parser.parse_args()
    destination = Path(__file__).resolve().parents[1] / 'data/saved_episodes.json'
    previous = json.loads(destination.read_text())
    with zipfile.ZipFile(args.archive) as archive:
        library = json.loads(archive.read(LIBRARY_PATH))
        as_of = args.as_of.isoformat() if args.as_of else snapshot_date(archive)
    result = merge_library(library, previous, as_of)
    destination.write_text(json.dumps(result, indent=2, ensure_ascii=False) + '\n')
    meta = result['meta']
    shown = meta['n_episodes'] - meta['n_removed']
    print(f"Export as of {as_of}{'' if args.as_of else ' (from zip timestamp)'}: "
          f"{meta['export_n_episodes']} in export, {meta['kept_after_export']} saved after it "
          f"kept, {meta['kept_removed_after_export']} finished/unsaved after it kept hidden, "
          f"{meta['previous_only_count']} dropped; dashboard shows {shown}")
