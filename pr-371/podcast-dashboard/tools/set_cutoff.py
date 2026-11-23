"""Set or clear the manual completion cutoff for the saved-podcast dashboard.

I listen oldest publication date first. After a session, the oldest episode
still in the queue is the boundary: everything published strictly before its
release date is finished. The dashboard hides those episodes from the default
queue; the boundary episode and everything else released the same day stay.

The cutoff lives in data/completion_cutoff.json, which neither the daily API
refresh nor the export import writes, so it survives both.

    python3 podcast-dashboard/tools/set_cutoff.py --show "Planet Money" \\
        --title "Some episode title" --released 2026-03-14
    python3 podcast-dashboard/tools/set_cutoff.py --clear
"""
import argparse
import datetime
import json
from pathlib import Path

DATA = Path(__file__).resolve().parents[1] / 'data'
CUTOFF_PATH = DATA / 'completion_cutoff.json'
EPISODES_PATH = DATA / 'saved_episodes.json'
EMPTY = {'cutoff': None, 'boundary': None, 'confirmed_at': None}


def find_boundary(episodes, show, title, released):
    """Return the saved episode matching show + title (case-insensitive).

    Raises ValueError if nothing matches, the match is ambiguous, or its stored
    release date disagrees with `released`, so a typo can't set a wrong cutoff.
    """
    def norm(s):
        return ' '.join((s or '').split()).casefold()

    matches = [e for e in episodes
               if norm(e.get('show')) == norm(show) and norm(e.get('name')) == norm(title)]
    if not matches:
        raise ValueError(f'No saved episode titled {title!r} from {show!r}.')
    if len(matches) > 1:
        raise ValueError(f'{len(matches)} saved episodes match {title!r} from {show!r}.')
    episode = matches[0]
    stored = episode.get('release')
    if stored and stored != released:
        raise ValueError(f'{title!r} was released {stored}, not {released}.')
    return episode


def build_cutoff(episode, released, today=None):
    return {'cutoff': released,
            'boundary': {'id': episode.get('id'), 'show': episode.get('show'),
                         'name': episode.get('name'), 'release': released},
            'confirmed_at': (today or datetime.date.today()).isoformat()}


def main():
    parser = argparse.ArgumentParser(description=__doc__,
                                     formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument('--show', help='show of the oldest episode still in the queue')
    parser.add_argument('--title', help='title of that episode')
    parser.add_argument('--released', type=datetime.date.fromisoformat,
                        help="that episode's publication date (YYYY-MM-DD)")
    parser.add_argument('--clear', action='store_true', help='remove the cutoff')
    args = parser.parse_args()

    if args.clear:
        config = EMPTY
    else:
        if not (args.show and args.title and args.released):
            parser.error('--show, --title and --released are required (or use --clear)')
        released = args.released.isoformat()
        episodes = json.loads(EPISODES_PATH.read_text())['episodes']
        try:
            episode = find_boundary(episodes, args.show, args.title, released)
        except ValueError as err:
            parser.error(str(err))
        config = build_cutoff(episode, released)
    CUTOFF_PATH.write_text(json.dumps(config, indent=2, ensure_ascii=False) + '\n')
    print('Cutoff cleared' if args.clear else
          f"Cutoff set: hiding episodes published before {config['cutoff']}")


if __name__ == '__main__':
    main()
