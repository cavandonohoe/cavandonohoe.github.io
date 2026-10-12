"""Download official IMDb inputs atomically; Python standard library only."""
import argparse
import concurrent.futures
import json
import pathlib
import time
import urllib.request


def get(name, root):
    target = root / (name + '.tsv.gz')
    partial = target.with_suffix('.gz.part')
    for attempt in range(3):
        try:
            with urllib.request.urlopen('https://datasets.imdbws.com/' + name + '.tsv.gz', timeout=180) as response:
                headers = dict(response.headers)
                length = response.headers.get('Content-Length')
                size = 0
                with partial.open('wb') as stream:
                    while chunk := response.read(1048576):
                        size += stream.write(chunk)
                if not size or (length and size != int(length)):
                    raise ValueError(f'Incomplete download: {name}')
            partial.replace(target)
            (root / (name + '.headers.json')).write_text(json.dumps(headers))
            print(name, flush=True)
            return
        except Exception:
            partial.unlink(missing_ok=True)
            if attempt == 2:
                raise
            time.sleep(5 * (attempt + 1))


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data-dir', type=pathlib.Path, default=pathlib.Path(__file__).resolve().parent / 'inputs')
    root = parser.parse_args().data_dir
    root.mkdir(parents=True, exist_ok=True)
    with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
        list(pool.map(lambda name: get(name, root), ['title.basics', 'title.episode', 'title.ratings']))


if __name__ == '__main__':
    main()
