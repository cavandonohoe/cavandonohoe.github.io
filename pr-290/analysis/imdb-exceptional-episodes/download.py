"""Download official IMDb inputs atomically; Python standard library only."""
import argparse
import concurrent.futures
import email.utils
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


NAMES = ['title.basics', 'title.episode', 'title.ratings']


def source_dates(root):
    """Return {dataset: (export run date, Last-Modified)} from saved headers."""
    dates = {}
    for name in NAMES:
        headers = json.loads((root / (name + '.headers.json')).read_text())
        lower = {key.lower(): value for key, value in headers.items()}
        modified = lower.get('last-modified')
        modified = email.utils.parsedate_to_datetime(modified) if modified else None
        run_date = lower.get('x-amz-meta-run-date') or (modified.date().isoformat() if modified else None)
        dates[name] = (run_date, modified)
    return dates


def date_mismatch_warning(dates):
    """Explain disagreeing export dates, or return None when all files agree."""
    run_dates = {run for run, _ in dates.values()}
    if len(run_dates) <= 1 and None not in run_dates:
        return None
    detail = ', '.join(f'{name} {run or "unknown"}' for name, (run, _) in dates.items())
    return f'IMDb source files have different or missing export dates: {detail}'


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--data-dir', type=pathlib.Path, default=pathlib.Path(__file__).resolve().parent / 'inputs')
    root = parser.parse_args().data_dir
    root.mkdir(parents=True, exist_ok=True)
    with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
        list(pool.map(lambda name: get(name, root), NAMES))
    warning = date_mismatch_warning(source_dates(root))
    if warning:
        # GitHub Actions annotation; still a plain warning line when run locally.
        print(f'::warning::{warning}', flush=True)


if __name__ == '__main__':
    main()
