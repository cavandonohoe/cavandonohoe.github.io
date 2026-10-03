"""Download official IMDb non-commercial analysis inputs; Python stdlib only."""
import urllib.request, concurrent.futures, pathlib, json
root = pathlib.Path(__file__).resolve().parent / 'inputs'
root.mkdir(exist_ok=True)

def get(name):
    with urllib.request.urlopen('https://datasets.imdbws.com/' + name + '.tsv.gz', timeout=180) as r:
        headers = dict(r.headers)
        with (root / (name + '.tsv.gz')).open('wb') as f:
            while (chunk := r.read(1048576)):
                f.write(chunk)
    (root / (name + '.headers.json')).write_text(json.dumps(headers))
    print(name, flush=True)
with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
    list(pool.map(get, ['title.basics', 'title.episode', 'title.ratings']))
