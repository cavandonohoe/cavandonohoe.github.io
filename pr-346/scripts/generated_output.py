"""Shared helpers for the scripts that write generated site data.

Used by build_changelog.py, build_lately.py, build_activity_art.py,
build_weekly_wrapped.py, and build_project_case_studies.py.
"""
import json
import os
import re
import subprocess
from datetime import datetime
from urllib.parse import urlencode

# Run-time stamps that change on every run without the data changing.
TIMESTAMP_KEYS = ("generated_at", "period_start")
SVG_GENERATED_RE = re.compile(r"Generated \d{4}-\d{2}-\d{2}")


def is_bot_author(name, email=""):
    """True for automation authors such as github-actions[bot] or dependabot[bot]."""
    name = (name or "").strip().lower()
    email = (email or "").strip().lower()
    return name.endswith("[bot]") or name == "github-actions" or "[bot]@" in email


def _without_timestamps(obj):
    if isinstance(obj, dict):
        return {
            k: _without_timestamps(v) for k, v in obj.items() if k not in TIMESTAMP_KEYS
        }
    if isinstance(obj, list):
        return [_without_timestamps(v) for v in obj]
    return obj


def _read(path):
    try:
        with open(path, encoding="utf-8") as fh:
            return fh.read()
    except FileNotFoundError:
        return None


def write_json_if_changed(path, payload):
    """Write payload as JSON unless only timestamp fields would change.

    Returns True when the file was (re)written.
    """
    old = _read(path)
    if old is not None:
        try:
            # Round-trip so tuples compare equal to the lists read back from disk.
            new = json.loads(json.dumps(payload))
            if _without_timestamps(json.loads(old)) == _without_timestamps(new):
                print(f"{path}: no data changes (ignoring timestamps); keeping existing file.")
                return False
        except json.JSONDecodeError:
            pass
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(json.dumps(payload, indent=2) + "\n")
    return True


def write_svg_if_changed(path, svg):
    """Write svg unless only its 'Generated YYYY-MM-DD' stamp would change."""
    old = _read(path)
    if old is not None and SVG_GENERATED_RE.sub("", old) == SVG_GENERATED_RE.sub("", svg):
        print(f"{path}: unchanged apart from the generated date; keeping existing file.")
        return False
    os.makedirs(os.path.dirname(path) or ".", exist_ok=True)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(svg)
    return True


def gh_api(path):
    return json.loads(subprocess.check_output(["gh", "api", path], text=True))


def gh_api_pages(path, params, stop=None, per_page=100, max_pages=50):
    """Yield items from a paginated list endpoint, one page at a time.

    stop(page_items) may return True to end pagination early (for example
    when results are sorted newest-first and the page is past the window).
    """
    for page in range(1, max_pages + 1):
        query = urlencode({**params, "per_page": per_page, "page": page})
        items = gh_api(f"{path}?{query}")
        yield from items
        if len(items) < per_page or (stop and stop(items)):
            return
    raise RuntimeError(f"{path}: more than {max_pages} pages; refusing to continue")


def merged_prs_since(repo, since):
    """All PRs merged at or after `since`, regardless of creation order.

    Lists closed PRs by most recent update. merged_at <= updated_at, so once a
    whole page was last updated before `since` no later page can qualify.
    """
    def parse(ts):
        return datetime.fromisoformat(ts.replace("Z", "+00:00"))

    def past_window(items):
        return all(parse(p["updated_at"]) < since for p in items)

    prs = gh_api_pages(
        f"/repos/{repo}/pulls",
        {"state": "closed", "sort": "updated", "direction": "desc"},
        stop=past_window,
    )
    return [p for p in prs if p.get("merged_at") and parse(p["merged_at"]) >= since]
