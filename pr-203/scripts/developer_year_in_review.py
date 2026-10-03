#!/usr/bin/env python3
"""Build a yearly public-GitHub developer snapshot for cavandonohoe."""
import json
import os
import subprocess
import tempfile
import urllib.parse
import urllib.request
from collections import Counter
from datetime import date, datetime, timedelta, timezone
from pathlib import Path

USER = os.getenv("GITHUB_USER", "cavandonohoe")
TOKEN = os.getenv("GH_STATS_TOKEN") or os.getenv("GITHUB_TOKEN")
YEAR = int(os.getenv("REVIEW_YEAR", "0")) or (datetime.now(timezone.utc).year - 1 if datetime.now(timezone.utc).month == 1 else datetime.now(timezone.utc).year)
START, END = f"{YEAR}-01-01", f"{YEAR + 1}-01-01"
OUT = Path("data/year-in-review") / f"{YEAR}.json"


def api(path):
    req = urllib.request.Request("https://api.github.com" + path, headers={"Accept": "application/vnd.github+json", "User-Agent": "developer-year-in-review"})
    if TOKEN:
        req.add_header("Authorization", f"Bearer {TOKEN}")
    with urllib.request.urlopen(req) as response:
        return json.load(response)


def public_repos():
    repos, page = [], 1
    while True:
        batch = api(f"/users/{USER}/repos?type=owner&sort=updated&per_page=100&page={page}")
        repos.extend(r for r in batch if not r.get("fork") and not r.get("archived"))
        if len(batch) < 100:
            return repos
        page += 1


def git(repo, *args):
    return subprocess.check_output(["git", "-C", str(repo), *args], text=True, stderr=subprocess.DEVNULL)


def longest_streak(days):
    days = sorted(set(days))
    if not days:
        return 0
    best = run = 1
    for previous, current in zip(days, days[1:]):
        run = run + 1 if current == previous + timedelta(days=1) else 1
        best = max(best, run)
    return best


repos = public_repos()
commit_total = additions = 0
commit_days, months = [], Counter()
repo_commits, touched_repos, language_bytes = Counter(), [], Counter()

with tempfile.TemporaryDirectory() as tmp:
    for meta in repos:
        name = meta["name"]
        dest = Path(tmp) / name
        try:
            subprocess.run(["git", "clone", "--quiet", "--filter=blob:none", "--no-checkout", meta["clone_url"], str(dest)], check=True, timeout=120)
            rows = git(dest, "log", "--all", f"--since={START}", f"--until={END}", f"--author={USER}", "--format=%H|%aI").splitlines()
            if not rows:
                continue
            touched_repos.append(meta["full_name"])
            repo_commits[meta["full_name"]] = len(rows)
            commit_total += len(rows)
            for row in rows:
                _, stamp = row.split("|", 1)
                d = datetime.fromisoformat(stamp).date()
                commit_days.append(d)
                months[d.strftime("%B")] += 1
            numstat = git(dest, "log", "--all", f"--since={START}", f"--until={END}", f"--author={USER}", "--numstat", "--format=")
            for line in numstat.splitlines():
                parts = line.split("\t")
                if len(parts) >= 2 and parts[0].isdigit():
                    additions += int(parts[0])
            try:
                langs = api(f"/repos/{meta['full_name']}/languages")
                language_bytes.update(langs)
            except Exception:
                pass
        except Exception as exc:
            print(f"Skipping {meta['full_name']}: {exc}")

query = urllib.parse.quote(f"author:{USER} type:pr created:{START}..{YEAR}-12-31")
try:
    prs = api(f"/search/issues?q={query}&per_page=1").get("total_count", 0)
except Exception:
    prs = None

out = {
    "year": YEAR,
    "commits": commit_total,
    "pull_requests": prs,
    "repositories_touched": len(touched_repos),
    "lines_added": additions,
    "most_used_language": language_bytes.most_common(1)[0][0] if language_bytes else None,
    "most_active_month": months.most_common(1)[0][0] if months else None,
    "longest_streak_days": longest_streak(commit_days),
    "biggest_project": repo_commits.most_common(1)[0][0].split("/", 1)[-1] if repo_commits else None,
    "biggest_project_commits": repo_commits.most_common(1)[0][1] if repo_commits else 0,
    "repositories": sorted(touched_repos),
    "scope": "Public repositories owned by the GitHub user; commit totals use author matching and PRs use GitHub search.",
    "generated_at": datetime.now(timezone.utc).isoformat(),
}
OUT.parent.mkdir(parents=True, exist_ok=True)
OUT.write_text(json.dumps(out, indent=2) + "\n")
print(json.dumps(out, indent=2))
