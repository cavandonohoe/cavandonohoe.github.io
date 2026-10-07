#!/usr/bin/env python3
"""Build annual snapshots from complete public repository commit history."""
import argparse
import calendar
import json
import os
import subprocess
import tempfile
import urllib.parse
import urllib.request
from collections import Counter, defaultdict
from datetime import datetime, timedelta, timezone
from pathlib import Path

USER = os.getenv("GITHUB_USER", "cavandonohoe")
TOKEN = os.getenv("GH_STATS_TOKEN") or os.getenv("GITHUB_TOKEN")
AUTHOR_NAMES = {USER.casefold(), *[x.strip().casefold() for x in os.getenv("GITHUB_AUTHOR_NAMES", "Cavan Donohoe").split(",")]}
LANGUAGES = {".r": "R", ".rmd": "R", ".py": "Python", ".js": "JavaScript", ".cjs": "JavaScript", ".ts": "TypeScript", ".tsx": "TypeScript", ".html": "HTML", ".css": "CSS", ".sql": "SQL", ".sh": "Shell"}


def api(path):
    req = urllib.request.Request("https://api.github.com" + path, headers={"Accept": "application/vnd.github+json", "User-Agent": "developer-year-in-review"})
    if TOKEN:
        req.add_header("Authorization", f"Bearer {TOKEN}")
    with urllib.request.urlopen(req, timeout=60) as response:
        return json.load(response)


def public_repos():
    repos, page = [], 1
    while True:
        batch = api(f"/users/{USER}/repos?type=owner&per_page=100&page={page}")
        # Include archived projects: they still contributed to past years.
        repos.extend(r for r in batch if not r.get("fork") and not r.get("private"))
        if len(batch) < 100:
            return repos
        page += 1


def git(repo, *args):
    return subprocess.check_output(["git", "-C", str(repo), *args], text=True)


def longest_streak(days):
    days = sorted(set(days))
    best = run = int(bool(days))
    for previous, current in zip(days, days[1:]):
        run = run + 1 if current == previous + timedelta(days=1) else 1
        best = max(best, run)
    return best


def authored_by_user(name, email):
    local = email.casefold().split("@", 1)[0].split("+")[-1]
    return name.casefold() in AUTHOR_NAMES or local == USER.casefold()


def read_history(repo):
    if git(repo, "rev-parse", "--is-shallow-repository").strip() == "true":
        raise ValueError(f"Full commit history is required: {repo}")
    log = git(repo, "log", "--all", "--numstat", "--no-renames", "--format=%x1e%H|%aI|%an|%ae")
    seen = set()
    for block in log.split("\x1e"):
        if not block.strip():
            continue
        header, *lines = block.strip().splitlines()
        sha, stamp, name, email = header.split("|", 3)
        if sha in seen or not authored_by_user(name, email):
            continue
        seen.add(sha)
        # Half-open calendar years, based on author dates normalized to UTC.
        day = datetime.fromisoformat(stamp).astimezone(timezone.utc).date()
        added, languages = 0, Counter()
        for line in lines:
            parts = line.split("\t", 2)
            if len(parts) == 3 and parts[0].isdigit():
                count = int(parts[0])
                added += count
                language = LANGUAGES.get(Path(parts[2]).suffix.casefold())
                if language:
                    languages[language] += count
        yield {"sha": sha, "day": day, "added": added, "languages": languages}


def snapshot(year, records, now):
    rows = [(repo, row) for repo, row in records if row["day"].year == year and row["day"] <= now.date()]
    repos = Counter(repo for repo, _ in rows)
    days = [row["day"] for _, row in rows]
    months = Counter(day.month for day in days)
    languages = Counter()
    for _, row in rows:
        languages.update(row["languages"])
    project, project_commits = repos.most_common(1)[0] if repos else (None, 0)
    return {
        "year": year,
        "commits": len(rows),
        "pull_requests": None,
        "repositories_touched": len(repos),
        "lines_added": sum(row["added"] for _, row in rows),
        "most_used_language": languages.most_common(1)[0][0] if languages else None,
        "language_basis": "Lines added in recognized source files during this year",
        "most_active_month": calendar.month_name[months.most_common(1)[0][0]] if months else None,
        "longest_streak_days": longest_streak(days),
        "active_days": len(set(days)),
        "monthly_commits": [months[m] for m in range(1, 13)],
        "biggest_project": project.split("/", 1)[-1] if project else None,
        "biggest_project_commits": project_commits,
        "repositories": sorted(repos),
        "repository_commits": dict(repos.most_common()),
        "scope": "Author-matched commits reachable from branches and tags in public, non-fork repositories owned by the user, including archived projects. Dates use UTC; bots and other authors are excluded. Lines added include generated files. PRs count public pull requests authored by the user, when available.",
        "is_partial_year": year == now.year,
        "generated_at": now.isoformat(),
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--all-years", action="store_true", help="Backfill every year with authored commits; clone each repository once")
    parser.add_argument("--year", type=int)
    parser.add_argument("--local-repos", type=Path, help="JSON map of public repository full names to complete local checkouts")
    parser.add_argument("--pr-counts", type=Path, help="Pre-fetched public PR counts keyed by year")
    parser.add_argument("--output", type=Path, default=Path("data/year-in-review"))
    args = parser.parse_args()
    now = datetime.now(timezone.utc)
    year = args.year or int(os.getenv("REVIEW_YEAR") or 0) or (now.year - 1 if now.month == 1 else now.year)
    if not 1970 <= year <= now.year:
        parser.error("Year must be between 1970 and the current year")
    records = []
    with tempfile.TemporaryDirectory() as tmp:
        if args.local_repos:
            checkouts = json.loads(args.local_repos.read_text())
        else:
            checkouts = {}
            for meta in public_repos():
                dest = Path(tmp) / meta["name"]
                # Fail instead of publishing a silently incomplete archive.
                subprocess.run(["git", "clone", "--quiet", "--no-checkout", meta["clone_url"], str(dest)], check=True, timeout=300)
                checkouts[meta["full_name"]] = str(dest)
        for repo, dest in checkouts.items():
            records.extend((repo, row) for row in read_history(dest))
    years = sorted({row["day"].year for _, row in records if row["day"] <= now.date()}) if args.all_years else [year]
    pr_counts = json.loads(args.pr_counts.read_text()) if args.pr_counts else None
    outputs = []
    for selected in years:
        out = snapshot(selected, records, now)
        if pr_counts is not None:
            out["pull_requests"] = pr_counts.get(str(selected))
        else:
            query = urllib.parse.quote(f"author:{USER} is:pr is:public created:{selected}-01-01..{selected}-12-31")
            try:
                out["pull_requests"] = api(f"/search/issues?q={query}&per_page=1")["total_count"]
            except Exception as exc:
                print(f"PR count unavailable for {selected}: {exc}")
        outputs.append(out)
    args.output.mkdir(parents=True, exist_ok=True)
    for out in outputs:
        (args.output / f"{out['year']}.json").write_text(json.dumps(out, indent=2) + "\n")
        print(f"{out['year']}: {out['commits']} commits across {out['repositories_touched']} repositories")


if __name__ == "__main__":
    main()
