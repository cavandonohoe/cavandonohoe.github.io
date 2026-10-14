#!/usr/bin/env python3
"""Build annual snapshots from each public repository's default-branch history."""
import argparse
import calendar
import json
import os
import re
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
# Directories that hold dependencies, build output, rendered sites, or docs.
# Like Linguist's vendor/documentation lists, these never count as authored code.
DEPENDENCY_DIRS = {
    "node_modules", "bower_components", "vendor", "vendors", "third_party", "third-party",
    "site_libs", "dist", "_site", "_freeze", "docs", ".next", "coverage", "__pycache__",
}
# Minified or bundled files, matched on the file name. Lockfiles need no rule:
# they are not in LANGUAGES, so they never reach language totals.
GENERATED_FILE = re.compile(r"([.-]min\.(js|css)|\.bundle\.js)$", re.IGNORECASE)
# Deploy actions (e.g. peaceiris/actions-gh-pages) commit rendered sites under
# the triggering user's name with a "deploy:" subject; they are not authored work.
DEPLOY_SUBJECT = re.compile(r"^deploy:", re.IGNORECASE)


def excluded_from_language_stats(repo, path):
    """Apply Linguist-style generated/vendored rules: .gitattributes plus built-in patterns.

    Explicit linguist-generated/linguist-vendored attributes win, as in Linguist.
    Otherwise built-in patterns cover dependency, build, rendered-site, and docs
    directories, renv's bootstrap script and project library, and minified bundles.
    This approximates, rather than reproduces, GitHub Linguist's full heuristics.
    """
    try:
        # Read the selected ref's rules even in --no-checkout clones. These rules
        # intentionally also classify deleted files and their historical paths.
        attrs = git(repo, "check-attr", "--source=HEAD", "linguist-generated", "linguist-vendored", "--", path).splitlines()
    except subprocess.CalledProcessError as exc:
        raise RuntimeError(f"Cannot classify language statistics for {repo}: {path}") from exc
    values = {line.rsplit(": ", 1)[-1].strip().casefold() for line in attrs}
    if values & {"set", "true"}:
        return True
    if values & {"unset", "false"}:
        return False
    parts = Path(path).parts
    lowered = tuple(part.casefold() for part in parts)
    # renv writes this bootstrap script; it is not authored project code.
    if lowered[-2:] == ("renv", "activate.r"):
        return True
    if any(lowered[i:i + 2] == ("renv", "library") for i in range(len(lowered) - 1)):
        return True
    if any(part in DEPENDENCY_DIRS for part in lowered[:-1]):
        return True
    if GENERATED_FILE.search(parts[-1] if parts else path):
        return True
    # Downloaded HTML under R packages' raw-data directory is an input dataset.
    # Keep the authored R/Python/JavaScript scripts alongside it counted.
    return Path(path).suffix.casefold() == ".html" and "data-raw" in parts[:-1]


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
    # quotePath=false keeps non-ASCII paths literal instead of octal-escaped.
    return subprocess.check_output(["git", "-c", "core.quotePath=false", "-C", str(repo), *args], text=True)


def default_ref(repo, branch=None):
    """Resolve the default branch so unmerged, rebased, and gh-pages commits are ignored."""
    candidates = [f"refs/remotes/origin/{branch}", f"refs/heads/{branch}"] if branch else []
    candidates.append("refs/remotes/origin/HEAD")
    for ref in candidates:
        try:
            return git(repo, "rev-parse", "--verify", "--quiet", f"{ref}^{{commit}}").strip()
        except subprocess.CalledProcessError:
            continue
    raise ValueError(f"Cannot resolve the default branch for {repo}")


def longest_streak(days):
    days = sorted(set(days))
    best = run = int(bool(days))
    for previous, current in zip(days, days[1:]):
        run = run + 1 if current == previous + timedelta(days=1) else 1
        best = max(best, run)
    return best


def is_bot(name, email):
    return "[bot]" in name.casefold() or "[bot]" in email.casefold()


def authored_by_user(name, email):
    if is_bot(name, email):
        return False
    local = email.casefold().split("@", 1)[0].split("+")[-1]
    return name.casefold() in AUTHOR_NAMES or local == USER.casefold()


def personal_commit(name, email, committer_name, committer_email, subject):
    """Author-matched commits, minus bot-committed and deploy commits."""
    return (
        authored_by_user(name, email)
        and not is_bot(committer_name, committer_email)
        and not DEPLOY_SUBJECT.match(subject)
    )


def parse_numstat(body):
    """Yield (added, path) from `git log -z --numstat`; renames use the new path."""
    tokens = iter(body.lstrip("\n").split("\0"))
    for token in tokens:
        parts = token.lstrip("\n").split("\t", 2)
        if len(parts) != 3:
            continue
        path = parts[2]
        if not path:
            # Rename/copy entries are "added\tdeleted\t\0old\0new".
            next(tokens, None)
            path = next(tokens, "")
        if parts[0].isdigit() and path:
            yield int(parts[0]), path


def read_history(repo, branch=None):
    if git(repo, "rev-parse", "--is-shallow-repository").strip() == "true":
        raise ValueError(f"Full commit history is required: {repo}")
    ref = default_ref(repo, branch)
    # -M counts moved files as renames (only changed lines), attributed to the new path.
    log = git(repo, "log", ref, "-z", "-M", "--numstat", "--format=%x1e%H|%aI|%an|%ae|%cn|%ce|%s")
    seen = set()
    exclusions = {}
    for block in log.split("\x1e"):
        if not block.strip():
            continue
        header, _, body = block.partition("\0")
        sha, stamp, name, email, committer_name, committer_email, subject = header.strip().split("|", 6)
        if sha in seen or not personal_commit(name, email, committer_name, committer_email, subject):
            continue
        seen.add(sha)
        # Half-open calendar years, based on author dates normalized to UTC.
        day = datetime.fromisoformat(stamp).astimezone(timezone.utc).date()
        added, languages, excluded_languages = 0, Counter(), Counter()
        language_files = defaultdict(Counter)
        for count, path in parse_numstat(body):
            added += count
            language = LANGUAGES.get(Path(path).suffix.casefold())
            if language:
                if path not in exclusions:
                    exclusions[path] = excluded_from_language_stats(repo, path)
                totals = excluded_languages if exclusions[path] else languages
                totals[language] += count
                if not exclusions[path] and count:
                    language_files[language][path] += count
        yield {"sha": sha, "day": day, "added": added, "languages": languages, "excluded_languages": excluded_languages, "language_files": dict(language_files)}


def ranked(counter):
    """Highest count first; ties break on the key so reruns are deterministic."""
    return sorted(counter.items(), key=lambda item: (-item[1], item[0]))


def snapshot(year, records, now):
    rows = [(repo, row) for repo, row in records if row["day"].year == year and row["day"] <= now.date()]
    repos = Counter(repo for repo, _ in rows)
    days = [row["day"] for _, row in rows]
    months = Counter(day.month for day in days)
    languages = Counter()
    excluded_languages = Counter()
    language_files = defaultdict(dict)
    for repo, row in rows:
        languages.update(row["languages"])
        excluded_languages.update(row.get("excluded_languages", {}))
        for language, files in row.get("language_files", {}).items():
            for path, count in files.items():
                entry = language_files[language].setdefault((repo, path), {
                    "repository": repo, "path": path, "lines_added": 0, "commits": [],
                })
                entry["lines_added"] += count
                entry["commits"].append({"sha": row["sha"], "date": row["day"].isoformat(), "lines_added": count})
    for files in language_files.values():
        for entry in files.values():
            entry["commits"].sort(key=lambda commit: (commit["date"], commit["sha"]), reverse=True)
    project, project_commits = ranked(repos)[0] if repos else (None, 0)
    daily_commits, daily_added = Counter(days), Counter()
    for _, row in rows:
        daily_added[row["day"]] += row["added"]
    daily_activity = [
        {"date": day.isoformat(), "commits": daily_commits[day], "lines_added": daily_added[day]}
        for day in sorted(daily_commits)
    ]
    return {
        "year": year,
        "commits": len(rows),
        "pull_requests": None,
        "repositories_touched": len(repos),
        "lines_added": sum(row["added"] for _, row in rows),
        "most_used_language": ranked(languages)[0][0] if languages else None,
        "language_lines_added": dict(ranked(languages)),
        "language_files": {language: sorted(files.values(), key=lambda entry: (-entry["lines_added"], entry["repository"], entry["path"])) for language, files in sorted(language_files.items())},
        "excluded_language_lines_added": dict(ranked(excluded_languages)),
        "language_basis": "Lines added in recognized first-party source files during this year; Linguist-generated, vendored, dependency, build, docs, minified, and raw HTML input files are excluded.",
        "most_active_month": calendar.month_name[ranked(months)[0][0]] if months else None,
        "longest_streak_days": longest_streak(days),
        "active_days": len(set(days)),
        "monthly_commits": [months[m] for m in range(1, 13)],
        # Active days only (sorted); feeds the year-over-year activity chart.
        "daily_activity": daily_activity,
        "biggest_project": project.split("/", 1)[-1] if project else None,
        "biggest_project_commits": project_commits,
        "repositories": sorted(repos),
        "repository_commits": dict(ranked(repos)),
        "scope": "Author-matched commits on the default branch of public, non-fork repositories owned by the user, including archived projects. Dates use UTC; bot-authored or bot-committed commits, deploy commits, and other authors are excluded. Moved files count only changed lines. Language totals use .gitattributes Linguist overrides plus built-in generated, vendored, dependency, build, docs, minified, and raw HTML input patterns. Overall lines added still include all files. PRs count public pull requests the user authored and merged into their own repositories during the year, when available.",
        "is_partial_year": year == now.year,
        "generated_at": now.isoformat(),
    }


def merged_pr_count(year):
    query = urllib.parse.quote(f"author:{USER} user:{USER} is:pr is:public is:merged merged:{year}-01-01..{year}-12-31")
    return api(f"/search/issues?q={query}&per_page=1")["total_count"]


def write_snapshot(path, out):
    """Write only when something besides generated_at changed, to avoid no-op commits."""
    if path.exists():
        previous = json.loads(path.read_text())
        if {k: v for k, v in previous.items() if k != "generated_at"} == {k: v for k, v in out.items() if k != "generated_at"}:
            return False
    path.write_text(json.dumps(out, indent=2) + "\n")
    return True


def stale_partial_years(output, now):
    """Earlier years whose snapshot is still flagged partial (e.g. the Jan 1 run failed)."""
    years = []
    for path in sorted(output.glob("[0-9][0-9][0-9][0-9].json")):
        year = int(path.stem)
        if year < now.year and json.loads(path.read_text()).get("is_partial_year"):
            years.append(year)
    return years


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--all-years", action="store_true", help="Backfill every year with authored commits; clone each repository once")
    parser.add_argument("--year", type=int)
    parser.add_argument("--local-repos", type=Path, help="JSON map of public repository full names to complete local checkouts (default branch from origin/HEAD)")
    parser.add_argument("--repository-override", action="append", default=[], metavar="OWNER/REPO=PATH", help="Use a full local checkout for a public repository, including its current attribute rules")
    parser.add_argument("--pr-counts", type=Path, help="Pre-fetched public PR counts keyed by year")
    parser.add_argument("--output", type=Path, default=Path("data/year-in-review"))
    args = parser.parse_args()
    overrides = {}
    for entry in args.repository_override:
        name, separator, dest = entry.partition("=")
        if not separator or not name or not dest:
            parser.error("Repository overrides must use OWNER/REPO=PATH")
        overrides[name] = dest
    now = datetime.now(timezone.utc)
    year = args.year or int(os.getenv("REVIEW_YEAR") or 0) or (now.year - 1 if now.month == 1 else now.year)
    if not 1970 <= year <= now.year:
        parser.error("Year must be between 1970 and the current year")
    records = []
    with tempfile.TemporaryDirectory() as tmp:
        if args.local_repos:
            checkouts = {name: (dest, None) for name, dest in json.loads(args.local_repos.read_text()).items()}
        else:
            checkouts = {}
            for meta in public_repos():
                branch = meta.get("default_branch")
                if meta["full_name"] in overrides:
                    checkouts[meta["full_name"]] = (overrides[meta["full_name"]], branch)
                    continue
                dest = Path(tmp) / meta["name"]
                # Fail instead of publishing a silently incomplete archive.
                subprocess.run(["git", "clone", "--quiet", "--no-checkout", meta["clone_url"], str(dest)], check=True, timeout=300)
                checkouts[meta["full_name"]] = (str(dest), branch)
        for repo, (dest, branch) in checkouts.items():
            records.extend((repo, row) for row in read_history(dest, branch))
    if args.all_years:
        years = sorted({row["day"].year for _, row in records if row["day"] <= now.date()})
    else:
        # Also finalize any earlier year still marked partial so a failed Jan 1 run self-heals.
        years = sorted({year, *stale_partial_years(args.output, now)})
    pr_counts = json.loads(args.pr_counts.read_text()) if args.pr_counts else None
    outputs = []
    for selected in years:
        out = snapshot(selected, records, now)
        if pr_counts is not None:
            out["pull_requests"] = pr_counts.get(str(selected))
        else:
            try:
                out["pull_requests"] = merged_pr_count(selected)
            except Exception as exc:
                existing = args.output / f"{selected}.json"
                # Keep the last good value rather than publishing a transient gap.
                out["pull_requests"] = json.loads(existing.read_text()).get("pull_requests") if existing.exists() else None
                print(f"PR count unavailable for {selected}; keeping {out['pull_requests']}: {exc}")
        outputs.append(out)
    args.output.mkdir(parents=True, exist_ok=True)
    for out in outputs:
        changed = write_snapshot(args.output / f"{out['year']}.json", out)
        print(f"{out['year']}: {out['commits']} commits across {out['repositories_touched']} repositories{'' if changed else ' (unchanged)'}")


if __name__ == "__main__":
    main()
