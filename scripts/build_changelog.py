#!/usr/bin/env python3
import json
import os
import re
import subprocess
from collections import defaultdict
from datetime import datetime, timezone, timedelta

repo = os.environ.get("GITHUB_REPOSITORY", "cavandonohoe/cavandonohoe.github.io")
since = datetime.now(timezone.utc) - timedelta(days=30)
since_iso = since.isoformat().replace("+00:00", "Z")

def gh(path):
    return json.loads(subprocess.check_output(["gh", "api", path], text=True))

prs = gh(f"/repos/{repo}/pulls?state=closed&per_page=100")
events = []
prs_by_day = defaultdict(list)
for p in prs:
    if not p.get("merged_at"):
        continue
    dt = datetime.fromisoformat(p["merged_at"].replace("Z", "+00:00"))
    if dt < since:
        continue
    event = {
        "date": dt.date().isoformat(),
        "type": "merged_pr",
        "title": p["title"],
        "url": p["html_url"],
    }
    events.append(event)
    prs_by_day[event["date"]].append(event)

events.sort(key=lambda x: (x["date"], x["title"]), reverse=True)

commits = gh(f"/repos/{repo}/commits?since={since_iso}&per_page=100")
signals_by_day = defaultdict(list)

def add_signal(day, emoji, text, url=None, priority=50):
    key = (emoji, text)
    if any((x["emoji"], x["text"]) == key for x in signals_by_day[day]):
        return
    item = {"emoji": emoji, "text": text, "priority": priority}
    if url:
        item["url"] = url
    signals_by_day[day].append(item)

for c in commits:
    commit = c.get("commit", {})
    author = commit.get("author") or {}
    date_raw = author.get("date")
    if not date_raw:
        continue
    dt = datetime.fromisoformat(date_raw.replace("Z", "+00:00"))
    day = dt.date().isoformat()
    message = (commit.get("message") or "").splitlines()[0]
    lower = message.lower()
    url = c.get("html_url")

    if "auto-update saved podcast episodes" in lower:
        try:
            detail = gh(f"/repos/{repo}/commits/{c['sha']}")
            for f in detail.get("files", []):
                if f.get("filename") != "podcast-dashboard/data/saved_episodes.json":
                    continue
                patch = f.get("patch") or ""
                old = re.search(r'^-\s*"n_episodes":\s*(\d+)', patch, re.M)
                new = re.search(r'^\+\s*"n_episodes":\s*(\d+)', patch, re.M)
                if old and new:
                    delta = int(new.group(1)) - int(old.group(1))
                    if delta > 0:
                        label = "episode" if delta == 1 else "episodes"
                        add_signal(day, "🎧", f"{delta} podcast {label} added", url, 100)
        except Exception:
            add_signal(day, "🎧", "Podcast library refreshed", url, 75)
    elif "imdb" in lower or "watched tv" in lower:
        add_signal(day, "🎬", "Movie & TV data refreshed", url, 70)

def category_signal(title):
    t = title.lower()
    if "podcast" in t or "spotify" in t:
        return ("🎧", "Podcast + Spotify experience improved", 90)
    if "cv" in t or "resume" in t:
        return ("📄", "CV rebuilt from the latest source data", 85)
    if "publication" in t:
        return ("📚", "Publications section expanded", 85)
    if "imdb" in t or "movie" in t or "tv" in t:
        return ("🎬", "Movie & TV projects updated", 80)
    if any(k in t for k in ["lately", "changelog", "year-in-review", "wrapped", "activity", "case-study", "data art"]):
        return ("⚙️", "Developer activity + site automation expanded", 78)
    if "heatmap" in t or "housing" in t or "rental" in t:
        return ("🗺️", "Data-viz projects polished", 72)
    if "public data" in t:
        return ("💬", "Public-data interaction workflow added", 72)
    if "shiny" in t:
        return ("✨", "Shiny app experience improved", 70)
    return None

for day, day_prs in prs_by_day.items():
    count = len(day_prs)
    if count >= 2:
        add_signal(day, "💻", f"{count} PRs merged", priority=95)
    seen = set()
    for p in day_prs:
        signal = category_signal(p["title"])
        if not signal:
            continue
        emoji, text, priority = signal
        if text in seen:
            continue
        seen.add(text)
        add_signal(day, emoji, text, p["url"], priority)

daily_summaries = []
all_days = sorted(set(prs_by_day) | set(signals_by_day), reverse=True)
for day in all_days:
    items = sorted(signals_by_day[day], key=lambda x: (-x["priority"], x["text"]))[:3]
    for item in items:
        item.pop("priority", None)
    if not items and prs_by_day.get(day):
        p = prs_by_day[day][0]
        items = [{"emoji": "🚀", "text": p["title"], "url": p["url"]}]
    if items:
        daily_summaries.append({"date": day, "items": items})

os.makedirs("data", exist_ok=True)
payload = {
    "generated_at": datetime.now(timezone.utc).isoformat(),
    "events": events,
    "daily_summaries": daily_summaries[:14],
}
open("data/changelog.json", "w").write(json.dumps(payload, indent=2) + "\n")
