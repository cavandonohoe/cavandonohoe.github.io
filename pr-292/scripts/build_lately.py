#!/usr/bin/env python3
"""Summarize the last 30 days of human (non-bot) commits into data/lately.json."""
import subprocess
from collections import Counter
from datetime import datetime, timezone, timedelta

from generated_output import is_bot_author, write_json_if_changed

WINDOW_DAYS = 30
SEP = "\x1f"

now = datetime.now(timezone.utc)
since = now - timedelta(days=WINDOW_DAYS)

# One header line per commit (marked with SEP), followed by its file names.
log = subprocess.check_output(
    [
        "git", "log", f"--since={since.isoformat()}", "--name-only",
        f"--pretty=format:{SEP}%an{SEP}%ae", "HEAD",
    ],
    text=True,
).splitlines()

commits = 0
areas = Counter()
include = False
for line in log:
    if line.startswith(SEP):
        _, name, email = line.split(SEP, 2)
        include = not is_bot_author(name, email)
        commits += include
    elif line and include:
        areas[line.split("/")[0] if "/" in line else "site"] += 1

write_json_if_changed(
    "data/lately.json",
    {
        "generated_at": now.isoformat(),
        "window_days": WINDOW_DAYS,
        "commits": commits,
        "excludes_bot_commits": True,
        "active_areas": [
            {"name": k, "file_touches": v} for k, v in areas.most_common(5)
        ],
    },
)
