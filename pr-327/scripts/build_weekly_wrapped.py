#!/usr/bin/env python3
"""Write a 7-day activity snapshot to data/weekly/<date>.json."""
import os
import subprocess
from datetime import datetime, timezone, timedelta

from generated_output import merged_prs_since, write_json_if_changed

repo = os.environ.get("GITHUB_REPOSITORY", "cavandonohoe/cavandonohoe.github.io")
now = datetime.now(timezone.utc)
since = now - timedelta(days=7)

log = subprocess.check_output(
    ["git", "log", f"--since={since.isoformat()}", "--pretty=%H", "HEAD"], text=True
).splitlines()
merged = sorted(merged_prs_since(repo, since), key=lambda p: p["merged_at"])

write_json_if_changed(
    f"data/weekly/{now.date().isoformat()}.json",
    {
        "generated_at": now.isoformat(),
        "period_start": since.isoformat(),
        "commits": len(log),
        "prs_merged": len(merged),
        "merged_prs": [{"number": p["number"], "title": p["title"]} for p in merged],
    },
)
