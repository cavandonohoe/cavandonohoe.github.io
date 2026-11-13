#!/usr/bin/env python3
"""Write per-directory project metadata to data/project_case_studies.json."""
import os
import subprocess
from collections import Counter

from generated_output import write_json_if_changed

skip = {".git", ".github", "data", "scripts"}
dirs = [d for d in os.listdir(".") if os.path.isdir(d) and not d.startswith(".") and d not in skip]
projects = []
for d in sorted(dirs):
    files = subprocess.check_output(["git", "ls-files", d], text=True).splitlines()
    if not files:
        continue
    ext = Counter(os.path.splitext(f)[1].lower() or "[none]" for f in files)
    commits = int(
        subprocess.check_output(["git", "rev-list", "--count", "HEAD", "--", d], text=True).strip()
        or 0
    )
    projects.append({
        "project": d,
        "tracked_files": len(files),
        "commits_touching_project": commits,
        "top_extensions": ext.most_common(5),
    })

write_json_if_changed("data/project_case_studies.json", {"projects": projects})
