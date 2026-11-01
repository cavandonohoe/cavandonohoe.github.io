"""Lawful local prose ingestion. Raw novel text never enters public outputs."""

import hashlib
import json
from pathlib import Path


def read_local_units(path: Path):
    """Read user-supplied JSONL scene text; caller must have lawful access.

    Input: work, unit_id, scene_id, text. Returns private records with hashes.
    No automatic upload, DRM removal, web scraping or copyrighted text export.
    """
    records = [json.loads(line) for line in path.read_text().splitlines() if line.strip()]
    for row in records:
        for key in ["work", "unit_id", "scene_id", "text"]:
            if key not in row:
                raise ValueError(f"Missing {key}")
        row["sha256"] = hashlib.sha256(row["text"].encode()).hexdigest()
    return records
