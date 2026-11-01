"""Compute only identified quantities; unavailable outcomes stay JSON null."""

import json
from pathlib import Path

import pandas as pd

from .schema import Event, Scene, validate_dataset
from .scoring import exposure_metrics, index_bounds


def load_project(root: Path):
    def jsonl(path, model):
        return [model.model_validate_json(line) for line in path.read_text().splitlines() if line]

    sources = json.loads((root / "data/sources.json").read_text())
    events = jsonl(root / "data/events.jsonl", Event)
    scenes = jsonl(root / "data/scenes.jsonl", Scene)
    validate_dataset(events, scenes, {s["source_id"] for s in sources})
    chapters = pd.read_csv(root / "data/chapters.csv")
    chapter_keys = set(zip(chapters.work, chapters.unit_id, strict=True))
    if any((e.work, e.unit_id) not in chapter_keys for e in events):
        raise ValueError("Event references unknown chapter")
    return sources, events, scenes, chapters


def analyze(root):
    sources, events, scenes, chapters = load_project(root)
    weights = json.loads((root / "configs/weights.json").read_text())
    coverage = json.loads((root / "data/coverage.json").read_text())
    results = {"scope": coverage["scope"], "verdict": "undetermined", "works": {}}
    for work in coverage["works"]:
        subset = [e for e in events if e.work == work]
        scene_subset = [s for s in scenes if s.work == work]
        ch = chapters[chapters.work == work]
        results["works"][work] = {
            "secondary_clusters": sum(e.evidence == "secondary" for e in subset),
            "primary_verified_events": sum(e.verification == "primary_verified" for e in subset),
            "summary_chapters_reviewed": int(ch.summary_reviewed.sum()),
            "primary_chapters_reviewed": int(ch.primary_reviewed.sum()),
            "chapter_units": len(ch),
            "metrics_on_reviewed_scenes": exposure_metrics(subset, scene_subset),
            "sensitivity": {
                name: index_bounds(
                    subset,
                    scene_subset,
                    w,
                    inventory_complete=coverage["works"][work]["complete_scene_inventory"],
                    channel="all",
                )
                for name, w in weights.items()
            },
        }
    return results
