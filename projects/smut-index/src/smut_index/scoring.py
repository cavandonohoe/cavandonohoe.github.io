"""Pre-specified scene-level index and exposure-safe metrics."""

import math

from .schema import Event, Scene

DIMENSIONS = ["explicit", "anatomy", "intimacy", "language", "intensity"]
SEXUAL = {
    "kissing",
    "sexual_touching",
    "implied_activity",
    "explicit_activity",
    "nudity",
    "sexual_dialogue",
    "sexual_thoughts",
    "sexual_violence",
}


def sexual_label(label):
    return label.category in SEXUAL and label.subtype not in {
        "casual_affection",
        "nonsexual",
        "nonsexual_partial",
        "nonsexual_full",
    }


def scene_dimensions(events: list[Event], channel="consensual"):
    """Max within dimension prevents repeated or overlapping labels inflating scores.

    Unknown/ambiguous consent is excluded from consensual and retained in all-content.
    Nonsexual nudity and affection remain available as descriptive categories.
    """
    if channel not in {"consensual", "all", "coercive"}:
        raise ValueError("Unknown channel")
    values = dict.fromkeys(DIMENSIONS, 0.0)
    for event in events:
        if event.verification != "primary_verified":
            continue
        if channel == "consensual" and event.consent != "consensual":
            continue
        if channel == "coercive" and event.consent not in {"coercive", "violent"}:
            continue
        for label in event.labels:
            if not sexual_label(label):
                continue
            value = label.severity / 5
            dim = {
                "explicit_activity": "explicit",
                "implied_activity": "explicit",
                "nudity": "anatomy",
                "kissing": "intimacy",
                "sexual_touching": "intimacy",
                "sexual_dialogue": "language",
                "sexual_thoughts": "language",
            }.get(label.category)
            if dim:
                values[dim] = max(values[dim], value)
            values["intensity"] = max(values["intensity"], value)
    return values


def weighted_score(values, weights):
    if set(values) != set(DIMENSIONS) or any(not 0 <= v <= 1 for v in values.values()):
        raise ValueError("Scene components must cover five dimensions and lie in [0, 1]")
    if set(weights) != set(DIMENSIONS) or any(v < 0 for v in weights.values()):
        raise ValueError("Weights must cover exactly the five nonnegative components")
    if not math.isclose(sum(weights.values()), 1):
        raise ValueError("Weights must sum to one")
    return 100 * sum(weights[k] * values[k] for k in DIMENSIONS)


def index_bounds(
    events: list[Event],
    scenes: list[Scene],
    weights,
    *,
    inventory_complete=False,
    channel="consensual",
):
    """Logical missingness bounds, NOT a confidence interval or a calibrated scale."""
    if not inventory_complete or not scenes:
        return {
            "score": None,
            "lower": None,
            "upper": None,
            "status": "no_complete_scene_inventory",
        }
    if len({s.work for s in scenes}) != 1:
        raise ValueError("Compute an index for one work at a time")
    reviewed = [s for s in scenes if s.reviewed_all_categories]
    total = 0
    for scene in reviewed:
        subset = [
            e
            for e in events
            if (e.work, e.unit_id, e.scene_id) == (scene.work, scene.unit_id, scene.scene_id)
        ]
        if any(e.verification != "primary_verified" for e in subset):
            raise ValueError("Resolve all candidates before marking a scene reviewed")
        total += weighted_score(scene_dimensions(subset, channel), weights)
    low = total / len(scenes)
    high = low + 100 * (len(scenes) - len(reviewed)) / len(scenes)
    return {
        "score": low if len(reviewed) == len(scenes) else None,
        "lower": low,
        "upper": high,
        "status": "complete" if len(reviewed) == len(scenes) else "partial",
    }


def union_length(intervals):
    """Union of half-open intervals, so simultaneous events count once."""
    merged = []
    for start, end in sorted(intervals):
        if start < 0 or end <= start:
            raise ValueError("Invalid interval")
        if merged and start <= merged[-1][1]:
            merged[-1][1] = max(end, merged[-1][1])
        else:
            merged.append([start, end])
    return sum(end - start for start, end in merged)


def exposure_metrics(events, scenes):
    """Only fully reviewed scenes and primary-verified events have valid denominators."""
    if len({s.work for s in scenes}) > 1:
        raise ValueError("Compute exposure metrics for one work at a time")
    reviewed = [s for s in scenes if s.reviewed_all_categories]
    keys = {(s.work, s.unit_id, s.scene_id) for s in reviewed}
    selected = [
        e
        for e in events
        if e.verification == "primary_verified"
        and (e.work, e.unit_id, e.scene_id) in keys
        and any(sexual_label(label) for label in e.labels)
    ]
    if not reviewed:
        return dict.fromkeys(
            [
                "events",
                "events_per_10000_words",
                "events_per_hour",
                "scenes_pct",
                "runtime_pct",
                "severity_per_scene",
                "severity_per_10000_words",
                "sexual_word_span_pct",
            ],
            None,
        )
    words = sum(s.words for s in reviewed) if all(s.words for s in reviewed) else None
    seconds = (
        sum(s.runtime_seconds for s in reviewed)
        if all(s.runtime_seconds for s in reviewed)
        else None
    )
    duration = 0
    for scene in reviewed:
        subset = [
            e
            for e in selected
            if (e.work, e.unit_id, e.scene_id) == (scene.work, scene.unit_id, scene.scene_id)
        ]
        if any(e.start_seconds is None for e in subset):
            duration = None
            break
        duration += union_length([(e.start_seconds, e.end_seconds) for e in subset])
    word_span = 0
    for scene in reviewed:
        subset = [
            e
            for e in selected
            if (e.work, e.unit_id, e.scene_id) == (scene.work, scene.unit_id, scene.scene_id)
        ]
        if any(e.word_start is None for e in subset):
            word_span = None
            break
        word_span += union_length([(e.word_start, e.word_end) for e in subset])
    severity_total = sum(max(x.severity for x in e.labels if sexual_label(x)) for e in selected)
    return {
        "events": len(selected),
        "events_per_10000_words": len(selected) / words * 10000 if words else None,
        "events_per_hour": len(selected) / seconds * 3600 if seconds else None,
        "scenes_pct": 100
        * len({(e.work, e.unit_id, e.scene_id) for e in selected})
        / len(reviewed),
        "runtime_pct": 100 * duration / seconds if seconds and duration is not None else None,
        "severity_per_scene": severity_total / len(reviewed),
        "severity_per_10000_words": severity_total / words * 10000 if words else None,
        "sexual_word_span_pct": 100 * word_span / words
        if words and word_span is not None
        else None,
    }
