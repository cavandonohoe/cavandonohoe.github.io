"""High-recall lexical candidates; output must be adjudicated, not scored as truth."""

import hashlib
import re


def candidates(records, config):
    output = []
    for row in records:
        labels = []
        for category, pattern in config["patterns"].items():
            hits = list(re.finditer(pattern, row["text"], re.IGNORECASE))
            if len(hits) >= config.get("minimum_pattern_hits", 1):
                labels.append(
                    {"category": category, "character_spans": [[m.start(), m.end()] for m in hits]}
                )
        if labels:
            identity = f"{row['work']}|{row['unit_id']}|{row['scene_id']}|{row['sha256']}"
            output.append(
                {
                    "candidate_id": hashlib.sha256(identity.encode()).hexdigest()[:20],
                    "work": row["work"],
                    "unit_id": row["unit_id"],
                    "scene_id": row["scene_id"],
                    "labels": labels,
                    "verification": "candidate",
                    "method": config["version"],
                    "confidence": None,
                }
            )
    return list({row["candidate_id"]: row for row in output}.values())


def review_sample(frame, per_stratum=2, seed=20261031):
    """Stratify labeled candidates AND unlabeled negatives by work/unit and category.

    This creates a queue, not ground truth. Keep strata and selection probabilities
    when estimating population-level performance from unequal sampling.
    """
    return (
        frame.groupby(["work", "stratum", "candidate_category"], dropna=False, group_keys=False)
        .sample(frac=1, random_state=seed)
        .groupby(["work", "stratum", "candidate_category"], dropna=False)
        .head(per_stratum)
    )
