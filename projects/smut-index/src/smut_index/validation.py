"""Metrics only for adjudicated, matched sampling units. No invented gold labels."""

import math


def binary_metrics(truth, predicted):
    if len(truth) != len(predicted) or not truth:
        raise ValueError("Nonempty matched labels required")
    if any(x not in (0, 1) for x in [*truth, *predicted]):
        raise ValueError("Binary labels required")
    tp = sum(a == b == 1 for a, b in zip(truth, predicted, strict=True))
    fp = sum(a == 0 and b == 1 for a, b in zip(truth, predicted, strict=True))
    fn = sum(a == 1 and b == 0 for a, b in zip(truth, predicted, strict=True))
    return {
        "tp": tp,
        "fp": fp,
        "fn": fn,
        "precision": tp / (tp + fp) if tp + fp else None,
        "recall": tp / (tp + fn) if tp + fn else None,
    }


def wilson(k, n, z=1.959963984540054):
    """Binomial interval for an actual random validation sample, not convenience samples."""
    if n <= 0 or not 0 <= k <= n:
        raise ValueError("Invalid binomial counts")
    p = k / n
    center = (p + z * z / (2 * n)) / (1 + z * z / n)
    half = z * math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / (1 + z * z / n)
    return max(0, center - half), min(1, center + half)


def cohen_kappa(a, b):
    if len(a) != len(b) or not a:
        raise ValueError("Two independent matched annotation vectors required")
    observed = sum(x == y for x, y in zip(a, b, strict=True)) / len(a)
    expected = sum(a.count(k) / len(a) * b.count(k) / len(b) for k in set(a + b))
    return (observed - expected) / (1 - expected) if expected < 1 else None
