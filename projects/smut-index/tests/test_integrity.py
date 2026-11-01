import json
from pathlib import Path

import pytest
from pydantic import ValidationError

from smut_index.analysis import analyze, load_project
from smut_index.classification import candidates, review_sample
from smut_index.ingestion import read_local_units
from smut_index.schema import Event, Label, Scene, deduplicate_events, validate_dataset
from smut_index.scoring import (
    exposure_metrics,
    index_bounds,
    scene_dimensions,
    union_length,
    weighted_score,
)
from smut_index.validation import binary_metrics, cohen_kappa, wilson
from smut_index.visualization import make_figures

ROOT = Path(__file__).resolve().parents[1]
WEIGHTS = json.loads((ROOT / "configs/weights.json").read_text())["starting"]


def event(**overrides):
    values = dict(
        event_id="test-1",
        work="got",
        unit_id="C01",
        scene_id="scene-1",
        labels=[Label(category="kissing", subtype="romantic", severity=2)],
        consent="consensual",
        source_ids=["test-source"],
        location="synthetic unit",
        evidence="primary",
        verification="primary_verified",
        detection="manual",
        annotator="synthetic_fixture",
        confidence=None,
    )
    values.update(overrides)
    return Event(**values)


def scene(**overrides):
    values = dict(
        work="got",
        unit_id="C01",
        scene_id="scene-1",
        reviewed_all_categories=True,
        words=1000,
        runtime_seconds=None,
    )
    values.update(overrides)
    return Scene(**values)


@pytest.mark.parametrize("severity", [-1, 6])
def test_severity_range(severity):
    with pytest.raises(ValidationError):
        Label(category="kissing", subtype="romantic", severity=severity)


def test_secondary_cannot_be_primary_verified():
    with pytest.raises(ValidationError):
        event(evidence="secondary")


def test_unknown_severity_cannot_be_verified():
    with pytest.raises(ValidationError):
        event(labels=[Label(category="kissing", subtype="romantic")])
    with pytest.raises(ValidationError):
        event(
            labels=[
                Label(
                    category="explicit_activity",
                    subtype="activity_reported_explicitness_unresolved",
                    severity=4,
                )
            ]
        )


@pytest.mark.parametrize(
    "interval",
    [{"word_start": 2}, {"word_start": 9, "word_end": 8}, {"start_seconds": 1, "end_seconds": 1}],
)
def test_invalid_intervals(interval):
    with pytest.raises(ValidationError):
        event(**interval)


def test_no_duplicate_categories():
    with pytest.raises(ValidationError):
        event(labels=[Label(category="kissing", subtype="romantic", severity=1)] * 2)


def test_reference_deduplication_and_conflicts():
    first = event()
    second = event(source_ids=["other-source"])
    assert len(deduplicate_events([first, second])) == 1
    assert deduplicate_events([first, second])[0].source_ids == ["other-source", "test-source"]
    with pytest.raises(ValueError):
        deduplicate_events([first, event(consent="unknown")])


@pytest.mark.parametrize("case", ["duplicate_event", "duplicate_scene", "source", "scene", "span"])
def test_referential_integrity(case):
    es, ss, ids = [event()], [scene()], {"test-source"}
    if case == "duplicate_event":
        es *= 2
    elif case == "duplicate_scene":
        ss *= 2
    elif case == "source":
        ids = set()
    elif case == "scene":
        ss = []
    elif case == "span":
        es = [event(word_start=1, word_end=2000)]
    with pytest.raises(ValueError):
        validate_dataset(es, ss, ids)


def test_overlapping_labels_do_not_inflate_dimension():
    a = event()
    b = event(
        event_id="test-2",
        labels=[Label(category="sexual_touching", subtype="intimate", severity=2)],
    )
    assert scene_dimensions([a, b]) == scene_dimensions([a])
    assert weighted_score(scene_dimensions([a]), WEIGHTS) == pytest.approx(12)


def test_nonsexual_nudity_and_affection_do_not_score():
    labels = [
        Label(category="nudity", subtype="nonsexual_full", severity=2),
        Label(category="nonsexual_affection", subtype="hug", severity=1),
    ]
    assert weighted_score(scene_dimensions([event(labels=labels)]), WEIGHTS) == 0


@pytest.mark.parametrize("consent", ["coercive", "violent", "unknown", "ambiguous"])
def test_consent_is_not_silently_romantic(consent):
    e = event(consent=consent)
    assert weighted_score(scene_dimensions([e]), WEIGHTS) == 0
    assert weighted_score(scene_dimensions([e], "all"), WEIGHTS) > 0


def test_secondary_and_candidates_excluded():
    e = event(evidence="secondary", verification="source_verified")
    assert weighted_score(scene_dimensions([e], "all"), WEIGHTS) == 0
    assert exposure_metrics([e], [scene(reviewed_all_categories=False)])["events"] is None


def test_unresolved_candidates_prevent_complete_scene():
    e = event(verification="candidate")
    with pytest.raises(ValueError):
        index_bounds([e], [scene()], WEIGHTS, inventory_complete=True)
    with pytest.raises(ValueError):
        exposure_metrics([e], [scene()])


def test_bounds_and_no_fake_zero():
    assert index_bounds([], [], WEIGHTS)["score"] is None
    ss = [scene(), scene(scene_id="scene-2", reviewed_all_categories=False)]
    result = index_bounds([event()], ss, WEIGHTS, inventory_complete=True)
    assert result["score"] is None
    assert result["lower"] == pytest.approx(6)
    assert result["upper"] == pytest.approx(56)
    assert index_bounds([event()], [scene()], WEIGHTS, inventory_complete=True)[
        "score"
    ] == pytest.approx(12)


def test_actual_reviewed_negative_is_zero():
    assert exposure_metrics([], [scene()])["events"] == 0
    assert index_bounds([], [scene()], WEIGHTS, inventory_complete=True)["score"] == 0


def test_word_rate_requires_matched_exposure():
    r = exposure_metrics([event()], [scene()])
    assert r["events_per_10000_words"] == 10
    assert r["events_per_hour"] is None
    assert r["runtime_pct"] is None
    assert r["severity_per_10000_words"] == 20
    assert r["sexual_word_span_pct"] is None


def test_word_spans_union():
    es = [event(word_start=0, word_end=100), event(event_id="test-2", word_start=50, word_end=150)]
    assert exposure_metrics(es, [scene()])["sexual_word_span_pct"] == 15


def test_unique_event_count_not_label_count():
    e = event(
        labels=[
            Label(category="kissing", subtype="sexual", severity=2),
            Label(category="sexual_touching", subtype="intimate", severity=3),
        ]
    )
    assert exposure_metrics([e], [scene()])["events"] == 1


def test_union_and_verified_duration():
    assert union_length([(0, 10), (5, 15), (15, 20), (30, 35)]) == 25
    es = [
        event(start_seconds=0, end_seconds=10),
        event(event_id="test-2", start_seconds=5, end_seconds=15),
    ]
    r = exposure_metrics(es, [scene(runtime_seconds=60)])
    assert r["runtime_pct"] == 25


def test_missing_duration_stays_null():
    assert exposure_metrics([event()], [scene(runtime_seconds=60)])["runtime_pct"] is None


def test_mixed_work_denominator_rejected():
    with pytest.raises(ValueError):
        exposure_metrics([], [scene(), scene(work="acotar")])


@pytest.mark.parametrize(
    "weights", [{"explicit": 1}, dict.fromkeys(WEIGHTS, 0.3), {**WEIGHTS, "explicit": -0.1}]
)
def test_invalid_weights(weights):
    with pytest.raises(ValueError):
        weighted_score(dict.fromkeys(WEIGHTS, 0), weights)


def test_components_cannot_exceed_index_scale():
    with pytest.raises(ValueError):
        weighted_score({**dict.fromkeys(WEIGHTS, 0), "explicit": 1.5}, WEIGHTS)


def test_metrics_on_synthetic_gold_only():
    assert binary_metrics([1, 0, 1, 0], [1, 1, 0, 0]) == {
        "tp": 1,
        "fp": 1,
        "fn": 1,
        "precision": 0.5,
        "recall": 0.5,
    }
    assert binary_metrics([0], [0])["precision"] is None
    assert cohen_kappa([1, 0, 1, 0], [1, 0, 1, 0]) == 1
    assert cohen_kappa([1], [1]) is None
    lower, upper = wilson(5, 10)
    assert lower == pytest.approx(0.23659, abs=1e-5)
    assert upper == pytest.approx(0.76341, abs=1e-5)


def test_detector_is_repeatable_and_includes_negative_test(tmp_path):
    # Original synthetic text, not excerpts from either novel. Tests do not estimate accuracy.
    path = tmp_path / "units.jsonl"
    path.write_text(
        json.dumps(dict(work="got", unit_id="C01", scene_id="a", text="They kissed."))
        + "\n"
        + json.dumps(dict(work="got", unit_id="C01", scene_id="b", text="The door is blue."))
    )
    records = read_local_units(path)
    config = json.loads((ROOT / "configs/pipeline.json").read_text())
    found = candidates(records, config)
    assert found == candidates(records, config)
    assert len(found) == 1 and found[0]["verification"] == "candidate"
    assert "text" not in found[0]
    assert candidates(records * 2, config) == found
    assert candidates(records, {**config, "minimum_pattern_hits": 2}) == []


def test_review_sample_contains_negative_strata():
    import pandas as pd

    df = pd.DataFrame(
        [
            dict(work=w, stratum=s, candidate_category=c, id=i)
            for w in ["got", "acotar"]
            for s in ["early", "middle", "late"]
            for c in ["kissing", "negative"]
            for i in range(4)
        ]
    )
    sample = review_sample(df, per_stratum=1)
    assert len(sample) == 12
    assert "negative" in set(sample.candidate_category)
    assert sample.equals(review_sample(df, per_stratum=1))


def test_real_data_is_explicitly_incomplete():
    result = analyze(ROOT)
    assert result["scope"] == "first_novel_vs_first_novel"
    assert result["verdict"] == "undetermined"
    assert result["works"]["got"]["chapter_units"] == 73
    assert result["works"]["acotar"]["chapter_units"] == 46
    for work in result["works"].values():
        assert work["primary_verified_events"] == 0
        assert work["metrics_on_reviewed_scenes"]["events"] is None
        assert all(v["score"] is None for v in work["sensitivity"].values())
    sources, events, scenes, chapters = load_project(ROOT)
    assert len(events) == 11 and len(scenes) == 11
    assert len(chapters) == 119 and chapters.primary_reviewed.sum() == 0
    assert not chapters.duplicated(["work", "unit_id"]).any()
    assert all(label.severity is None for e in events for label in e.labels)


def test_dashboard_views_are_complete_and_do_not_publish_false_scores():
    figures = make_figures(ROOT)
    assert len(figures) == 9
    assert "No defensible winner" in figures["01_overall"].layout.annotations[0].text
    assert len(figures["03_got_chapters"].data[0].x) == 73
    assert len(figures["04_acotar_chapters"].data[0].x) == 46


def test_no_source_corpus_in_committed_data():
    for path in (ROOT / "data").iterdir():
        if path.is_file() and path.suffix in {".json", ".jsonl", ".csv"}:
            assert path.stat().st_size < 100_000
            if path.suffix == ".jsonl":
                for line in path.read_text().splitlines():
                    assert "text" not in json.loads(line)
