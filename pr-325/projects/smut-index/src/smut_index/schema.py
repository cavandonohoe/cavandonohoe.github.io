"""Public annotation schema. No source prose is stored in event records."""

from typing import Literal

from pydantic import BaseModel, ConfigDict, Field, model_validator

Category = Literal[
    "kissing",
    "nonsexual_affection",
    "sexual_touching",
    "implied_activity",
    "explicit_activity",
    "nudity",
    "sexual_dialogue",
    "sexual_thoughts",
    "sexual_violence",
]
Consent = Literal["consensual", "coercive", "violent", "ambiguous", "unknown", "not_applicable"]


class Label(BaseModel):
    model_config = ConfigDict(extra="forbid")
    category: Category
    subtype: str = Field(min_length=1)
    severity: int | None = Field(default=None, ge=0, le=5)


class Event(BaseModel):
    model_config = ConfigDict(extra="forbid")
    event_id: str
    work: Literal["got", "acotar"]
    unit_id: str
    scene_id: str
    labels: list[Label] = Field(min_length=1)
    consent: Consent
    source_ids: list[str] = Field(min_length=1)
    location: str = Field(min_length=1)
    evidence: Literal["primary", "secondary"]
    confidence: float | None = Field(default=None, ge=0, le=1)
    detection: Literal["rule", "llm", "manual"]
    verification: Literal["candidate", "source_verified", "primary_verified"]
    annotator: str
    participant_age: Literal["adult", "minor_involved", "unknown"] = "unknown"
    start_seconds: float | None = Field(default=None, ge=0)
    end_seconds: float | None = Field(default=None, ge=0)
    word_start: int | None = Field(default=None, ge=0)
    word_end: int | None = Field(default=None, ge=0)
    note: str = ""

    @model_validator(mode="after")
    def consistent(self):
        for a, b in [(self.start_seconds, self.end_seconds), (self.word_start, self.word_end)]:
            if (a is None) != (b is None) or (a is not None and b <= a):
                raise ValueError("Provide both endpoints of a positive half-open interval")
        if self.verification == "primary_verified" and self.evidence != "primary":
            raise ValueError("A secondary guide cannot verify the primary work")
        if self.verification == "primary_verified" and any(x.severity is None for x in self.labels):
            raise ValueError("Primary verification requires explicit severity adjudication")
        if self.verification == "primary_verified" and any(
            "unresolved" in x.subtype for x in self.labels
        ):
            raise ValueError("Resolve provisional subtypes before primary verification")
        if len({x.category for x in self.labels}) != len(self.labels):
            raise ValueError("Use one label per category, with a combined subtype if needed")
        if any(x.category == "sexual_violence" for x in self.labels):
            if self.consent not in {"coercive", "violent", "ambiguous", "unknown"}:
                raise ValueError("Sexual violence cannot be consensual or not applicable")
        return self


class Scene(BaseModel):
    model_config = ConfigDict(extra="forbid")
    work: Literal["got", "acotar"]
    unit_id: str
    scene_id: str
    reviewed_all_categories: bool = False
    words: int | None = Field(default=None, gt=0)
    runtime_seconds: float | None = Field(default=None, gt=0)


def validate_dataset(events: list[Event], scenes: list[Scene], source_ids: set[str]):
    """Reject duplicate identities, broken references and out-of-scene intervals."""
    lookup = {(s.work, s.unit_id, s.scene_id): s for s in scenes}
    if len(lookup) != len(scenes) or len({e.event_id for e in events}) != len(events):
        raise ValueError("Duplicate event or scene identity")
    for event in events:
        scene = lookup.get((event.work, event.unit_id, event.scene_id))
        if scene is None or not set(event.source_ids) <= source_ids:
            raise ValueError("Unresolved scene or source")
        if scene.reviewed_all_categories and event.verification != "primary_verified":
            raise ValueError("A reviewed scene cannot contain unresolved candidates")
        if event.end_seconds is not None:
            if scene.runtime_seconds is None or event.end_seconds > scene.runtime_seconds:
                raise ValueError("Event interval exceeds scene runtime")
        if event.word_end is not None:
            if scene.words is None or event.word_end > scene.words:
                raise ValueError("Event interval exceeds scene word count")


def deduplicate_events(events: list[Event]):
    """Merge references for an adjudicated shared ID; conflicting annotations fail closed.

    Similar-looking encounters are not automatically merged: use a reviewer to assign
    the same event ID when multiple sources describe the same occurrence.
    """
    merged = {}
    for event in events:
        if event.event_id in merged:
            prior = merged[event.event_id]
            if prior.model_dump(exclude={"source_ids"}) != event.model_dump(exclude={"source_ids"}):
                raise ValueError("Conflicting annotations for shared event ID")
            event = event.model_copy(
                update={"source_ids": sorted(set(prior.source_ids + event.source_ids))}
            )
        merged[event.event_id] = event
    return list(merged.values())
