# Public derived data

- `chapters.csv`: 119 chapter units; GOT includes the prologue as C00. Chapter-order
  percentage is not cumulative word exposure. Empty word counts are missing values.
- `events.jsonl`: 11 **secondary-source event clusters**, all unscored. This is not a
  whole-book event inventory. One event can carry multiple provisional categories.
- `scenes.jsonl`: unresolved guide-cluster placeholders, all unreviewed. These are not
  a complete scene inventory and have no narrative word denominators.
- `sources.json`: URLs, access date, coverage, reliability, access status and rights notes.
- `coverage.json`: scope, book identities, inventory-completeness and calibration gates.

Original abstract annotations were produced by a single interactive assistant reviewing
public guide pages. There is no independent human or second-annotator validation. All
severity, span and calibrated confidence fields are null. The exact assistant model build
was not exposed; the committed annotation snapshot is the reproducibility input.

Do not promote a guide's statement of sexual activity into verified explicit prose.
Do not infer absence from a guide omitting an event. Do not infer more content in ACOTAR
from its larger number of pilot clusters. No source text is stored in these records.

Use ignored `private/` for lawful local texts, provenance, hashes, annotation queues and
model caches. Never stage those files. Use edition-specific word offsets within scenes,
represented as half-open [start, end) intervals, once the original text has been reviewed.
