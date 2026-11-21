# Ocean Park volleyball permits

The Projects menu opens `ocean_park_volleyball.html`. The static page and its relative assets work at both the root and a PR preview subpath. No backend, accounts or third-party browser requests are needed to view the tracker.

## Source investigation, November 21, 2026

- The [city's beach court guide](https://www.santamonica.gov/process-explainers/how-to-reserve-a-volleyball-court-or-beach-soccer-court) lists 16 North and 9 South reservable courts, and separately 8 first-come North Players’ Courts. Its online booking instructions concern Annenberg. Ocean Park requests go through the beach permits office. The November sand-removal closure is a facility notice, never a permit.
- The [public ActiveNet directory](https://anc.apm.activecommunities.com/santamonicarecreation/reservation/landing/search?keyword=Ocean%20Park&resourceType=0&equipmentQty=0) independently identifies North VB#1 through VB#16 and South VB#A through VB#I. The [South A detail](https://anc.apm.activecommunities.com/santamonicarecreation/reservation/landing/search/detail/179) offers an availability check and identifies the facility as unavailable for online reservation. That check cannot establish the cause of a conflict. The eight Players’ Courts have no verified individual labels in this directory. They are described separately from the 25 permit tiles. From south to north, the North group uses the player-reported rows 1–2, 3–5, 6–8, 9–11, 12–14 and 15–16. Pole coordinates and South court positions remain unverified.
- Searched GitHub repository names/descriptions for Santa Monica volleyball and volleyball permits, and indexed code for the tenant slug, Ocean Park North, South-VB, court permits and the city's permit-office address. Checked web and GitHub Pages searches, local player discussions, the [community petition](https://form.jotform.com/232546083995164), league/event listings and the city open-data search. [pickupvb.com](https://github.com/cooler09/pickupvb.com) indexes Meetup events. [PIC16B Final Project](https://github.com/Lehan77/PIC16B_Final_Project) lists venues. [seattle-parks](https://github.com/coryking/seattle-parks) references the Santa Monica tenant. None yielded a verified Ocean Park issued-permit schedule. The subsequently supplied live URL identified the community tracker described below. No third-party project code was used.
- Could not verify a public roster of issued permits, public holder names, historical archive, future coverage, cancellation notifications, CSV, calendar subscription or documented API for either group. [ACTIVE terms](https://www.activenetwork.com/information/terms-of-use) require prior written consent for scraping. The page links to the official directory. No scheduled ActiveNet retrieval is configured.

## Updating the schedule

Obtain a dated permit, schedule export or cancellation from the Beach Permits Office (beach.permits@santamonica.gov), with permission to publish the relevant details. Request court labels, effective dates, start/end times, public organization/event names, cancellations and explicit completeness for each court-day. Omit personal contact details. A league calendar, booking conflict or venue listing alone is insufficient.

Edit `assets/ocean-park/permits.json` on a branch. Its root is `{"days":{}}`. Each Pacific ISO date maps to an object keyed by the exact court identifier, such as `North-1` or `South-A`. Each court object has the following fields.

| Field | Meaning |
| --- | --- |
| checked | ISO timestamp with UTC offset or Z for the most recent verification of the whole court-day snapshot |
| source | Public source URL or traceable document title, issuer and issue date |
| complete | Boolean. Set true only when the source explicitly covers every permit for this court for the entire local date |
| permits | Array of objects with stable permit id, start and end ISO timestamps with offsets, and optional public holder or event name |

Use one stable id across all courts covered by the same permit. Split overnight records at Pacific midnight. Preserve distinct intervals including overlaps. Replace a court-day snapshot as a unit. Remove canceled intervals and replace modified times after reverifying the remaining entries. A single notice cannot refresh unrelated old rows. Keep uncertain court-days partial or omit them. An empty permits array with complete=true is a positive declaration of a verified empty daily schedule. Never create it from a blank search result.

Run `node --test tests/ocean-park/model.test.mjs` before publishing via a reviewed PR. On collection failure, leave the last verified file and timestamp intact. Freshness expires after 24 hours unless a retrospective check occurred after the selected day ended. The latter is treated as an archived observation. Stale intervals remain visible in details and the schedule with a warning. Corrections to an archive require a new reviewed snapshot. Missing, malformed or unreadable data fails closed to unknown. Intervals include their start and exclude their end. A positive interval can establish a permit on a partially covered day. Only a complete fresh snapshot can establish absence. The known daily permit count deduplicates permit identifiers across courts. A dash means the total is unknown.

Serve the repository root with `python -m http.server` and open `/ocean_park_volleyball.html` to review locally.

### Estimated unavailable hours

A reviewed court-day can also include a `calendar` with the city's court-specific `url` and the complete list of `available` start/end intervals shown for that date (expand “More”). Only use an empty array with explicit `fullyBooked: true` when the city displays “Fully booked”. Omit the calendar when the lookup fails or its results are incomplete. Review both sources before changing a shared snapshot timestamp.

The displayed estimates are gaps within an assumed 8 AM–9 PM Pacific comparison window. An early or late gap may reflect facility hours. The city calendar does not explain why a slot is unavailable. Known permit intervals appear separately and are excluded from the estimated intervals and counted once. No calendar gaps means coverage of this comparison window, without implying a clear permit record. November 21 observations cover North 1–6, 9 and 10.

### Community tracker identified

[Lion3334/OPcourts](https://github.com/Lion3334/OPcourts) links to [OP Court Availability](https://opcourts.vercel.app/). The Node collector reads ActiveNet availability, commits a rolling 14-day JSON snapshot twice daily via Actions, and Vercel serves a React/Vite frontend. The November 21 snapshot at 9:49 AM PST matched our eight North court checks. It includes North 1–16 and South A–H, omitting South I. Its daily window is based on the earliest and latest opening on each side, with default hours of 8 AM–9 PM North and 8 AM–7 PM South. The resulting gaps do not specify permit holders or reasons. The South calendar can also show openings during a city-announced closure. The repository README states MIT; permission for downstream republication of the ActiveNet-derived data was not established. Link to the community view and retain local permit evidence and closure notices.
