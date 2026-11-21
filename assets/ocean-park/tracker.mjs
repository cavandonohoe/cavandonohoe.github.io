import {
  clock,
  courts,
  estimatedGaps,
  dayOf,
  fresh,
  shift,
  state,
  validate,
} from "./model.mjs";
const el = (id) => document.getElementById(id);
const labels = {
  estimated: "Estimated unavailable",
  open: "No calendar gaps",
  active: "All-day permit",
  partial: "Part-day permit",
  clear: "No permits",
  unknown: "Unknown",
};
const symbols = { estimated: "≈", open: "○", active: "●", partial: "◐", clear: "✓", unknown: "?" };
const buttons = new Map();
let data = { days: {} },
  failure = false,
  selected;
function node(tag, text) {
  const n = document.createElement(tag);
  n.textContent = text;
  return n;
}
function stamp(iso) {
  return dayOf(Date.parse(iso)) + " · " + clock(Date.parse(iso));
}
function selectDay(day) {
  el("date").value = day;
  render();
}
function today() {
  selectDay(dayOf(Date.now()));
}
function courtOptions() {
  const before = el("court").value;
  el("court").replaceChildren(new Option("All courts", ""));
  for (const key of courts.filter(
    (key) => !el("area").value || key.startsWith(el("area").value),
  )) {
    el("court").add(new Option(key.replace("-", " "), key));
  }
  if ([...el("court").options].some((o) => o.value === before))
    el("court").value = before;
}
for (const area of ["North", "South"]) {
  const group = node("div", ""),
    grid = node("div", "");
  group.className = "group";
  grid.className = "grid";
  group.append(
    node("h3", "Ocean Park " + area),
    node(
      "small",
      area === "North" ? "100 Hollister Avenue" : "2600 Ocean Front Walk",
    ),
    grid,
  );
  for (const key of courts.filter((k) => k.startsWith(area))) {
    const b = node("button", key.split("-")[1]);
    grid.append(b);
    buttons.set(key, b);
    b.onclick = () => {
      selected = key;
      render();
      el("details").showModal();
    };
  }
  el("groups").append(group);
}
function details(entry, status, day, intervals) {
  el("detailTitle").textContent = selected.replace("-", " · Court ");
  const body = el("detailBody");
  body.replaceChildren(node("strong", labels[status]), node("p", day));
  if (!entry) {
    body.append(
      node(
        "p",
        "No verified permit source for this court and date. Last updated: unavailable.",
      ),
    );
    return;
  }
  body.append(
    node("p", "Source: " + entry.source),
    node("p", "Last updated: " + stamp(entry.checked)),
    node(
      "p",
      fresh(entry, day, Date.now())
        ? entry.complete
          ? "Complete daily court record."
          : "Partial record. Other permits may exist."
        : "Stale record. Confirm with the city.",
    ),
  );
  if (entry.calendar) {
    const link = node("a", "View city court calendar");
    link.href = entry.calendar.url;
    body.append(link, node("p", "Listed availability: " + (entry.calendar.available.map(w =>
      clock(Date.parse(w.start)) + " to " + clock(Date.parse(w.end))).join(", ") || "Fully booked")));
  }
  for (const p of intervals) {
    body.append(
      node(
        "p",
        clock(Date.parse(p.start)) +
          " to " +
          clock(Date.parse(p.end)) +
          " · " +
          (p.estimated ? "Estimated unavailable" : p.holder || "Holder unpublished"),
      ),
    );
  }
}
function render() {
  const day = el("date").value;
  if (!day) return;
  const now = Date.now();
  const entries = data.days[day] || {};
  el("notice").hidden = day < "2026-11-15" || day > "2026-11-25";
  const counts = { active: 0, clear: 0, unknown: 0, partial: 0, estimated: 0, open: 0 },
    rows = [],
    permitIds = new Set();
  for (const [key, b] of buttons) {
    const entry = entries[key];
    const gaps = estimatedGaps(entry, day);
    const stale = entry && !fresh(entry, day, now);
    let status = state(entry, day, now);
    if (entry?.calendar && !stale && !entry.permits.length) {
      status = gaps.length ? "estimated" : "open";
    }
    counts[status]++;
    b.className = status;
    const permits = [...(entry?.permits || []), ...gaps].sort(
      (a, b) => Date.parse(a.start) - Date.parse(b.start),
    );
    b.replaceChildren(node("strong", key.split("-")[1] + " " + symbols[status]));
    if (!permits.length || stale) {
      b.append(node("small", stale ? "Stale record" : labels[status]));
    }
    const times = permits.map(p => {
      const start = clock(Date.parse(p.start)), end = clock(Date.parse(p.end));
      // Show the offset on both ends when a permit crosses a DST transition.
      const compactStart = start.split(" ").at(-1) === end.split(" ").at(-1)
        ? start.slice(0, start.lastIndexOf(" ")) : start;
      return compactStart + " – " + end + (p.estimated ? " · Estimated" : " · Permit");
    });
    for (const range of times) b.append(node("small", range));
    b.setAttribute("aria-label", [key.replace("-", " court "), stale ? "Stale record" : labels[status], ...times].join(", "));
    if (selected === key) details(entry, status, day, permits);
    for (const p of permits) {
      const current = fresh(entry, day, now);
      if (current && !p.estimated) permitIds.add(p.id);
      rows.push({ key, p, status: !current ? "Stale" : p.estimated ? "Estimated unavailable" : "Permitted" });
    }
  }
  const n = Object.keys(entries).length;
  el("coverage").textContent = failure
    ? "Permit data could not be loaded. Coverage is unknown."
    : !n
      ? "No verified permit records for this date. All court statuses are unknown."
      : "Current evidence for " +
        Object.values(entries).filter((e) => fresh(e, day, now)).length +
        " of 25 reservable courts.";
  el("stats").replaceChildren();
  for (const [name, value] of [
    ["Courts", 25],
    ["Courts with permits", counts.active + counts.partial],
    ["Estimated only", counts.estimated],
    ["No calendar gaps", counts.open],
    ["No permits", counts.clear],
    ["Unknown", counts.unknown],
    ["Known daily permits", permitIds.size || (counts.clear === 25 ? 0 : "—")],
  ]) {
    const box = node("span", "");
    box.append(node("b", String(value)), name);
    el("stats").append(box);
  }
  const checks = Object.values(entries).map((e) => Date.parse(e.checked));
  el("updated").textContent = checks.length
    ? "Last updated for this date: " +
      stamp(new Date(Math.max(...checks)).toISOString())
    : "Last updated for this date: no verified snapshot.";
  const tbody = el("rows");
  tbody.replaceChildren();
  for (const { key, p, status } of rows.sort(
    (a, b) => Date.parse(a.p.start) - Date.parse(b.p.start),
  )) {
    const [area, court] = key.split("-"),
      start = Date.parse(p.start),
      end = Date.parse(p.end);
    if (
      (el("area").value && area !== el("area").value) ||
      (el("court").value && key !== el("court").value) ||
      (el("status").value && status !== el("status").value)
    )
      continue;
    const tr = node("tr", "");
    [
      court,
      area,
      clock(start),
      clock(end),
      p.estimated ? "Unknown" : p.holder || "Unpublished",
      status,
    ].forEach((v, i) => {
      const td = node("td", v);
      td.dataset.label = [
        "Court",
        "Location",
        "Start",
        "End",
        "Holder / event",
        "Status",
      ][i];
      tr.append(td);
    });
    tbody.append(tr);
  }
  el("empty").textContent = tbody.children.length
    ? ""
    : rows.length
      ? "No records match these filters."
      : "No unavailable hours on file for this date.";
}
courtOptions();
el("date").onchange = () => {
  if (el("date").value) selectDay(el("date").value);
  else today();
};
for (const [id, delta] of [
  ["prev", -1],
  ["next", 1],
])
  el(id).onclick = () => selectDay(shift(el("date").value, delta));
el("today").onclick = today;
el("close").onclick = () => el("details").close();
el("area").onchange = () => {
  courtOptions();
  render();
};
for (const id of ["status", "court"])
  el(id).oninput = render;
today();
try {
  const response = await fetch(new URL("./permits.json", import.meta.url), {
    cache: "no-cache",
  });
  if (!response.ok) throw new Error("HTTP " + response.status);
  data = validate(await response.json());
} catch {
  failure = true;
}
render();
setInterval(render, 60000);
