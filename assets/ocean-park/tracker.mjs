import {
  clock,
  courts,
  dayOf,
  fresh,
  midnight,
  minute,
  sameTime,
  shift,
  state,
  validate,
} from "./model.mjs";
const el = (id) => document.getElementById(id);
const labels = {
  active: "Active permit",
  partial: "Part-day permit",
  clear: "No active permit",
  unknown: "Unknown",
};
const symbols = { active: "●", partial: "◐", clear: "✓", unknown: "?" };
const buttons = new Map();
let displayedDay;
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
function selectDay(day, elapsed) {
  displayedDay = day;
  el("date").value = day;
  el("slider").max = (midnight(shift(day, 1)) - midnight(day)) / minute - 1;
  el("slider").value = Math.min(elapsed, Number(el("slider").max));
  render();
}
function today() {
  const now = Date.now(),
    day = dayOf(now);
  selectDay(day, Math.floor((now - midnight(day)) / minute));
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
    b.append(node("small", ""));
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
function details(entry, status, day) {
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
  for (const p of [...entry.permits].sort(
    (a, b) => Date.parse(a.start) - Date.parse(b.start),
  )) {
    body.append(
      node(
        "p",
        clock(Date.parse(p.start)) +
          " to " +
          clock(Date.parse(p.end)) +
          " · " +
          (p.holder || "Holder unpublished"),
      ),
    );
  }
}
function render() {
  const day = el("date").value;
  if (!day) return;
  const now = Date.now(),
    at = midnight(day) + Number(el("slider").value) * minute;
  const allDay = el("mode").value === "day",
    entries = data.days[day] || {};
  el("clock").textContent = clock(at);
  el("slider").setAttribute("aria-valuetext", clock(at));
  el("notice").hidden =
    day < "2026-11-15" ||
    day > "2026-11-25" ||
    (day === "2026-11-15" &&
      !allDay &&
      at < Date.parse("2026-11-15T15:00:00-08:00"));
  const counts = { active: 0, clear: 0, unknown: 0, partial: 0 },
    rows = [],
    permitIds = new Set();
  for (const [key, b] of buttons) {
    const entry = entries[key],
      status = state(entry, day, at, allDay, now);
    counts[status]++;
    b.className = status;
    b.lastChild.textContent = symbols[status];
    b.setAttribute(
      "aria-label",
      key.replace("-", " court ") + ", " + labels[status],
    );
    b.title = labels[status];
    if (selected === key) details(entry, status, day);
    for (const p of entry?.permits || []) {
      const current = fresh(entry, day, now);
      if (current) permitIds.add(p.id);
      rows.push({ key, p, status: current ? "Permitted" : "Stale" });
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
    [
      allDay ? "With permits" : "Active permits (courts)",
      counts.active + counts.partial,
    ],
    ["No active permit", counts.clear],
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
  const noon = midnight(shift(day, 1)) - 12 * 60 * minute;
  for (const { key, p, status } of rows.sort(
    (a, b) => Date.parse(a.p.start) - Date.parse(b.p.start),
  )) {
    const [area, court] = key.split("-"),
      start = Date.parse(p.start),
      end = Date.parse(p.end),
      period = el("period").value;
    if (
      (el("area").value && area !== el("area").value) ||
      (el("court").value && key !== el("court").value) ||
      (el("status").value && status !== el("status").value) ||
      (period === "selected" && !(start <= at && at < end)) ||
      (period === "am" && start >= noon) ||
      (period === "pm" && end <= noon)
    )
      continue;
    const tr = node("tr", "");
    [
      court,
      area,
      clock(start),
      clock(end),
      p.holder || "Unpublished",
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
      : "No permit entries on file for this date.";
}
courtOptions();
function navigate(day) {
  selectDay(day, sameTime(displayedDay, +el("slider").value, day));
}
el("date").onchange = () => {
  if (el("date").value) navigate(el("date").value);
  else today();
};
for (const [id, delta] of [
  ["prev", -1],
  ["next", 1],
])
  el(id).onclick = () => navigate(shift(el("date").value, delta));
el("today").onclick = today;
el("close").onclick = () => el("details").close();
el("area").onchange = () => {
  courtOptions();
  render();
};
for (const id of ["slider", "mode", "period", "status", "court"])
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
