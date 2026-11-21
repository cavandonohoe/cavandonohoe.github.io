export const zone = "America/Los_Angeles";
const dayFormatter = new Intl.DateTimeFormat("en-CA", {
  timeZone: zone,
  year: "numeric",
  month: "2-digit",
  day: "2-digit",
});
const timeFormatter = new Intl.DateTimeFormat("en-US", {
  timeZone: zone,
  hour: "numeric",
  minute: "2-digit",
  timeZoneName: "short",
});
export const dayOf = (ms) => dayFormatter.format(ms);
export const clock = (ms) => timeFormatter.format(ms);
export const minute = 60000;
export const courts = [
  ...Array.from({ length: 16 }, (_, i) => "North-" + (i + 1)),
  ...Array.from("ABCDEFGHI", (letter) => "South-" + letter),
];
export function shift(day, delta) {
  const d = new Date(day + "T12:00:00Z");
  d.setUTCDate(d.getUTCDate() + delta);
  return d.toISOString().slice(0, 10);
}
// Pacific midnight is 07:00 or 08:00 UTC, including on DST transition dates.
export function midnight(day) {
  const seven = Date.parse(day + "T07:00:00Z");
  return dayOf(seven) === day ? seven : seven + 60 * minute;
}
export function fresh(entry, day, now) {
  if (!entry) return false;
  const checked = Date.parse(entry.checked);
  return (
    checked <= now &&
    (now - checked < 24 * 60 * minute || checked >= midnight(shift(day, 1)))
  );
}
const instant = (s) =>
  typeof s === "string" &&
  /^\d{4}-\d\d-\d\dT\d\d:\d\d(?::\d\d(?:\.\d{1,3})?)?(?:Z|[+-]\d\d:\d\d)$/.test(
    s,
  ) &&
  Number.isFinite(Date.parse(s));
function requireField(ok, message) {
  if (!ok) throw new Error(message);
}
function object(value) {
  return value !== null && typeof value === "object" && !Array.isArray(value);
}
export function validate(data) {
  requireField(object(data) && object(data.days), "Missing days");
  for (const [day, entries] of Object.entries(data.days)) {
    requireField(
      /^\d{4}-\d\d-\d\d$/.test(day) && shift(day, 0) === day && object(entries),
      "Invalid day",
    );
    for (const [key, e] of Object.entries(entries)) {
      requireField(
        courts.includes(key) &&
          object(e) &&
          instant(e.checked) &&
          typeof e.complete === "boolean" &&
          typeof e.source === "string" &&
          e.source.trim() &&
          Array.isArray(e.permits),
        "Invalid evidence for " + key,
      );
      const seen = new Set();
      for (const p of e.permits) {
        requireField(
          object(p) &&
            typeof p.id === "string" &&
            p.id.trim() &&
            instant(p.start) &&
            instant(p.end) &&
            (p.holder == null || typeof p.holder === "string"),
          "Invalid permit",
        );
        const start = Date.parse(p.start),
          end = Date.parse(p.end);
        requireField(
          midnight(day) <= start &&
            start < end &&
            end <= midnight(shift(day, 1)),
          "Outside selected day",
        );
        const slot = JSON.stringify([p.id, start, end]);
        requireField(!seen.has(slot), "Duplicate permit interval");
        seen.add(slot);
      }
    }
  }
  return data;
}
export function state(entry, day, now) {
  if (!fresh(entry, day, now)) return "unknown";
  const intervals = entry.permits
    .map((p) => [Date.parse(p.start), Date.parse(p.end)])
    .sort((a, b) => a[0] - b[0]);
  if (intervals.length) {
    let through = midnight(day);
    for (const [start, end] of intervals) {
      if (start > through) break;
      through = Math.max(through, end);
    }
    return through === midnight(shift(day, 1)) ? "active" : "partial";
  }
  return entry.complete ? "clear" : "unknown";
}
