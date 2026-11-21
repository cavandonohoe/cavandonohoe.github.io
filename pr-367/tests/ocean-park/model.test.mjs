import test from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import {
  midnight,
  shift,
  clock,
  minute,
  sameTime,
  courts,
  dayOf,
  validate,
  state,
} from "../../assets/ocean-park/model.mjs";
const day = "2026-06-10",
  start = midnight(day),
  now = start + 10 * 60 * minute;
const time = (h) => new Date(start + h * 60 * minute).toISOString();
const record = {
  checked: time(9),
  complete: false,
  source: "Test fixture",
  permits: [
    { id: "one", start: time(9), end: time(11) },
    { id: "two", start: time(10), end: time(12) },
  ],
};
test("inventory and published snapshot", () => {
  assert.equal(courts.length, 25);
  assert.ok(courts.includes("South-I"));
  assert.ok(!courts.includes("South-1"));
  validate(
    JSON.parse(
      readFileSync(
        new URL("../../assets/ocean-park/permits.json", import.meta.url),
      ),
    ),
  );
});
test("partial, complete, missing, stale and overlapping evidence", () => {
  assert.equal(state(undefined, day, now, false, now), "unknown");
  assert.equal(state(record, day, now, false, now), "active");
  assert.equal(state(record, day, Date.parse(time(12)), false, now), "unknown");
  assert.equal(
    state({ ...record, complete: true }, day, Date.parse(time(12)), false, now),
    "clear",
  );
  assert.equal(state(record, day, now, true, now), "partial");
  assert.equal(
    state(record, day, now, false, now + 24 * 60 * minute),
    "unknown",
  );
  assert.equal(
    state(
      { ...record, permits: [{ id: "one", start: time(0), end: time(24) }] },
      day,
      now,
      true,
      now,
    ),
    "active",
  );
});
test("Pacific day boundaries and both occurrences of fall 1 AM", () => {
  for (const [d, h] of [
    ["2026-03-08", 23],
    ["2026-11-01", 25],
  ]) {
    assert.equal((midnight(shift(d, 1)) - midnight(d)) / minute, h * 60);
    assert.equal(dayOf(midnight(d) - 1), shift(d, -1));
  }
  const fall = midnight("2026-11-01");
  assert.notEqual(clock(fall + 90 * minute), clock(fall + 150 * minute));
  assert.equal(sameTime("2026-03-07", 150, "2026-03-08"), 120);
  assert.equal(sameTime("2026-11-02", 90, "2026-11-01"), 150);
});
test("refuse unmapped courts, duplicate intervals and invalid coverage", () => {
  const pack = (key, e) => ({ days: { [day]: { [key]: e } } });
  validate(pack("North-1", record));
  assert.throws(() => validate(pack("South-1", record)));
  assert.throws(() =>
    validate(pack("North-1", { ...record, complete: "yes" })),
  );
  assert.throws(() =>
    validate(
      pack("North-1", {
        ...record,
        permits: record.permits.concat(record.permits),
      }),
    ),
  );
});
