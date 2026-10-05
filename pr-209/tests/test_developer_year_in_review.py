"""Regression tests for archive attribution and calendar boundaries."""
import importlib.util
import unittest
from datetime import date, datetime, timezone
from pathlib import Path
from unittest.mock import patch

spec = importlib.util.spec_from_file_location("review", Path(__file__).resolve().parents[1] / "scripts/developer_year_in_review.py")
review = importlib.util.module_from_spec(spec)
spec.loader.exec_module(review)


class HistoryTests(unittest.TestCase):
    def test_author_aliases_exclude_bots_and_other_authors(self):
        self.assertTrue(review.authored_by_user("Cavan Donohoe", "old-address@example.com"))
        self.assertTrue(review.authored_by_user("Cavan", "59952579+cavandonohoe@users.noreply.github.com"))
        self.assertFalse(review.authored_by_user("github-actions[bot]", "github-actions[bot]@users.noreply.github.com"))
        self.assertFalse(review.authored_by_user("Other Person", "other@example.com"))

    def test_history_uses_utc_author_dates_and_deduplicates_branches(self):
        log = (
            "\x1eabc|2025-12-31T23:30:00-08:00|Cavan Donohoe|cavandonohoe@gmail.com\n3\t0\tapp.R\n"
            "\x1edef|2026-01-01T00:00:00+09:00|cavandonohoe|cavandonohoe@users.noreply.github.com\n4\t2\tapp.py\n-\t-\timage.png\n"
            "\x1eabc|2025-12-31T23:30:00-08:00|Cavan Donohoe|cavandonohoe@gmail.com\n3\t0\tapp.R\n"
            "\x1ebot|2026-01-01T00:00:00Z|github-actions[bot]|bot@example.com\n99\t0\tapp.R\n"
        )
        with patch.object(review, "git", side_effect=["false\n", log]):
            rows = list(review.read_history("fixture"))
        self.assertEqual(len(rows), 2)
        now = datetime(2026, 10, 5, tzinfo=timezone.utc)
        records = [("cavandonohoe/project", row) for row in rows]
        previous = review.snapshot(2025, records, now)
        current = review.snapshot(2026, records, now)
        self.assertEqual((previous["commits"], previous["lines_added"]), (1, 4))
        self.assertEqual((current["commits"], current["lines_added"]), (1, 3))
        self.assertEqual(current["most_used_language"], "R")
        self.assertTrue(current["is_partial_year"])
        self.assertFalse(previous["is_partial_year"])
        self.assertEqual(sum(current["monthly_commits"]), current["commits"])

    def test_shallow_history_is_rejected(self):
        with patch.object(review, "git", return_value="true\n"):
            with self.assertRaises(ValueError):
                list(review.read_history("fixture"))

    def test_streak_deduplicates_days_and_spans_year_end(self):
        self.assertEqual(review.longest_streak([date(2025, 12, 30), date(2025, 12, 31), date(2025, 12, 31), date(2026, 1, 1)]), 3)
        self.assertEqual(review.longest_streak([]), 0)


if __name__ == "__main__":
    unittest.main()
