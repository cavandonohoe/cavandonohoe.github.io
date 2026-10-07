"""Regression tests for archive attribution and calendar boundaries."""
import importlib.util
import subprocess
import tempfile
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
        def fixture_git(repo, *args):
            if args[0] == "rev-parse":
                return "false\n"
            if args[0] == "log":
                return log
            if args[0] == "check-attr":
                return ""
            raise AssertionError(args)
        with patch.object(review, "git", side_effect=fixture_git):
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

    def test_language_exclusions_work_without_checkout_and_keep_first_party_js(self):
        with tempfile.TemporaryDirectory() as tmp:
            source = Path(tmp) / "source"
            source.mkdir()
            review.git(source, "init", "-q")
            review.git(source, "config", "user.name", "Cavan Donohoe")
            review.git(source, "config", "user.email", "cavandonohoe@example.com")
            files = {
                "app.R": "x <- 1\n" * 5,
                "app.js": "const x = 1;\n" * 3,
                "_archive/wc-momentum/main.js": "const scene = {};\n" * 2,
                "scripts/tohs/contact_workflow.js": "const contacts = [];\n" * 2,
                "generated/output.js": "const generated = true;\n" * 100,
                "marked/library.js": "const library = true;\n" * 100,
                "vendor/three.js": "const library = true;\n" * 100,
                "site_libs/widget.js": "const widget = true;\n" * 100,
                "nested/node_modules/package/index.js": "const dependency = true;\n" * 100,
                "explicit/app.js": "const authored = true;\n",
                "pr-215/app.js": "const deployed = true;\n" * 100,
                "_site/index.html": "<p>Rendered</p>\n" * 100,
                "index.html": "<p>Rendered</p>\n" * 100,
            }
            for path, content in files.items():
                target = source / path
                target.parent.mkdir(parents=True, exist_ok=True)
                target.write_text(content)
            review.git(source, "add", ".")
            review.git(source, "commit", "-qm", "Add sources and dependencies")
            # New rules must also classify past additions to now-deleted files.
            (source / "generated/output.js").unlink()
            (source / ".gitattributes").write_text(
                "generated/** linguist-generated=true\n"
                "marked/** linguist-vendored\n"
                "explicit/** linguist-generated=false linguist-vendored=false\n"
                "pr-*/** linguist-generated\n"
                "_site/** linguist-generated\n"
                "/*.html linguist-generated\n"
            )
            review.git(source, "add", ".")
            review.git(source, "commit", "-qm", "Classify generated and vendored files")
            clone = Path(tmp) / "clone"
            subprocess.run(["git", "clone", "--quiet", "--no-checkout", str(source), str(clone)], check=True)
            self.assertFalse((clone / ".gitattributes").exists())
            rows = list(review.read_history(clone))
            now = datetime.now(timezone.utc)
            result = review.snapshot(now.year, [("cavandonohoe/project", row) for row in rows], now)
            self.assertEqual(result["language_lines_added"], {"JavaScript": 8, "R": 5})
            self.assertEqual(result["excluded_language_lines_added"], {"JavaScript": 600, "HTML": 200})
            self.assertEqual(result["lines_added"], 819)

    def test_attribute_failure_does_not_publish_unfiltered_totals(self):
        with patch.object(review, "git", side_effect=subprocess.CalledProcessError(1, "git")):
            with self.assertRaises(RuntimeError):
                review.excluded_from_language_stats("fixture", "app.js")

    def test_streak_deduplicates_days_and_spans_year_end(self):
        self.assertEqual(review.longest_streak([date(2025, 12, 30), date(2025, 12, 31), date(2025, 12, 31), date(2026, 1, 1)]), 3)
        self.assertEqual(review.longest_streak([]), 0)


if __name__ == "__main__":
    unittest.main()
