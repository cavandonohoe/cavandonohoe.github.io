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
        def commit(sha, stamp, name, email, numstat, committer="Cavan Donohoe|cavandonohoe@gmail.com", subject="Change"):
            return f"\x1e{sha}|{stamp}|{name}|{email}|{committer}|{subject}\0\n" + "".join(f"{line}\0" for line in numstat)
        log = (
            commit("abc", "2025-12-31T23:30:00-08:00", "Cavan Donohoe", "cavandonohoe@gmail.com", ["3\t0\tapp.R"])
            + commit("def", "2026-01-01T00:00:00+09:00", "cavandonohoe", "cavandonohoe@users.noreply.github.com", ["4\t2\tapp.py", "-\t-\timage.png"])
            + commit("abc", "2025-12-31T23:30:00-08:00", "Cavan Donohoe", "cavandonohoe@gmail.com", ["3\t0\tapp.R"])
            + commit("bot", "2026-01-01T00:00:00Z", "github-actions[bot]", "bot@example.com", ["99\t0\tapp.R"])
            + commit("dep", "2026-02-01T00:00:00Z", "cavandonohoe", "cavandonohoe@users.noreply.github.com", ["9999\t0\tindex.html"], subject="deploy: 1234abcd")
            + commit("bcm", "2026-02-02T00:00:00Z", "Cavan Donohoe", "cavandonohoe@gmail.com", ["50\t0\tapp.R"], committer="github-actions[bot]|41898282+github-actions[bot]@users.noreply.github.com")
        )
        calls = []
        def fixture_git(repo, *args):
            calls.append(args)
            if args[:2] == ("rev-parse", "--is-shallow-repository"):
                return "false\n"
            if args[0] == "rev-parse":
                return "0123abcd\n"
            if args[0] == "log":
                return log
            if args[0] == "check-attr":
                return ""
            raise AssertionError(args)
        with patch.object(review, "git", side_effect=fixture_git):
            rows = list(review.read_history("fixture"))
        self.assertEqual(len(rows), 2)
        log_args = next(args for args in calls if args[0] == "log")
        self.assertNotIn("--all", log_args)
        self.assertIn("0123abcd", log_args)
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
        # Daily activity uses UTC days, so each commit lands in the year it is counted in.
        self.assertEqual(previous["daily_activity"], [{"date": "2025-12-31", "commits": 1, "lines_added": 4}])
        self.assertEqual(current["daily_activity"], [{"date": "2026-01-01", "commits": 1, "lines_added": 3}])

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
                "data-raw/download.html": "<p>Downloaded</p>\n" * 100,
                "data-raw/prepare.R": "x <- 1\n",
                "renv/activate.R": "# Generated bootstrap\n" * 100,
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
            self.assertEqual(result["language_lines_added"], {"JavaScript": 8, "R": 6})
            self.assertEqual(result["excluded_language_lines_added"], {"JavaScript": 600, "HTML": 300, "R": 100})
            self.assertEqual(result["lines_added"], 1020)
            files = result["language_files"]["JavaScript"]
            self.assertEqual(sum(file["lines_added"] for file in files), 8)
            self.assertEqual({file["path"] for file in files}, {"app.js", "_archive/wc-momentum/main.js", "scripts/tohs/contact_workflow.js", "explicit/app.js"})
            for file in files:
                self.assertEqual(sum(commit["lines_added"] for commit in file["commits"]), file["lines_added"])
                self.assertEqual(file["repository"], "cavandonohoe/project")
                self.assertEqual(len(file["commits"][0]["sha"]), 40)

    def test_file_attribution_separates_repositories_and_calendar_years(self):
        records = [
            ("cavandonohoe/one", {"sha": "a", "day": date(2026, 1, 1), "added": 3, "languages": {"R": 3}, "language_files": {"R": {"app.R": 3}}}),
            ("cavandonohoe/one", {"sha": "b", "day": date(2026, 2, 1), "added": 2, "languages": {"R": 2}, "language_files": {"R": {"app.R": 2}}}),
            ("cavandonohoe/two", {"sha": "c", "day": date(2026, 1, 1), "added": 7, "languages": {"R": 7}, "language_files": {"R": {"app.R": 7}}}),
            ("cavandonohoe/one", {"sha": "d", "day": date(2025, 1, 1), "added": 100, "languages": {"R": 100}, "language_files": {"R": {"app.R": 100}}}),
        ]
        result = review.snapshot(2026, records, datetime(2026, 10, 7, tzinfo=timezone.utc))
        files = result["language_files"]["R"]
        self.assertEqual([file["lines_added"] for file in files], [7, 5])
        self.assertEqual([commit["sha"] for commit in files[1]["commits"]], ["b", "a"])
        self.assertEqual(sum(file["lines_added"] for file in files), result["language_lines_added"]["R"])

    def test_attribute_failure_does_not_publish_unfiltered_totals(self):
        with patch.object(review, "git", side_effect=subprocess.CalledProcessError(1, "git")):
            with self.assertRaises(RuntimeError):
                review.excluded_from_language_stats("fixture", "app.js")

    def test_site_attribute_rules_keep_original_javascript(self):
        with tempfile.TemporaryDirectory() as tmp:
            repo = Path(tmp)
            review.git(repo, "init", "-q")
            review.git(repo, "config", "user.name", "Fixture")
            review.git(repo, "config", "user.email", "fixture@example.com")
            rules = Path(__file__).resolve().parents[1] / ".gitattributes"
            (repo / ".gitattributes").write_text(rules.read_text())
            review.git(repo, "add", ".gitattributes")
            review.git(repo, "commit", "-qm", "Site language rules")
            for path in ["index.html", "archive/sp500.html", "blog/about_me_blog.html", "cv_stuff/cv.html", "_site/index.html", "pr-215/detour-finder/app.js", "wc-momentum/vendor/three.module.js", "_archive/wc-momentum/vendor/three.module.js", "site_libs/jquery/jquery.js"]:
                with self.subTest(path=path):
                    self.assertTrue(review.excluded_from_language_stats(repo, path))
            for path in ["detour-finder/app.js", "wc-momentum/main.js", "_archive/wc-momentum/main.js", "scripts/tohs/contact_workflow.js", "tests/tohs-contact-workflow.test.cjs", "movie-ranker/next.config.js", "data-raw/prepare.js"]:
                with self.subTest(path=path):
                    self.assertFalse(review.excluded_from_language_stats(repo, path))

    def test_streak_deduplicates_days_and_spans_year_end(self):
        self.assertEqual(review.longest_streak([date(2025, 12, 30), date(2025, 12, 31), date(2025, 12, 31), date(2026, 1, 1)]), 3)
        self.assertEqual(review.longest_streak([]), 0)

    def _repo(self, tmp):
        repo = Path(tmp) / "source"
        repo.mkdir()
        review.git(repo, "init", "-q", "-b", "main")
        review.git(repo, "config", "user.name", "Cavan Donohoe")
        review.git(repo, "config", "user.email", "cavandonohoe@example.com")
        return repo

    def _commit(self, repo, files, message):
        for path, content in files.items():
            target = repo / path
            target.parent.mkdir(parents=True, exist_ok=True)
            target.write_text(content)
        review.git(repo, "add", "-A")
        review.git(repo, "commit", "-qm", message)

    def test_only_default_branch_counts_and_deploys_are_skipped(self):
        with tempfile.TemporaryDirectory() as tmp:
            repo = self._repo(tmp)
            self._commit(repo, {"app.R": "x <- 1\n" * 2}, "Add app")
            review.git(repo, "checkout", "-q", "-b", "feature")
            self._commit(repo, {"unmerged.R": "y <- 1\n" * 50}, "Unmerged work")
            review.git(repo, "checkout", "-q", "--orphan", "gh-pages")
            self._commit(repo, {"index.html": "<p>site</p>\n" * 500}, "deploy: abc123")
            review.git(repo, "checkout", "-q", "main")
            self._commit(repo, {"deployed.html": "<p>x</p>\n" * 70}, "deploy: copied into main")
            clone = Path(tmp) / "clone"
            subprocess.run(["git", "clone", "--quiet", "--no-checkout", str(repo), str(clone)], check=True)
            rows = list(review.read_history(clone, "main"))
            self.assertEqual([row["added"] for row in rows], [2])
            # origin/HEAD fallback when the API branch is unavailable.
            self.assertEqual(len(list(review.read_history(clone))), 1)

    def test_renames_count_changed_lines_on_new_non_ascii_path(self):
        with tempfile.TemporaryDirectory() as tmp:
            repo = self._repo(tmp)
            self._commit(repo, {"old café.R": "".join(f"x{i} <- {i}\n" for i in range(20))}, "Add")
            (repo / "dir").mkdir()
            review.git(repo, "mv", "old café.R", "dir/new café.R")
            with open(repo / "dir/new café.R", "a") as handle:
                handle.write("y <- 2\n")
            review.git(repo, "commit", "-qam", "Move")
            rows = list(review.read_history(repo, "main"))
            moved = rows[0]
            self.assertEqual(moved["added"], 1)
            self.assertEqual(moved["language_files"], {"R": {"dir/new café.R": 1}})
            self.assertEqual(rows[1]["language_files"], {"R": {"old café.R": 20}})

    def test_built_in_generated_patterns_and_attribute_override(self):
        with tempfile.TemporaryDirectory() as tmp:
            repo = self._repo(tmp)
            self._commit(repo, {".gitattributes": "dist/keep.js linguist-generated=false\n"}, "Rules")
            for path in ["lib/jquery.min.js", "css/site-min.css", "app.bundle.js", "dist/app.js", "renv/library/R-4.4/pkg/R/x.R", "docs/site.js", "_freeze/a.js", "bower_components/x/x.js"]:
                with self.subTest(path=path):
                    self.assertTrue(review.excluded_from_language_stats(repo, path))
            for path in ["dist/keep.js", "src/minimal.js", "renv_helpers.R", "R/library.R"]:
                with self.subTest(path=path):
                    self.assertFalse(review.excluded_from_language_stats(repo, path))

    def test_ties_break_deterministically(self):
        records = [
            ("cavandonohoe/zeta", {"sha": "a", "day": date(2026, 3, 1), "added": 5, "languages": {"R": 5}, "language_files": {}}),
            ("cavandonohoe/alpha", {"sha": "b", "day": date(2026, 2, 1), "added": 5, "languages": {"Python": 5}, "language_files": {}}),
        ]
        result = review.snapshot(2026, records, datetime(2026, 10, 7, tzinfo=timezone.utc))
        self.assertEqual(result["biggest_project"], "alpha")
        self.assertEqual(result["most_used_language"], "Python")
        self.assertEqual(result["most_active_month"], "February")
        self.assertEqual(list(result["repository_commits"]), ["cavandonohoe/alpha", "cavandonohoe/zeta"])

    def test_snapshot_written_only_when_data_changes(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "2026.json"
            out = {"year": 2026, "commits": 3, "generated_at": "2026-10-01T00:00:00+00:00"}
            self.assertTrue(review.write_snapshot(path, out))
            self.assertFalse(review.write_snapshot(path, {**out, "generated_at": "2026-10-02T00:00:00+00:00"}))
            self.assertIn("2026-10-01", path.read_text())
            self.assertTrue(review.write_snapshot(path, {**out, "commits": 4}))

    def test_stale_partial_years_are_refinalized(self):
        with tempfile.TemporaryDirectory() as tmp:
            out = Path(tmp)
            (out / "2024.json").write_text('{"is_partial_year": false}')
            (out / "2025.json").write_text('{"is_partial_year": true}')
            (out / "2026.json").write_text('{"is_partial_year": true}')
            now = datetime(2026, 1, 2, tzinfo=timezone.utc)
            self.assertEqual(review.stale_partial_years(out, now), [2025])


if __name__ == "__main__":
    unittest.main()
