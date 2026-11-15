#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
hook="$repo_root/.githooks/pre-push"
zero="0000000000000000000000000000000000000000"

bash -n "$hook"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

cp "$hook" "$tmp/pre-push"
chmod +x "$tmp/pre-push"

run_hook() {
  SKIP_TYPOS=1 SKIP_LINT=1 SKIP_TESTS=1 SKIP_DESC=1 "$tmp/pre-push"
}

git -C "$tmp" init -q
git -C "$tmp" config user.email "hook-test@example.com"
git -C "$tmp" config user.name "Hook Test"

printf 'one\n' > "$tmp/file.txt"
git -C "$tmp" add file.txt
git -C "$tmp" commit -q -m "Initial commit"
git -C "$tmp" branch -M main
git -C "$tmp" update-ref refs/remotes/origin/main HEAD
main_sha="$(git -C "$tmp" rev-parse HEAD)"

printf 'two\n' >> "$tmp/file.txt"
git -C "$tmp" commit -q -am "Update main"
main_tip="$(git -C "$tmp" rev-parse HEAD)"
printf 'refs/heads/main %s refs/heads/main %s\n' "$main_tip" "$main_sha" | (cd "$tmp" && run_hook)

git -C "$tmp" checkout -q -b feature "$main_sha"
printf 'feature\n' > "$tmp/feature.txt"
git -C "$tmp" add feature.txt
git -C "$tmp" commit -q -m "Feature commit"
feature_tip="$(git -C "$tmp" rev-parse HEAD)"
printf 'refs/heads/feature %s refs/heads/feature %s\n' "$feature_tip" "$zero" | (cd "$tmp" && run_hook)

printf 'refs/heads/old %s refs/heads/old %s\n' "$zero" "$main_tip" | (cd "$tmp" && "$tmp/pre-push")
(cd "$tmp" && run_hook < /dev/null)
(cd "$tmp" && SKIP_PRE_PUSH=1 "$tmp/pre-push" < /dev/null)
(cd "$tmp" && SKIP_HOOKS=1 "$tmp/pre-push" < /dev/null)

echo "pre-push hook smoke tests passed."
