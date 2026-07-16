#!/usr/bin/env bash
#
# Enable the repo's tracked git hooks (see .githooks/).
#
# Run once after cloning:
#   ./scripts/install-git-hooks.sh
#
# This points git at the tracked .githooks/ directory instead of the
# per-clone .git/hooks/, so the hooks are versioned and shared. Undo with:
#   git config --unset core.hooksPath

set -euo pipefail

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

chmod +x .githooks/* 2>/dev/null || true
git config core.hooksPath .githooks

echo "Enabled git hooks from .githooks/ (core.hooksPath set)."
echo "Bypass any run with:  git push --no-verify"
