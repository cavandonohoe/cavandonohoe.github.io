#!/usr/bin/env bash
# Commit generated outputs and push them to main, retrying when another
# writer pushed first. On a rebase conflict the outputs are regenerated from
# the latest main instead of being merged by hand.
#
# Usage:
#   scripts/push_generated_changes.sh "<commit message>" "<regenerate command>" <path>...
#
# Writes pushed=true|false to $GITHUB_OUTPUT so callers can dispatch a site
# deploy only when a commit actually landed.
#
# The body lives in a function so bash parses it fully before
# `git reset --hard` can rewrite this file on disk.
set -euo pipefail

main() {
  local msg=$1 regen=$2
  shift 2
  local paths=("$@")
  local out=${GITHUB_OUTPUT:-/dev/null}

  git config user.name "github-actions[bot]"
  git config user.email "41898282+github-actions[bot]@users.noreply.github.com"

  git add -A -- "${paths[@]}"
  if git diff --cached --quiet; then
    echo "No generated changes to commit."
    echo "pushed=false" >> "$out"
    return 0
  fi
  git commit -m "$msg"

  local attempt
  for attempt in 1 2 3; do
    git fetch origin main
    if ! git rebase origin/main; then
      echo "Rebase conflicted on generated outputs; regenerating from latest main..."
      git rebase --abort || true
      git reset --hard origin/main
      bash -c "$regen"
      git add -A -- "${paths[@]}"
      if git diff --cached --quiet; then
        echo "Latest main already contains equivalent output."
        echo "pushed=false" >> "$out"
        return 0
      fi
      git commit -m "$msg"
    fi

    if git push origin HEAD:main; then
      echo "pushed=true" >> "$out"
      return 0
    fi

    echo "Push raced with another writer; retrying ($attempt/3)..."
    sleep $((attempt * 2))
  done

  echo "ERROR: failed to push generated changes after 3 attempts." >&2
  return 1
}

main "$@"
exit $?
