#!/usr/bin/env bash
# Post or update a single "sticky" comment on a pull request, keyed by a
# hidden HTML marker. Replaces thollander/actions-comment-pull-request so we
# don't depend on a Node 20 action (deprecated on GitHub Actions runners).
#
# Required env:
#   GH_TOKEN    - token with pull-requests: write
#   PR_NUMBER   - pull request number
#   STICKY_TAG  - unique tag identifying this comment (e.g. "repo-size")
# Body source (exactly one):
#   BODY_FILE   - path to a file whose contents become the comment body, or
#   BODY        - literal comment body string
set -euo pipefail

: "${GH_TOKEN:?GH_TOKEN is required}"
: "${PR_NUMBER:?PR_NUMBER is required}"
: "${STICKY_TAG:?STICKY_TAG is required}"

marker="<!-- sticky-comment: ${STICKY_TAG} -->"

if [ -n "${BODY_FILE:-}" ]; then
  body_content="$(cat "$BODY_FILE")"
elif [ -n "${BODY:-}" ]; then
  body_content="$BODY"
else
  echo "Either BODY_FILE or BODY must be set" >&2
  exit 1
fi

full_body="${marker}"$'\n'"${body_content}"

repo="${GITHUB_REPOSITORY:?GITHUB_REPOSITORY is required}"

# Find an existing sticky comment with our marker (paginate all comments).
existing_id="$(
  gh api --paginate \
    "repos/${repo}/issues/${PR_NUMBER}/comments" \
    --jq ".[] | select(.body | contains(\"${marker}\")) | .id" \
    | head -n 1
)"

if [ -n "$existing_id" ]; then
  echo "Updating existing sticky comment ${existing_id} (tag: ${STICKY_TAG})"
  gh api --method PATCH \
    "repos/${repo}/issues/comments/${existing_id}" \
    -f body="$full_body" >/dev/null
else
  echo "Creating new sticky comment (tag: ${STICKY_TAG})"
  gh api --method POST \
    "repos/${repo}/issues/${PR_NUMBER}/comments" \
    -f body="$full_body" >/dev/null
fi
