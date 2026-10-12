# TOHS Apps Script: reviewed GitHub deployment

## Status and existing system

Infrastructure is installed **disabled**. No Google login, live script download,
script push, function execution, trigger edit, sharing change, or response/Sheet
write was performed during setup. The repository identifies the installed project
as **TOHS 2012 Contact Workflow**; its Script ID and live contents still require
owner verification. Drive metadata search did not expose the script project.
Do not substitute a workbook ID or create another Apps Script project.

The existing source remains in `scripts/tohs/contact_workflow.js` and
`scripts/tohs/acceptance_test.js`. There is no duplicate runtime source tree.
`tools/tohs-apps-script/project.json` maps repository files to the **existing
remote filenames** and will hold the exact live `appsscript.json` manifest after
linking. Its initial filenames are provisional, `verified` is false, and
`manifest` is null. A guessed manifest must never be deployed.

[PR #231](https://github.com/cavandonohoe/cavandonohoe.github.io/pull/231) was open
when this infrastructure was prepared. It fixes response replay, rejection,
organizer edits, ID validation, concurrent edits, and public-name privacy checks.
This is a separate infrastructure change against main; it does not silently ship
that PR's unmerged code. Review and merge #231 before the initial live comparison
and activation. Any newer live-only changes must also be reconciled through a PR.

The data flow is unchanged:

1. Google Form records responses in the private workbook.
2. Existing submission/hourly triggers run `refreshContactWorkflow()` and reconcile
   private contacts/review state.
3. Apps Script writes only the allowlisted public fields to the separate export
   workbook: first name, last name, email-presence boolean, and update timestamp.
4. `.github/workflows/update_tohs_reunion.yml` uses the existing read-only Google
   service account to refresh the public CSV; Pages renders that snapshot.

The new workflow only updates script source. Its OAuth credentials are separate
from `GCP_SA_KEY`. Never expand that service account's access to private contacts.
`clasp push` changes the project's current code used by installed triggers. It
does not create a versioned web-app deployment. Preserve any existing versioned
integrations; if one needs updating, plan it separately.

## One-time Google authorization and private comparison

Use Node 22 and run from the repository root, on a new linking branch after this
infrastructure and #231 have merged:

```sh
git switch -c infra/tohs-link-existing-script
cd tools/tohs-apps-script
npm ci --ignore-scripts
mkdir -p .local/live
chmod 700 .local
```

Open the existing **TOHS 2012 Contact Workflow** in the owning Google account at
[Apps Script](https://script.google.com/home). Confirm its title, source functions,
workbook references, Script Properties, existing triggers and trigger owners, and
integrations. Record that inventory privately. In Project Settings copy its
**Script ID**, not the deployment ID, spreadsheet ID, or Cloud project ID.
Enable the [Apps Script API for your account](https://script.google.com/home/usersettings).

Authenticate interactively on your own machine. Never paste credentials into a
chat, commit, PR, issue, or CI log. These commands use an explicit private auth
path and clone into an isolated directory, never over repository source:

```sh
./node_modules/.bin/clasp --auth .local/.clasprc.json login
chmod 600 .local/.clasprc.json
read -r TOHS_SCRIPT_ID  # enter the existing Script ID locally
cd .local/live
../../node_modules/.bin/clasp --auth ../.clasprc.json clone-script "$TOHS_SCRIPT_ID"
cd ../..
```

If Google policy requires a custom desktop OAuth client, follow the
[clasp authentication guide](https://github.com/google/clasp#bring-your-own-projectcredentials)
and use `login --creds` with its downloaded client JSON under `.local/`. This does
not require creating a new Apps Script project or changing its Cloud association.
For unattended use, check the OAuth consent app's publishing status: external
apps left in Testing can have short-lived refresh tokens. Reauthenticate privately
and replace the secret if Google revokes/expires it.

Keep `.local/live` as a private backup; copy it to secure owner-controlled storage
before the initial deployment. Never upload it as an Actions artifact. It can
contain sensitive code/configuration even if no Sheet rows were downloaded.
Inspect **all** files and the manifest locally, comparing the live workflow and
acceptance helper against their repository counterparts. For example, if the live
workflow is named `Code.js`:

```sh
# Run only in a private local terminal: the diff can contain sensitive contents.
git diff --no-index -- .local/live/Code.js ../../scripts/tohs/contact_workflow.js
```

A diff exits 1 when files differ; do not treat that as authorization to overwrite.
Reconcile every difference, retaining newer live fixes and all additional files.
Inspect for private contact values or embedded credentials before bringing any
live-only code into this public repository. Keep runtime secrets in Script
Properties. Do not commit raw responses, backups, properties, or private logs.

In the linking PR, adjust `project.json` file keys to the actual remote filenames
(e.g. `Code.js`, not an invented rename). Values point to reviewed files under
`scripts/tohs/`. Preserve every remote file, including acceptance helpers; the
preflight refuses any addition/deletion/rename or manifest difference. The current
inventory supports flat JavaScript/HTML filenames using letters, digits, `_` and
`-`. If the live project uses other names or nested paths, extend the validator
with tests in a reviewed PR rather than renaming the live files. For HTML, add a
specific `.gitignore` exception and confirm it is tracked.

After privately verifying the complete inventory, import the **exact** live
manifest and set the verification flag:

```sh
node - <<'JS'
const fs = require('node:fs');
const p = JSON.parse(fs.readFileSync('project.json', 'utf8'));
p.manifest = JSON.parse(fs.readFileSync('.local/live/appsscript.json', 'utf8'));
p.verified = true;
fs.writeFileSync('project.json', JSON.stringify(p, null, 2) + '\n');
JS
npm run validate
npm test
```

Review manifest content before committing; preserve timezone, V8 runtime, scopes,
advanced services, and integration settings exactly. If sensitive values appear
there, stop and plan a safe configuration change rather than publishing them.
Record a content-only initial baseline locally, bound to this exact reviewed
candidate (recompute after **any** source/manifest/mapping change):

```sh
node - <<'JS'
const fs = require('node:fs');
const {fingerprint, snapshot, bundle} = require('./manage.cjs');
fs.writeFileSync('.local/bootstrap.json', JSON.stringify({
  live: fingerprint(snapshot('.local/live')),
  target: fingerprint(bundle(undefined, true).files)
}), {mode: 0o600});
JS
```

Hashes include all names and source contents, normalize CRLF/trailing newlines,
and canonicalize manifest JSON. They include no contact records. The baseline is
not a substitute for inspecting differences: it records the owner's completed
comparison. No automated linking command calls `push`, an installer, or a refresh.

## One-time GitHub configuration (repository administrator)

Before enabling deployment:

1. Create environment **`tohs-apps-script-production`** in repository Settings →
   Environments; allow deployments from the `main` branch only, not tags. Optional
   required environment reviewers can add a release approval, but are not needed
   for the normal reviewed-PR flow.
2. Protect main with a ruleset/branch protection requiring a PR, at least one human
   approval, dismissal of stale reviews, resolution of review conversations, and
   **Validate Apps Script** as a required check. Prevent force pushes and deletion.
   The repository had no main protection/rulesets when inspected. Coordinate this
   with the existing scheduled data Actions, which currently push generated data
   directly to main; migrate them to PRs or explicitly scope any trusted-bot bypass.
   Never grant a bypass for script source or deployment tooling. The deployment
   workflow independently rejects direct pushes even if a bot has a data bypass.
3. Store these **environment secrets**, not repository files or general variables:

   | Secret | Value |
   | --- | --- |
   | `TOHS_SCRIPT_ID` | Verified existing Script ID |
   | `TOHS_CLASPRC_JSON` | Complete private clasp auth JSON, including refresh token |
   | `TOHS_BOOTSTRAP_JSON` | `.local/bootstrap.json` for initial comparison/recovery |

   From `tools/tohs-apps-script`, upload directly via stdin without printing them:

   ```sh
   printf '%s' "$TOHS_SCRIPT_ID" | gh secret set TOHS_SCRIPT_ID --env tohs-apps-script-production
   gh secret set TOHS_CLASPRC_JSON --env tohs-apps-script-production < .local/.clasprc.json
   gh secret set TOHS_BOOTSTRAP_JSON --env tohs-apps-script-production < .local/bootstrap.json
   ```

4. Keep repository variable `TOHS_APPS_SCRIPT_ENABLED` absent or `false` until the
   comparison, secrets, environment restrictions, protection settings, and linking
   PR have been verified. Set the **repository-level** variable to `true` immediately
   before merging the approved linking PR (job-level conditions cannot use variables
   available only after entering an environment). If linking was already merged
   with the switch off, open a small reviewed follow-up under `scripts/tohs/` or
   `tools/tohs-apps-script/` to trigger the first deployment. Do not push to main.
5. Have another human approve the PR's **final head commit**, then squash-merge or
   use a merge commit into main. Self-approval, bot approvals, stale approvals,
   outstanding change requests, rebase/multi-commit pushes, and stale workflow
   commits are deliberately refused. A solo owner must arrange another reviewer;
   do not bypass this check to make deployment run.

The initial baseline can be removed after a successful readback. Keep the local
backup securely. Delete/revoke OAuth credentials when retiring access; never
commit them. `.gitignore` excludes auth/link files and private work directories,
and `_site.yml` excludes `tools` and `private` from site output.

## Routine changes and deployment behavior

Edit `scripts/tohs/` on a branch, run the checks, and open a PR. The secret-free
validation job runs on every PR so its required check does not remain pending due
to path filters. Only a push to main affecting source/tooling/workflow can deploy.
There is no manual dispatch, schedule, PR deployment, or `pull_request_target`.

After validation, the deployment job verifies that the current main SHA is a
single merge/squash commit from an approved PR. It installs locked clasp 3.4.1
without package lifecycle scripts, retests the merged tree, then:

1. Authenticates with a temporary private auth file and pulls the whole live
   project into a temporary directory. No raw source or API errors are logged.
2. Requires live contents to equal the previous main revision's verified bundle,
   the exact new bundle (safe rerun), or the owner-verified bootstrap pair.
3. Requires unchanged remote file inventory and manifest, preventing accidental
   deletion of helpers or permission/integration changes.
4. Pulls again immediately before writing and rechecks main/review state; any drift
   stops the job. Serial deployment concurrency prevents competing CI pushes.
5. Stages only allowlisted source plus manifest, calls `clasp push --force`, pulls
   back and verifies the exact result. `--force` skips the manifest prompt only
   after the equality guard; it is not permission to skip comparisons.
6. Removes all temporary source/auth files, including on exceptions. No artifacts
   are uploaded and no functions are executed by the deployer.

Existing Script Properties, triggers, owner identities, Form responses, Sheet
contents, and permissions are not recreated. Installed triggers will execute the
new code on their normal schedule, so code review/tests still matter. Check the
existing execution history and safe-export freshness privately after first push;
do not invoke installation or acceptance helpers against production as a CI test.

Apps Script does not offer a conditional `updateContent` write. Avoid browser
editor edits during deployment: the second pull narrows, but cannot eliminate, the
race between last comparison and push. Freeze source editing for initial cutover.
Unexpected file additions/removals, manifest changes, or scope migrations require
a separately reviewed migration; this workflow deliberately cannot perform them.

## Failures and rollback

A failed preflight leaves live source untouched. A failed readback after push may
mean code already changed: disable the repository variable, inspect privately,
and compare a fresh clone. Do not keep rerunning blindly. Secrets, downloaded
source, and private diagnostics must never be printed to debug a failure.

If main advanced while a deployment waited, or a prior source deployment was
skipped/failed, the job stops rather than pushing an older or unverified version.
Open a reconciliation PR from current main, compare a fresh private clone against
its candidate, and replace the bootstrap secret with the new live/target pair.
Have the final candidate reviewed and merged normally. Updating a bootstrap secret
alone does not trigger deployment. Remove it after recovery.

For rollback, revert the source in a **new reviewed PR**; the same comparison and
push checks apply. Never delete/recreate the script, triggers, Form, or Sheets.
A source rollback cannot reverse data writes already made by runtime triggers.

## Checks and dependency maintenance

```sh
npm --prefix tools/tohs-apps-script ci --ignore-scripts
npm --prefix tools/tohs-apps-script run validate
npm --prefix tools/tohs-apps-script test
Rscript -e 'testthat::test_file("tests/testthat/test-tohs_reunion.R", stop_on_failure=TRUE)'
actionlint .github/workflows/tohs-apps-script.yml
npm --prefix tools/tohs-apps-script audit
```

Tests include existing reconciliation/privacy tests plus simulated deployment,
unknown-file/manifest preservation, drift and race refusal, bootstrap binding,
readback failure, credential cleanup, and review-state guards. Simulations use
fictional source and credentials; passing them is not a live Google deployment.

At setup, `npm audit` reports three high-severity entries in the clasp → micromatch
→ braces chain for [GHSA-vfj7-8cjw-p6xm](https://github.com/advisories/GHSA-vfj7-8cjw-p6xm)
(stack exhaustion from deeply nested patterns; no patched braces release listed).
The deployer generates fixed ignore patterns from restricted flat filenames;
it does not accept user-supplied brace patterns. Track the upstream fix and update
the lockfile in a reviewed PR. Do not downgrade clasp blindly via `npm audit fix
--force`. Audit findings are disclosed rather than hidden or described as passing.

References: [Google's clasp guide](https://developers.google.com/apps-script/guides/clasp),
[clasp command/configuration documentation](https://github.com/google/clasp),
and the [existing TOHS workflow](tohs-contact-workflow.md).
