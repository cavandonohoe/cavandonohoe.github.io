# TOHS Apps Script deployment

This directory deploys the existing TOHS project using the verified mapping in
`project.json`. The repository sources are combined into the existing `Code.js`;
the live manifest and file inventory must match before any write.

## Initial activation

The linking change (#244) merged while deployment was disabled. This follow-up
provides a reviewed change under the deployment workflow's path filter so its
merge can start the first deployment after environment secrets and the repository
variable have been configured.

The live source was downloaded again on 2026-10-12 and matched the reviewed
bootstrap baseline. The current main bundle also matched the recorded target.
The bootstrap credential is bound to that exact pair; later source changes
require a fresh comparison if the initial deployment has not succeeded.

The `tohs-apps-script-production` environment restricts deployments to `main`
and requires the repository owner's approval with self-review allowed.
After merging, approve the pending deployment and verify its source readback.
Existing triggers will run the updated code on their normal schedule; the
deployment does not run installation, refresh, or acceptance helpers.

See [the deployment guide](../../docs/tohs-apps-script-deployment.md) for setup,
validation, drift handling, and rollback. Remove the bootstrap secret after a
successful first deployment and retain the private source backup securely.
