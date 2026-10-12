'use strict';
const fs = require('node:fs');
function approved(pr, reviews, sha, repository) {
  if (!pr.merged || pr.base.ref !== 'main' || pr.base.repo.full_name !== repository ||
      pr.merge_commit_sha !== sha) return false;
  const latest = new Map();
  for (const r of reviews.slice().sort((a,b) => a.id - b.id)) {
    if (['APPROVED','CHANGES_REQUESTED','DISMISSED'].includes(r.state)) latest.set(r.user.login,r);
  }
  const decisions = [...latest.values()];
  if (decisions.some(r => r.state === 'CHANGES_REQUESTED')) return false;
  // Solo repository: the owner's own merged PR is the review. Anyone else's PR still
  // needs a human, non-author approval of its final head commit.
  const owner = repository.split('/')[0];
  if (pr.user.login === owner && pr.user.type === 'User') return true;
  return decisions.some(r => r.state === 'APPROVED' && r.commit_id === pr.head.sha &&
    r.user.login !== pr.user.login && r.user.type === 'User');
}
async function gate() {
  const {GITHUB_TOKEN:token,GITHUB_REPOSITORY:repo,GITHUB_SHA:sha} = process.env;
  const event = JSON.parse(fs.readFileSync(process.env.GITHUB_EVENT_PATH,'utf8'));
  if (process.env.GITHUB_EVENT_NAME !== 'push' || event.ref !== 'refs/heads/main' || event.forced || event.deleted)
    throw new Error('Only normal merged main pushes may deploy');
  async function get(endpoint) {
    const r = await fetch(`https://api.github.com/repos/${repo}/${endpoint}`, {
      headers:{Authorization:`Bearer ${token}`,Accept:'application/vnd.github+json','X-GitHub-Api-Version':'2022-11-28'}});
    if (!r.ok) throw new Error('GitHub verification unavailable');
    return r.json();
  }
  const current = await get('git/ref/heads/main');
  if (current.object.sha !== sha) throw new Error('Stale deployment; reconcile on current main');
  const commit = await get(`commits/${sha}`);
  if (commit.parents[0]?.sha !== event.before) throw new Error('Use one squash or merge commit per push; multi-commit pushes are refused');
  const prs = await get(`commits/${sha}/pulls?per_page=100`);
  for (const summary of prs) {
    const pr = await get(`pulls/${summary.number}`);
    const reviews = [];
    for (let page = 1; ; page++) {
      const batch = await get(`pulls/${pr.number}/reviews?per_page=100&page=${page}`);
      reviews.push(...batch);
      if (batch.length < 100) break;
    }
    if (approved(pr,reviews,sha,repo)) { console.log('Merged PR from the owner or with an approval of its final head verified.'); return; }
  }
  throw new Error('No merged owner PR or approved PR for this commit; deployment refused');
}
module.exports = {approved, gate};
if (require.main === module) gate().catch(() => { console.error('Reviewed-merge gate refused deployment; see deployment guide.'); process.exitCode=1; });
