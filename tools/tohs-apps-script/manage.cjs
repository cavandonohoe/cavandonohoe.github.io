'use strict';
// Never print downloaded source, OAuth material, API errors, or contact data.
const fs = require('node:fs');
const path = require('node:path');
const os = require('node:os');
const crypto = require('node:crypto');
const vm = require('node:vm');
const {execFileSync} = require('node:child_process');
const ROOT = path.resolve(__dirname, '../..');
const CONFIG = 'tools/tohs-apps-script/project.json';
function assert(ok, message) { if (!ok) throw new Error(message); }
function stable(value) {
  if (Array.isArray(value)) return value.map(stable);
  if (value && typeof value === 'object') return Object.fromEntries(
    Object.keys(value).sort().map(k => [k, stable(value[k])]));
  return value;
}
function canonical(files) {
  return Object.keys(files).sort().map(name => [name,
    name === 'appsscript.json' ? JSON.stringify(stable(JSON.parse(files[name]))) :
      files[name].replace(/\r\n/g, '\n').replace(/\n*$/, '\n')]);
}
function fingerprint(files) {
  return crypto.createHash('sha256').update(JSON.stringify(canonical(files))).digest('hex');
}
function readAt(file, ref) {
  return ref ? execFileSync('git', ['show', `${ref}:${file}`], {cwd:ROOT, encoding:'utf8', stdio:['ignore','pipe','pipe']}) :
    fs.readFileSync(path.join(ROOT, file), 'utf8');
}
function bundle(ref, requireVerified = false, read = readAt) {
  const config = JSON.parse(read(CONFIG, ref));
  assert(typeof config.verified === 'boolean', 'Missing verification state');
  assert(!requireVerified || config.verified, 'Live project has not been verified; follow the linking guide');
  assert(config.files && Object.keys(config.files).length > 0, 'Empty source inventory');
  const files = {};
  const included = new Set();
  for (const [target, mapping] of Object.entries(config.files)) {
    assert(/^[A-Za-z0-9_-]+\.(js|html)$/.test(target), 'Unsafe remote filename');
    const sources = Array.isArray(mapping) ? mapping : [mapping];
    assert(sources.length > 0, 'Empty source mapping');
    assert(!Array.isArray(mapping) || target.endsWith('.js'), 'Only JavaScript files can combine sources');
    const parts = sources.map(source => {
      assert(typeof source === 'string' && /^scripts\/tohs\/[A-Za-z0-9_-]+\.(js|html)$/.test(source), 'Source must be reviewed under scripts/tohs');
      assert(source.endsWith(path.extname(target)), 'Source and target types must match');
      assert(!included.has(source), 'Source mapped more than once');
      included.add(source);
      return read(source, ref);
    });
    const text = parts.join('\n');
    if (target.endsWith('.js')) new vm.Script(text, {filename:target});
    files[target] = text;
  }
  assert(included.has('scripts/tohs/contact_workflow.js'), 'Missing contact workflow');
  assert(included.has('scripts/tohs/acceptance_test.js'), 'Preserve the existing acceptance helper');
  // Do not invent scopes, timezone, advanced services, or execution settings.
  if (config.verified) {
    assert(config.manifest && typeof config.manifest === 'object' && !Array.isArray(config.manifest), 'Import the exact live manifest');
    assert(config.manifest.runtimeVersion === 'V8', 'Review the existing runtime before enabling deployment');
    assert(typeof config.manifest.timeZone === 'string', 'Missing live timezone');
    files['appsscript.json'] = JSON.stringify(config.manifest, null, 2) + '\n';
  } else assert(config.manifest === null, 'Unverified configuration must not carry a guessed manifest');
  // Compile together as Apps Script does, to catch duplicate globals.
  new vm.Script(Object.entries(files).filter(([n]) => n.endsWith('.js')).map(([,s]) => s).join('\n'));
  return {config, files};
}
function snapshot(directory) {
  const files = {};
  function walk(dir, prefix = '') {
    for (const entry of fs.readdirSync(dir, {withFileTypes:true})) {
      if (['.clasp.json', '.claspignore'].includes(entry.name)) continue;
      assert(!entry.isSymbolicLink(), 'Snapshot must not contain symlinks');
      const name = prefix + entry.name;
      if (entry.isDirectory()) walk(path.join(dir,entry.name), name + '/');
      else {
        assert(/\.(js|gs|html|json)$/.test(name), 'Unexpected snapshot file; use a clean clone');
        const normalized = name.replace(/\.gs$/, '.js');
        assert(!Object.hasOwn(files, normalized), 'Duplicate remote filename');
        files[normalized] = fs.readFileSync(path.join(dir,entry.name), 'utf8');
      }
    }
  }
  walk(directory);
  assert(files['appsscript.json'], 'Snapshot has no manifest');
  return files;
}
function allowedBaseline(live, desired, previous, bootstrap) {
  if (live === desired) return true; // Safe rerun, no write needed.
  if (previous && live === previous) return true;
  return !!bootstrap && bootstrap.live === live && bootstrap.target === desired;
}
function clasp(args, cwd) {
  if (args[0] === 'pull') {
    fs.rmSync(path.join(cwd,'src'), {recursive:true, force:true});
    fs.mkdirSync(path.join(cwd,'src'), {mode:0o700});
  }
  // Capture all output; clasp/API failures may contain private server details.
  try { execFileSync(process.execPath, [path.join(__dirname,'node_modules/@google/clasp/build/src/index.js'), '--auth', path.join(cwd,'.clasprc.json'), ...args],
    {cwd, stdio:['ignore','pipe','pipe'], timeout:120000}); }
  catch { throw new Error('clasp failed; inspect authentication/API access privately (output suppressed)'); }
}
async function deploy({load = bundle, run = clasp, verify = require('./review-gate.cjs').gate} = {}) {
  assert(process.env.GITHUB_ACTIONS === 'true' && process.env.GITHUB_EVENT_NAME === 'push' &&
    process.env.GITHUB_REF === 'refs/heads/main', 'Deployment is restricted to main push CI');
  assert(process.env.TOHS_APPS_SCRIPT_ENABLED === 'true', 'Deployment is disabled');
  await verify();
  const {files:desired} = load(undefined, true);
  const id = process.env.TOHS_SCRIPT_ID || '';
  assert(/^[A-Za-z0-9_-]{20,}$/.test(id), 'Missing valid existing Script ID');
  const auth = JSON.parse(process.env.TOHS_CLASPRC_JSON || 'null');
  assert(auth && typeof auth === 'object', 'Missing clasp credentials');
  const before = process.env.BEFORE_SHA;
  assert(/^[a-f0-9]{40}$/.test(before || ''), 'Missing previous main SHA');
  let previous;
  // The first linking PR has an unverified predecessor, so uses a reviewed bootstrap hash.
  const prior = load(before);
  if (prior.config.verified) previous = fingerprint(prior.files);
  const bootstrap = process.env.TOHS_BOOTSTRAP_JSON ? JSON.parse(process.env.TOHS_BOOTSTRAP_JSON) : null;
  const targetHash = fingerprint(desired);
  const temp = fs.mkdtempSync(path.join(os.tmpdir(), 'tohs-clasp-'));
  fs.chmodSync(temp, 0o700);
  try {
    const liveDir = path.join(temp, 'live'), pushDir = path.join(temp, 'push');
    for (const dir of [liveDir,pushDir]) {
      fs.mkdirSync(dir, {mode:0o700});
      fs.writeFileSync(path.join(dir,'.clasprc.json'), JSON.stringify(auth), {mode:0o600});
      fs.writeFileSync(path.join(dir,'.clasp.json'), JSON.stringify({scriptId:id, rootDir:'src'}));
      fs.mkdirSync(path.join(dir,'src'), {mode:0o700});
    }
    run(['pull'], liveDir);
    const live = snapshot(path.join(liveDir,'src'));
    const liveHash = fingerprint(live);
    assert(allowedBaseline(liveHash,targetHash,previous,bootstrap),
      'Live drift or unverified initial baseline; no push performed. Compare privately and review a reconciliation PR.');
    // Preserve manifest/permissions/integrations; any change needs a separate migration.
    assert(JSON.stringify(stable(JSON.parse(live['appsscript.json']))) ===
      JSON.stringify(stable(JSON.parse(desired['appsscript.json']))), 'Live manifest differs; no push performed');
    assert(JSON.stringify(Object.keys(live).sort()) === JSON.stringify(Object.keys(desired).sort()),
      'Remote file inventory differs; no push performed (avoid deleting unknown files)');
    if (liveHash === targetHash) { console.log('Live source already matches reviewed source; no push needed.'); return; }
    for (const [name,source] of Object.entries(desired)) fs.writeFileSync(path.join(pushDir,'src',name), source);
    fs.writeFileSync(path.join(pushDir,'.claspignore'), '**/**\n' + Object.keys(desired).map(n => '!' + n).join('\n') + '\n');
    // Re-read just before writing to narrow the editor-change race. Apps Script has no compare-and-swap API.
    run(['pull'], liveDir);
    assert(fingerprint(snapshot(path.join(liveDir,'src'))) === liveHash, 'Live source changed during preflight; no push performed');
    await verify();
    run(['push', '--force'], pushDir); // Force bypasses only the manifest prompt, after equality checks above.
    run(['pull'], liveDir);
    assert(fingerprint(snapshot(path.join(liveDir,'src'))) === targetHash,
      'Post-push verification failed; disable deployment and investigate privately');
    console.log('Existing project updated and source readback verified. No functions executed.');
  } finally { fs.rmSync(temp, {recursive:true, force:true}); }
}
async function main() {
  const command = process.argv[2];
  if (command === 'validate') {
    const {config} = bundle();
    console.log(config.verified ? 'Verified source inventory and syntax valid.' : 'Source syntax valid; linking pending and deployment blocked.');
  } else if (command === 'fingerprint') {
    console.log(fingerprint(process.argv[3] ? snapshot(path.resolve(process.argv[3])) : bundle(undefined,true).files));
  } else if (command === 'deploy') await deploy();
  else throw new Error('Use validate, fingerprint [private-clone-directory], or deploy');
}
module.exports = {stable, fingerprint, bundle, snapshot, allowedBaseline, deploy};
if (require.main === module) main().catch(() => {
  // Even syntax/JSON errors can quote sensitive inputs. Keep CI output generic.
  console.error('TOHS operation refused or failed. No private diagnostics are logged. Follow docs/tohs-apps-script-deployment.md.');
  process.exitCode = 1;
});
