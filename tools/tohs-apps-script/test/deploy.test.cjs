const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const {deploy,fingerprint,snapshot} = require('../manage.cjs');
const manifest = JSON.stringify({runtimeVersion:'V8',timeZone:'Etc/UTC'});
const old = {'Code.js':'function refreshContactWorkflow() { return 1; }','appsscript.json':manifest};
const desired = {...old,'Code.js':'function refreshContactWorkflow() { return 2; }'};
async function scenario({live=old, previous=old, bootstrap, nextPull, failReadback=false, failGate=false}={}) {
  const saved={...process.env};
  Object.assign(process.env,{GITHUB_ACTIONS:'true',GITHUB_EVENT_NAME:'push',GITHUB_REF:'refs/heads/main',
    TOHS_APPS_SCRIPT_ENABLED:'true',TOHS_SCRIPT_ID:'fictional_script_id_for_test',TOHS_CLASPRC_JSON:'{"test":"fake"}',
    BEFORE_SHA:'a'.repeat(40),TOHS_BOOTSTRAP_JSON:bootstrap ? JSON.stringify(bootstrap) : ''});
  let pushes=0,pulls=0,gates=0,temp;
  let current=live;
  try {
    await deploy({
      load:ref => ({config:{verified:!!(ref ? previous : true)},files:ref ? previous : desired}),
      verify:async()=>{ gates++; if (failGate && gates === 2) throw new Error('stale'); },
      run:(args,cwd)=>{
        temp=path.dirname(cwd);
        if(args[0]==='pull') {
          pulls++;
          if(pulls===2 && nextPull) current=nextPull;
          fs.rmSync(path.join(cwd,'src'),{recursive:true,force:true});
          fs.mkdirSync(path.join(cwd,'src'));
          for(const [name,source] of Object.entries(current)) fs.writeFileSync(path.join(cwd,'src',name),source);
        } else {
          assert.deepEqual(args,['push','--force']);
          pushes++;
          assert.deepEqual(snapshot(path.join(cwd,'src')),desired);
          assert.ok(fs.readFileSync(path.join(cwd,'.claspignore'),'utf8').includes('!Code.js'));
          if (!failReadback) current=desired;
        }
      }
    });
    return {pushes,pulls,gates};
  } catch(error) { return {error,pushes,pulls,gates}; }
  finally {
    if(temp) assert.equal(fs.existsSync(temp),false,'credentials and snapshots cleaned');
    for(const k of Object.keys(process.env)) if(!(k in saved)) delete process.env[k];
    Object.assign(process.env,saved);
  }
}
test('deployment pushes only verified payload and checks readback without running functions',async()=>{
  assert.deepEqual(await scenario(),{pushes:1,pulls:3,gates:2});
});
test('same source is an idempotent no-op',async()=>{
  assert.deepEqual(await scenario({live:desired}),{pushes:0,pulls:1,gates:1});
});
test('unverified live source, unknown files, and manifest changes never push',async()=>{
  for(const live of [{...old,'Extra.js':'function extra() {}'},
    {...old,'appsscript.json':'{"runtimeVersion":"V8","timeZone":"America/Los_Angeles"}'}]) {
    const result=await scenario({live,previous:live});
    // Previous equality alone must not allow deletion of a file or changing settings.
    assert.ok(result.error); assert.equal(result.pushes,0);
  }
  const drift=await scenario({live:{...old,'Code.js':'// private edit'}});
  assert.ok(drift.error); assert.equal(drift.pushes,0);
});
test('a new remote file or stale main during preflight aborts the write',async()=>{
  for(const opts of [{nextPull:{...old,'Extra.js':'// concurrent edit'}},{failGate:true}]) {
    const result=await scenario(opts); assert.ok(result.error); assert.equal(result.pushes,0);
  }
});
test('initial deployment needs both live and target hashes; readback mismatch fails',async()=>{
  const blocked=await scenario({previous:null}); assert.ok(blocked.error); assert.equal(blocked.pushes,0);
  const first=await scenario({previous:null,bootstrap:{live:fingerprint(old),target:fingerprint(desired)}});
  assert.equal(first.pushes,1); assert.equal(first.error,undefined);
  const failed=await scenario({failReadback:true}); assert.ok(failed.error); assert.equal(failed.pushes,1);
});
