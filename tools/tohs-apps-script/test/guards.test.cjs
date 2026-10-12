const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const {fingerprint,bundle,snapshot,allowedBaseline} = require('../manage.cjs');
const {approved} = require('../review-gate.cjs');
test('checked-in inventory compiles; unverified project cannot deploy', () => {
  const {config} = bundle();
  if (!config.verified) assert.throws(() => bundle(undefined,true), /not been verified/);
});
test('fingerprint includes every file; ignores only line endings and JSON formatting', () => {
  const one = {'Code.js':'function x() {}\r\n', 'appsscript.json':'{"a":1,"b":2}'};
  const two = {'appsscript.json':'{ "b": 2, "a": 1 }', 'Code.js':'function x() {}\n\n'};
  assert.equal(fingerprint(one),fingerprint(two));
  assert.notEqual(fingerprint(one),fingerprint({...one,'Extra.js':'// preserve me'}));
  assert.notEqual(fingerprint(one),fingerprint({...one,'Code.js':'function y() {}'}));
});
test('drift fails closed, bootstrap is bound to the exact reviewed target', () => {
  assert.equal(allowedBaseline('edited','new','old',null),false);
  assert.equal(allowedBaseline('old','new','old',null),true);
  assert.equal(allowedBaseline('new','new','old',null),true);
  assert.equal(allowedBaseline('live','target',null,{live:'live',target:'target'}),true);
  assert.equal(allowedBaseline('live','other',null,{live:'live',target:'target'}),false);
});
test('snapshot handles .gs but refuses duplicate names and unexpected private files', () => {
  const dir=fs.mkdtempSync(path.join(os.tmpdir(),'tohs-test-'));
  try {
    fs.writeFileSync(path.join(dir,'appsscript.json'),'{}');
    fs.writeFileSync(path.join(dir,'Code.gs'),'function x() {}');
    assert.ok(snapshot(dir)['Code.js']);
    fs.writeFileSync(path.join(dir,'Code.js'),'function x() {}');
    assert.throws(() => snapshot(dir), /Duplicate/);
    fs.unlinkSync(path.join(dir,'Code.js'));
    fs.writeFileSync(path.join(dir,'responses.csv'),'fictional');
    assert.throws(() => snapshot(dir), /Unexpected/);
  } finally { fs.rmSync(dir,{recursive:true,force:true}); }
});
const pr={merged:true,base:{ref:'main',repo:{full_name:'owner/repo'}},merge_commit_sha:'merge',head:{sha:'head'},user:{login:'author'}};
const review={id:1,state:'APPROVED',commit_id:'head',user:{login:'reviewer',type:'User'}};
const ok=(p=pr,r=[review])=>approved(p,r,'merge','owner/repo');
const own={...pr,user:{login:'owner',type:'User'}};
test('the owner\'s own merged main PR deploys without a second reviewer', () => {
  assert.equal(ok(own,[]),true);
  assert.equal(ok(own,[{...review,commit_id:'stale'}]),true);
  assert.equal(ok({...own,merged:false},[]),false);
  assert.equal(ok({...own,merge_commit_sha:'other'},[]),false);
  assert.equal(ok({...own,base:{ref:'dev',repo:{full_name:'owner/repo'}}},[]),false);
  assert.equal(ok({...own,base:{ref:'main',repo:{full_name:'fork/repo'}}},[]),false);
  assert.equal(ok({...own,user:{login:'owner',type:'Bot'}},[]),false);
  assert.equal(ok(own,[{...review,state:'CHANGES_REQUESTED'}]),false);
});
test('other authors still need final-head human approval of the merged main PR', () => {
  assert.equal(ok(),true);
  assert.equal(ok({...pr,merged:false}),false);
  assert.equal(ok({...pr,merge_commit_sha:'other'}),false);
  assert.equal(ok(pr,[]),false);
  assert.equal(ok(pr,[{...review,commit_id:'stale'}]),false);
  assert.equal(ok(pr,[{...review,user:{login:'robot',type:'Bot'}}]),false);
  assert.equal(ok(pr,[{...review,user:{login:'author',type:'User'}}]),false);
  assert.equal(ok(pr,[review,{...review,id:2,state:'DISMISSED'}]),false);
  assert.equal(ok(pr,[review,{...review,id:2,state:'CHANGES_REQUESTED'}]),false);
  assert.equal(ok(pr,[review,{...review,id:2,state:'COMMENTED'}]),true);
});
