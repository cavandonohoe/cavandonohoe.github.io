const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const os = require('node:os');
const path = require('node:path');
const {execFileSync} = require('node:child_process');
test('pinned clasp stages only the explicit allowlist, with no Google authentication', () => {
  const dir=fs.mkdtempSync(path.join(os.tmpdir(),'tohs-payload-'));
  try {
    fs.mkdirSync(path.join(dir,'src'));
    fs.writeFileSync(path.join(dir,'.clasp.json'),JSON.stringify({scriptId:'fictional_script_id_for_test',rootDir:'src'}));
    fs.writeFileSync(path.join(dir,'.claspignore'),'**/**\n!Code.js\n!appsscript.json\n');
    fs.writeFileSync(path.join(dir,'src','Code.js'),'function fixture() {}');
    fs.writeFileSync(path.join(dir,'src','appsscript.json'),'{"runtimeVersion":"V8","timeZone":"Etc/UTC"}');
    fs.writeFileSync(path.join(dir,'src','excluded.js'),'// must not push');
    const output=execFileSync(process.execPath,[path.resolve(__dirname,'../node_modules/@google/clasp/build/src/index.js'),
      '--auth',path.join(dir,'absent-auth.json'),'show-file-status','--json'],{cwd:dir,encoding:'utf8',stdio:['ignore','pipe','pipe']});
    assert.deepEqual(JSON.parse(output).filesToPush.sort(),['src/Code.js','src/appsscript.json'].sort());
  } finally {fs.rmSync(dir,{recursive:true,force:true});}
});
