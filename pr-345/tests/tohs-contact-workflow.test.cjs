const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const {reconcileContacts,literalCell,responseSheetSubmissions,assertSubmissionAudit} = require('../scripts/tohs/contact_workflow.js');
const roster = () => [{id:'9001',first:'Testy',last:'Fakename',email:'',phone:'',preferred:'',sources:{}},
  {id:'91',first:'Ada',last:'Smith',email:'existing@example.invalid',phone:'1234567890',preferred:'',sources:{}}];
const submission = (extra={}) => Object.assign({response_id:'test-response',timestamp:'2026-10-04T00:00:00Z',
  first:' Testy ',last:'FAKENAME',preferred:'Testy Fakename',email:'test@example.invalid',phone:'',reunion:'Yes',committee:'No'},extra);

test('Testy ID 9001 updates email coverage and leaves the public missing-name list', () => {
  const original = roster();
  const before = reconcileContacts(original,[],{});
  const after = reconcileContacts(original,[submission()],{});
  assert.equal(after.master[0].id,'9001');
  assert.equal(after.master[0].email,'test@example.invalid');
  assert.equal(after.publicRows[0].email_bool,true);
  assert.equal(before.publicRows.filter(r => r.email_bool).length + 1,after.publicRows.filter(r => r.email_bool).length);
  // Same public-name and missing-email fields consumed by the Rmd website.
  assert.equal(after.publicRows.filter(r => !r.email_bool).some(r => r.first_name === 'Testy' && r.last_name === 'Fakename'),false);
  assert.equal(original[0].email,'');
  assert.equal(JSON.stringify(after.publicRows).includes('@'),false);
  assert.deepEqual(Object.keys(after.publicRows[0]),['first_name','last_name','email_bool']);
});
test('conflicting contacts require explicit approval; blanks cannot erase', () => {
  const s = submission({first:'Ada',last:'Smith',email:'new@example.invalid',phone:''});
  const pending = reconcileContacts(roster(),[s],{});
  assert.equal(pending.master[1].email,'existing@example.invalid');
  assert.equal(pending.master[1].phone,'1234567890');
  assert.equal(pending.review.length,1);
  const approved = reconcileContacts(roster(),[s],{'test-response':{roster_id:'91',action:'Approve replacement'}});
  assert.equal(approved.master[1].email,'new@example.invalid');
  assert.equal(approved.master[1].phone,'1234567890');
});
test('duplicates, aliases and identity disagreements cannot silently merge people', () => {
  const duplicate = roster().concat({...roster()[0],id:'92'});
  assert.equal(reconcileContacts(duplicate,[submission()],{}).review.length,1);
  assert.equal(reconcileContacts(roster(),[submission({email:'existing@example.invalid'})],{}).master[0].email,'');
  assert.equal(reconcileContacts(roster(),[submission({last:'Changed'})],{}).review.length,1);
  const manual = reconcileContacts(roster(),[submission({last:'Changed'})],
    {'test-response':{roster_id:'9001',action:'Approve fill'}});
  assert.equal(manual.master[0].email,'test@example.invalid');
});
test('replay and duplicate delivery are deterministic; rejected responses never apply', () => {
  const a = submission(), b = submission({response_id:'second',timestamp:'2026-10-05T00:00:00Z',email:'other@example.invalid'});
  assert.deepEqual(reconcileContacts(roster(),[a,b,a],{}),reconcileContacts(roster(),[b,a],{}));
  assert.equal(reconcileContacts(roster(),[a],{'test-response':{action:'Reject'}}).master[0].email,'');
});
test('invalid values fail safely, and formula-like private values are literal', () => {
  assert.equal(reconcileContacts(roster(),[submission({email:'invalid'})],{}).master[0].email,'');
  assert.throws(() => reconcileContacts([roster()[0],roster()[0]],[],{}),/Invalid roster/);
  assert.throws(() => reconcileContacts([{...roster()[0],first:'private@example.invalid'}],[],{}),/Unsafe public/);
  assert.equal(literalCell('=IMPORTDATA("https://example.invalid")'),'\'=IMPORTDATA("https://example.invalid")');
});
test('page renders snapshots only; no live private-sheet bootstrap remains', () => {
  const page = fs.readFileSync('tohs_reunion.Rmd','utf8');
  assert.equal(page.includes('googlesheets4::read_sheet'),false);
  assert.equal(page.includes('snapshot_path'),true);
});

test('installer links a new Form with no destination and reuses it on retry', () => {
  let destination = '', links = 0, refreshes = 0;
  const titles = ['First name','Last name','Preferred/full name','Email','Phone number'];
  const form = {
    getItems: () => titles.map(title => ({getTitle: () => title})),
    getDestinationId: () => { if (!destination) throw new Error('The form currently has no response destination.'); return destination; },
    setDestination: (_type,id) => { destination = id; links++; return form; }
  };
  for (const setter of ['setPublishingSummary','setCollectEmail','setLimitOneResponsePerUser',
    'setAllowResponseEdits','setConfirmationMessage','setPublished','setAcceptingResponses']) form[setter] = () => form;
  const context = {
    FormApp: {openById: () => form, create: () => {throw new Error('Must reuse existing Form');},DestinationType:{SPREADSHEET:'spreadsheet'}},
    PropertiesService:{getScriptProperties: () => ({getProperty: () => 'existing-form'})},
    ScriptApp:{getProjectTriggers: () => ['onContactSubmit','refreshContactWorkflow'].map(name => ({getHandlerFunction: () => name}))},
    recordRefresh: () => {refreshes++;}
  };
  vm.runInNewContext(fs.readFileSync('scripts/tohs/contact_workflow.js','utf8') +
    '\nrefreshContactWorkflow = recordRefresh; installContactWorkflow(); installContactWorkflow();', context);
  assert.equal(links,1);
  assert.equal(refreshes,4);
});

test('legacy rows without IDs get stable content references', () => {
  const vm = require('node:vm');
  const crypto = require('node:crypto');
  const table = [
    ['ID','First Name','Last Name','Preferred Name','email','phone number'],
    ['', 'Testy','Fakename','','testy@example.invalid',''],
    ['', 'Ada','Lovelace','','ada@example.invalid','']
  ];
  const forms = [['ID','First Name','Last Name','Preferred Full Name','Email','Phone Number',
    'Are you interested in joining the reunion?','Are you interested in joining the planning committee?']];
  const context = {Utilities:{DigestAlgorithm:{SHA_256:'sha256'},
    computeDigest:(_,s)=>crypto.createHash('sha256').update(s).digest(),
    base64EncodeWebSafe:b=>Buffer.from(b).toString('base64url')},
    ss:{getSheetByName:n=>({getDataRange:()=>({getDisplayValues:()=>n==='Contact List (No-form)'?table:forms})})}};
  vm.createContext(context);
  vm.runInContext(fs.readFileSync('scripts/tohs/contact_workflow.js','utf8'),context);
  const first=vm.runInContext('legacySubmissions(ss)',context).map(r=>r.response_id);
  table.splice(1,2,table[2],table[1]);
  const reordered=vm.runInContext('legacySubmissions(ss)',context).map(r=>r.response_id);
  assert.equal(new Set(first).size,2);
  assert.deepEqual([...first].sort(),[...reordered].sort());
  assert.ok(first.every(id=>id.startsWith('legacy:Contact List (No-form):sha256:')));
});


test('five-question responses preserve existing historical interests', () => {
  const original = roster();
  original[0].reunion='Yes'; original[0].committee='No';
  const response = submission({reunion:'',committee:''});
  const result = reconcileContacts(original,[response],{});
  assert.equal(result.master[0].email,response.email);
  assert.equal(result.master[0].reunion,'Yes');
  assert.equal(result.master[0].committee,'No');
});

function sheetFixture(rows) {
  const crypto = require('node:crypto');
  global.Utilities = {DigestAlgorithm:{SHA_256:'sha256'},
    computeDigest:(_,s) => crypto.createHash('sha256').update(s).digest(),
    base64EncodeWebSafe:b => Buffer.from(b).toString('base64url')};
  const form = {getId:()=>'contact-form',getPublishedUrl:()=>'https://docs.google.com/forms/d/e/public-id/viewform',
    getDestinationId:()=>'private-workbook'};
  const sheet = {getSheetId:()=>12345,getFormUrl:()=>'https://docs.google.com/forms/d/contact-form/edit',
    getDataRange:()=>({getValues:()=>rows})};
  const ss = {getId:()=>'private-workbook',getSheets:()=>[sheet]};
  return {form,sheet,ss};
}
const responseHeader = ['Timestamp','First name','Last name','Preferred/full name','Email','Phone number'];

test('Sample-person Sheet-only entry reaches master and leaves public missing list', () => {
  const rows = [responseHeader,['1/2/2030 03:04:05','Sample','Personne','','sample@example.invalid','']];
  const {ss,form} = sheetFixture(rows);
  const contacts = [{id:'9002',first:'Sample',last:'Personne',email:'',sources:{}}];
  const inputs = responseSheetSubmissions(ss,form,[]);
  const result = reconcileContacts(contacts,inputs,{});
  assertSubmissionAudit(inputs,result);
  assert.equal(result.master[0].email,'sample@example.invalid');
  assert.equal(result.audit[0].status,'matched');
  assert.equal(result.publicRows.filter(r=>!r.email_bool).length,0);
  assert.equal(JSON.stringify(result.publicRows).includes('@'),false);
  assert.equal(rows[1][4],'sample@example.invalid');
  assert.deepEqual(reconcileContacts(contacts,inputs.concat(inputs),{}),result);
});

test('native duplicates keep response IDs and cannot bypass a rejection decision', () => {
  const native = submission({first:'Testy',last:'Fakename',preferred:'',reunion:'',committee:''});
  const rows = [responseHeader,[new Date('2026-10-04T00:00:00Z'),' Testy ','FAKENAME','',native.email,'']];
  const {ss,form} = sheetFixture(rows);
  const sheetInputs = responseSheetSubmissions(ss,form,[native]);
  assert.deepEqual(sheetInputs,[]);
  const result = reconcileContacts(roster(),[native,...sheetInputs],{[native.response_id]:{action:'Reject'}});
  assert.equal(result.master[0].email,'');
  assert.equal(result.audit.length,1);
  assert.equal(result.audit[0].response_id,native.response_id);
});

test('Sheet IDs survive reorder and unrelated tabs are excluded', () => {
  const rows = [responseHeader,[new Date('2026-10-04T00:00:00Z'),'Sample','Personne','','sample@example.invalid',''],
    ['','Other','Person','','other@example.invalid','']];
  const {ss,form,sheet} = sheetFixture(rows);
  ss.getSheets = ()=>[{getFormUrl:()=>null,getDataRange:()=>{throw new Error('Do not read unrelated tabs');}},sheet];
  const first = responseSheetSubmissions(ss,form,[]).map(s=>s.response_id);
  rows.splice(1,2,rows[2],rows[1]);
  assert.deepEqual(responseSheetSubmissions(ss,form,[]).map(s=>s.response_id).sort(),first.sort());
  rows[1][4]='changed@example.invalid';
  assert.notDeepEqual(responseSheetSubmissions(ss,form,[]).map(s=>s.response_id).sort(),first);
});

test('partial, invalid and conflicting Sheet entries go to review', () => {
  const rows = [responseHeader,['','Testy','','','missing-last@example.invalid',''],
    ['','Testy','Fakename','','invalid',''],['','Ada','Smith','','replacement@example.invalid','']];
  const {ss,form} = sheetFixture(rows);
  const inputs = responseSheetSubmissions(ss,form,[]), result = reconcileContacts(roster(),inputs,{});
  assertSubmissionAudit(inputs,result);
  assert.equal(result.review.length,3);
  assert.equal(result.master[0].email,'');
  assert.equal(result.master[1].email,'existing@example.invalid');
});

test('broken destination or response schema fails instead of silently ignoring inputs', () => {
  const {ss,form,sheet} = sheetFixture([responseHeader]);
  form.getDestinationId=()=>'wrong-workbook';
  assert.throws(()=>responseSheetSubmissions(ss,form,[]),/destination changed/);
  form.getDestinationId=()=>ss.getId();
  ss.getSheets=()=>[];
  assert.throws(()=>responseSheetSubmissions(ss,form,[]),/exactly one linked/);
  ss.getSheets=()=>[sheet,sheet];
  assert.throws(()=>responseSheetSubmissions(ss,form,[]),/exactly one linked/);
  ss.getSheets=()=>[sheet];
  sheet.getDataRange=()=>({getValues:()=>[['Renamed field']]});
  assert.throws(()=>responseSheetSubmissions(ss,form,[]),/schema changed/);
});

test('every input must be audited and unresolved inputs must be in review', () => {
  const inputs = [submission()], result = reconcileContacts(roster(),inputs,{});
  assert.doesNotThrow(()=>assertSubmissionAudit(inputs,result));
  assert.throws(()=>assertSubmissionAudit(inputs,{...result,audit:[]}),/missing from audit/);
  assert.throws(()=>assertSubmissionAudit(inputs,{...result,audit:result.audit.concat(result.audit)}),/missing from audit/);
  const pending = reconcileContacts(roster(),[submission({last:'Unknown'})],{});
  assert.throws(()=>assertSubmissionAudit([submission()],{...pending,review:[]}),/missing from review/);
});

test('refresh reads Sheet-only inputs and audits before writing generated output', () => {
  const {ss,form} = sheetFixture([responseHeader,['','Sample','Personne','','sample@example.invalid','']]);
  form.getResponses=()=>[];
  const writes=[];
  const context = {Date,Utilities:global.Utilities,target:ss,
    SpreadsheetApp:{openById:id=>id==='14WancXKUFazrPQPz09vSQve0Ae9jroSAufHhbYNrJYw'?ss:{},flush:()=>{}},
    LockService:{getScriptLock:()=>({waitLock:()=>{},releaseLock:()=>{}})},
    PropertiesService:{getScriptProperties:()=>({getProperty:()=>'contact-form'})},
    FormApp:{openById:()=>form},recordWrite:(...args)=>writes.push(args)};
  vm.createContext(context);
  vm.runInContext(fs.readFileSync('scripts/tohs/contact_workflow.js','utf8')+
    '\nsourceRoster = () => [{id:"9002",first:"Sample",last:"Personne",email:"",sources:{}}];'+
    '\nlegacySubmissions = () => []; readDecisions = () => ({}); writeGenerated = recordWrite; refreshContactWorkflow();',context);
  assert.equal(writes.find(w=>w[1]==='Master Contacts')[3][0][4],'sample@example.invalid');
  assert.equal(writes.filter(w=>w[1]==='Public Export').length,2);
  assert.equal(writes.find(w=>w[1]==='Public Export')[3][0][2],true);
  assert.equal(writes.find(w=>w[1]==='Match Audit')[3].length,1);
  writes.length=0;
  vm.runInContext('assertSubmissionAudit = () => {throw new Error("audit failure");};',context);
  assert.throws(()=>vm.runInContext('refreshContactWorkflow()',context),/audit failure/);
  assert.equal(writes.length,0);
});


// Organized workbook regression tests.
{
const test=require('node:test'),assert=require('node:assert/strict'),fs=require('node:fs'),vm=require('node:vm');
const code=fs.readFileSync('scripts/tohs/contact_workflow.js','utf8');
const header=['Roster ID','First Name','Graduation Last Name','Current/Preferred Name','Primary Email','Phone',
  'Reunion Interest','Committee Interest','Field Sources','Contact Status','Alternate Email','Last Contacted',
  'Last Verified','Contact Source','Notes'];
// Mutable Contacts sheet: display values are strings, writes land in the backing rows.
function context(rows, opts={}) {
  const writes=[];
  const sheet={
    getDataRange:()=>({getDisplayValues:()=>rows.map(r=>r.map(v=>v == null ? '' : String(v))),getValues:()=>rows}),
    getRange:(row,col,nr,nc)=>({setValues:v=>{writes.push({row,col,nr,nc,values:v});
      v.forEach((vals,k)=>vals.forEach((x,j)=>{rows[row-1+k][col-1+j]=String(x);}));}})
  };
  const baseline=opts.baseline || [['roster_id'],['9003']];
  const legacy=opts.legacy;
  const c={ss:{getSheetByName:n=>n==='Contacts'?sheet:n==='Master Contacts'?
    {getDataRange:()=>({getDisplayValues:()=>baseline})}:
    n==='Legacy Intake'&&legacy?{getDataRange:()=>({getDisplayValues:()=>legacy})}:null}};
  vm.createContext(c);vm.runInContext(code,c);
  // One scheduled refresh against the persisted Contacts tab.
  c.cycle=(subs,decisions)=>{c.subs=subs;c.decisions=decisions||{};
    return vm.runInContext('(()=>{const rows=readContactRows(ss);'+
      'const result=reconcileContacts(editableRoster(ss,rows),subs,decisions);'+
      'const written=writeEditableContacts(ss,result,rows);return {result,written};})()',c);};
  return {c,writes,rows};
}
const contactRow=(extra={})=>{const r=['9003','Pat','Exampleton','','','','','','{}','','','','','',''];
  for (const [i,v] of Object.entries(extra)) r[i]=v; return r;};
const sub=(extra={})=>Object.assign({response_id:'r1',timestamp:'2030-01-01T00:00:00Z',first:'Pat',
  last:'Exampleton',preferred:'',email:'pat@example.invalid',phone:'',reunion:'',committee:''},extra);

test('direct Contacts edit becomes public coverage; tracking columns are never written',()=>{
  const rows=[header,['9003','Pat','Exampleton','Pat Placeholder','pat@example.invalid','','Yes','No','{}','',
    'other@example.invalid','2030-01-02','2030-01-03','Text','Follow up']];
  const tracking=rows[1].slice(10);
  const {c,writes}=context(rows);
  const {result}=c.cycle([]);
  assert.equal(result.publicRows[0].email_bool,true);
  assert.equal(writes.length,1);
  assert.equal(writes[0].nc,10);
  assert.equal(rows[1][3],'Pat Placeholder');
  assert.equal(rows[1][9],'Email collected');
  assert.deepEqual(rows[1].slice(10),tracking);
  assert.equal(JSON.stringify(result.publicRows).includes('@'),false);
  assert.equal(c.cycle([]).written,0);
});
test('ID deletions and invalid primary emails refuse publication; roster changes can be accepted',()=>{
  let {c}=context([header]);
  assert.throws(()=>vm.runInContext('editableRoster(ss)',c),/acceptContactsRosterChange/);
  ({c}=context([header,contactRow(),contactRow({0:'9004',1:'New',2:'Classmate'})]));
  assert.throws(()=>vm.runInContext('editableRoster(ss)',c),/IDs changed/);
  assert.equal(vm.runInContext('editableRoster(ss,null,{acceptRosterChange:true}).length',c),2);
  ({c}=context([header,['9003','Pat','Exampleton','','invalid']]));
  assert.throws(()=>vm.runInContext('editableRoster(ss)',c),/Invalid primary email/);
});
test('short rows are accepted and a conflict marks the existing contact for review',()=>{
  const rows=[header,['9003','Pat','Exampleton','','saved@example.invalid','','','','{}']];
  const {c}=context(rows);
  c.cycle([sub({email:'different@example.invalid'})]);
  assert.equal(rows[1][4],'saved@example.invalid');
  assert.equal(rows[1][9],'Needs review');
});
test('review actions require a note and survive through the decision archive',()=>{
 const decisionRows=[['response_id','roster_id','action','reviewer_note'],['old','9003','Approve fill','Previously verified']];
 let reviewRows=[['Response ID','Roster ID','Reason','First Name','Last Name','Current/Preferred Name','Submitted Email','Phone','Reunion Interest','Committee Interest','Action','Reviewer Note'],['new','9003','','Pat','Exampleton','','','','','','Approve replacement','Confirmed by organizer']];
 const c={ss:{getSheetByName:n=>({getDataRange:()=>({getDisplayValues:()=>n==='Review Decisions'?decisionRows:reviewRows})})}};
 vm.createContext(c);vm.runInContext(code,c);
 assert.equal(vm.runInContext('readOrganizedDecisions(ss).new.note',c),'Confirmed by organizer');
 assert.equal(vm.runInContext('readOrganizedDecisions(ss).old.action',c),'Approve fill');
 reviewRows[1][11]='';assert.throws(()=>vm.runInContext('readOrganizedDecisions(ss)',c),/reviewer note required/);
});

test('replay onto persisted Contacts applies each submission once; cleared emails stay cleared',()=>{
  const rows=[header,contactRow()];
  const {c}=context(rows);
  c.cycle([sub()]);
  assert.equal(rows[1][4],'pat@example.invalid');
  const sources=JSON.parse(rows[1][8]);
  assert.equal(sources.email.id,'r1');
  assert.equal(sources._applied.r1,'auto');
  rows[1][4]=''; // Organizer clears an email they know is wrong.
  const second=c.cycle([sub()]);
  assert.equal(rows[1][4],'');
  assert.equal(second.result.review.length,0);
  assert.equal(c.cycle([sub()]).written,0);
});
test('manual edits survive an Approve replacement decision on later runs',()=>{
  const rows=[header,contactRow({4:'old@example.invalid'})];
  const {c}=context(rows);
  const decisions={r1:{roster_id:'9003',action:'Approve replacement',note:'Verified'}};
  c.cycle([sub({email:'new@example.invalid'})],decisions);
  assert.equal(rows[1][4],'new@example.invalid');
  rows[1][4]='edited@example.invalid';
  const later=c.cycle([sub({email:'new@example.invalid'})],decisions);
  assert.equal(rows[1][4],'edited@example.invalid');
  assert.equal(later.result.review.length,0);
});
test('an auto-applied value edited by an organizer is not re-queued for review',()=>{
  const rows=[header,contactRow()];
  const {c}=context(rows);
  c.cycle([sub()]);
  rows[1][4]='edited@example.invalid';
  const later=c.cycle([sub()]);
  assert.equal(rows[1][4],'edited@example.invalid');
  assert.equal(later.result.review.length,0);
});
test('Reject clears fields auto-filled by that submission, but keeps later organizer edits',()=>{
  const rows=[header,contactRow()];
  const {c}=context(rows);
  c.cycle([sub({phone:'555-0100'})]);
  assert.equal(rows[1][4],'pat@example.invalid');
  rows[1][5]='555-0199'; // Organizer corrected the phone after the auto-fill.
  const reject={r1:{action:'Reject',note:'Not this classmate'}};
  const rejected=c.cycle([sub({phone:'555-0100'})],reject);
  assert.equal(rows[1][4],'');
  assert.equal(rows[1][5],'555-0199');
  assert.equal(JSON.stringify(JSON.parse(rows[1][8])).includes('r1'),false);
  assert.equal(rejected.result.audit[0].status,'rejected');
  assert.equal(rejected.result.publicRows[0].email_bool,false);
});
test('Reject also clears auto-fills recorded in the older plain-ID Field Sources format',()=>{
  const rows=[header,contactRow({4:'pat@example.invalid',8:'{"email":"r1"}'})];
  const {c}=context(rows);
  assert.equal(c.cycle([sub()]).written,1); // Status only; the old source counts as applied.
  rows[1][4]='';
  c.cycle([sub()]);
  assert.equal(rows[1][4],'');
  rows[1][4]='pat@example.invalid'; rows[1][8]='{"email":"r1"}';
  c.cycle([sub()],{r1:{action:'Reject',note:'Wrong person'}});
  assert.equal(rows[1][4],'');
});
test('Contacts rows with data but no Roster ID fail before anything is written',()=>{
  const rows=[header,contactRow(),['','Pat','Exampleton','','','','','','','','','','','','Call back']];
  const {c,writes}=context(rows);
  assert.throws(()=>c.cycle([sub()]),/row 3 has data but no Roster ID/);
  assert.equal(writes.length,0);
});
test('Contacts writes are matched by Roster ID, not position',()=>{
  const rows=[header,['','','','','','','','','','','','','','',''],contactRow({14:'Keep me'}),
    contactRow({0:'9004',1:'Sam',2:'Sample'})];
  const {c,writes}=context(rows,{baseline:[['roster_id'],['9003'],['9004']]});
  c.cycle([sub()]);
  assert.deepEqual(writes.map(w=>w.row).sort(),[3,4]);
  assert.equal(rows[2][4],'pat@example.invalid');
  assert.equal(rows[2][14],'Keep me');
  assert.equal(rows[3][1],'Sam');
  assert.equal(rows[3][4],'');
  assert.deepEqual(rows[1],Array(15).fill(''));
});
test('edits made to Contacts during a refresh are never overwritten',()=>{
  const rows=[header,contactRow()];
  const {c,writes}=context(rows);
  const snapshot=vm.runInContext('readContactRows(ss)',c);
  c.snapshot=snapshot;
  const result=vm.runInContext('reconcileContacts(editableRoster(ss,snapshot),[],{})',c);
  rows[1][4]='typed-during-refresh@example.invalid';
  c.result=result;
  assert.throws(()=>vm.runInContext('writeEditableContacts(ss,result,snapshot)',c),/changed during refresh/);
  assert.equal(writes.length,0);
  assert.equal(rows[1][4],'typed-during-refresh@example.invalid');
});
test('Legacy Intake rows without an ID or with duplicate IDs fail loudly',()=>{
  const lh=['response_id','timestamp','first','last','preferred','email','phone','reunion','committee'];
  let {c}=context([header],{legacy:[lh,['','','Pat','Exampleton','','pat@example.invalid','','','']]});
  assert.throws(()=>vm.runInContext('organizedLegacySubmissions(ss)',c),/no response_id/);
  ({c}=context([header],{legacy:[lh,['l1','','Pat','Exampleton'],['l1','','Sam','Sample']]}));
  assert.throws(()=>vm.runInContext('organizedLegacySubmissions(ss)',c),/Duplicate Legacy Intake/);
  ({c}=context([header],{legacy:[lh,['l1','','Pat','Exampleton'],['','','','']]}));
  assert.equal(vm.runInContext('organizedLegacySubmissions(ss).length',c),1);
});
test('reused submission IDs with different content and missing IDs fail loudly',()=>{
  const roster=[{id:'9003',first:'Pat',last:'Exampleton',email:'',sources:{}}];
  const {reconcileContacts}=require('../scripts/tohs/contact_workflow.js');
  assert.throws(()=>reconcileContacts(roster,[sub(),sub({email:'other@example.invalid'})],{}),/Duplicate contact submission ID/);
  assert.throws(()=>reconcileContacts(roster,[sub({response_id:''})],{}),/without response ID/);
});
test('names containing any digit (such as a phone number) are refused for publication',()=>{
  const {reconcileContacts}=require('../scripts/tohs/contact_workflow.js');
  for (const bad of ['555-123-4567','Pat 2','Exampleton5']) {
    assert.throws(()=>reconcileContacts([{id:'9003',first:'Pat',last:bad,email:'',sources:{}}],[],{}),/Unsafe public name/);
  }
  assert.doesNotThrow(()=>reconcileContacts([{id:'9003',first:"Pat-Ann",last:"O'Example",email:'',sources:{}}],[],{}));
});
}
