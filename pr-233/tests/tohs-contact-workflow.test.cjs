const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');
const {reconcileContacts,literalCell,responseSheetSubmissions,assertSubmissionAudit} = require('../scripts/tohs/contact_workflow.js');
const roster = () => [{id:'90',first:'Meghan',last:'Conlan',email:'',phone:'',preferred:'',sources:{}},
  {id:'91',first:'Ada',last:'Smith',email:'existing@example.invalid',phone:'1234567890',preferred:'',sources:{}}];
const submission = (extra={}) => Object.assign({response_id:'test-response',timestamp:'2026-10-04T00:00:00Z',
  first:' Meghan ',last:'CONLAN',preferred:'Meghan Conlan',email:'test@example.invalid',phone:'',reunion:'Yes',committee:'No'},extra);

test('Meghan ID 90 updates email coverage and leaves the public missing-name list', () => {
  const original = roster();
  const before = reconcileContacts(original,[],{});
  const after = reconcileContacts(original,[submission()],{});
  assert.equal(after.master[0].id,'90');
  assert.equal(after.master[0].email,'test@example.invalid');
  assert.equal(after.publicRows[0].email_bool,true);
  assert.equal(before.publicRows.filter(r => r.email_bool).length + 1,after.publicRows.filter(r => r.email_bool).length);
  // Same public-name and missing-email fields consumed by the Rmd website.
  assert.equal(after.publicRows.filter(r => !r.email_bool).some(r => r.first_name === 'Meghan' && r.last_name === 'Conlan'),false);
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
    {'test-response':{roster_id:'90',action:'Approve fill'}});
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
    ['', 'Meghan','Conlan','','meghan@example.invalid',''],
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
  const sheet = {getSheetId:()=>2040209201,getFormUrl:()=>'https://docs.google.com/forms/d/contact-form/edit',
    getDataRange:()=>({getValues:()=>rows})};
  const ss = {getId:()=>'private-workbook',getSheets:()=>[sheet]};
  return {form,sheet,ss};
}
const responseHeader = ['Timestamp','First name','Last name','Preferred/full name','Email','Phone number'];

test('Karis-style Sheet-only entry reaches master and leaves public missing list', () => {
  const rows = [responseHeader,['10/4/2026 19:49:32','Karis','Schneider','','karis@example.invalid','']];
  const {ss,form} = sheetFixture(rows);
  const contacts = [{id:'468',first:'Karis',last:'Schneider',email:'',sources:{}}];
  const inputs = responseSheetSubmissions(ss,form,[]);
  const result = reconcileContacts(contacts,inputs,{});
  assertSubmissionAudit(inputs,result);
  assert.equal(result.master[0].email,'karis@example.invalid');
  assert.equal(result.audit[0].status,'matched');
  assert.equal(result.publicRows.filter(r=>!r.email_bool).length,0);
  assert.equal(JSON.stringify(result.publicRows).includes('@'),false);
  assert.equal(rows[1][4],'karis@example.invalid');
  assert.deepEqual(reconcileContacts(contacts,inputs.concat(inputs),{}),result);
});

test('native duplicates keep response IDs and cannot bypass a rejection decision', () => {
  const native = submission({first:'Meghan',last:'Conlan',preferred:'',reunion:'',committee:''});
  const rows = [responseHeader,[new Date('2026-10-04T00:00:00Z'),' Meghan ','CONLAN','',native.email,'']];
  const {ss,form} = sheetFixture(rows);
  const sheetInputs = responseSheetSubmissions(ss,form,[native]);
  assert.deepEqual(sheetInputs,[]);
  const result = reconcileContacts(roster(),[native,...sheetInputs],{[native.response_id]:{action:'Reject'}});
  assert.equal(result.master[0].email,'');
  assert.equal(result.audit.length,1);
  assert.equal(result.audit[0].response_id,native.response_id);
});

test('Sheet IDs survive reorder and unrelated tabs are excluded', () => {
  const rows = [responseHeader,[new Date('2026-10-04T00:00:00Z'),'Karis','Schneider','','karis@example.invalid',''],
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
  const rows = [responseHeader,['','Meghan','','','missing-last@example.invalid',''],
    ['','Meghan','Conlan','','invalid',''],['','Ada','Smith','','replacement@example.invalid','']];
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
  const {ss,form} = sheetFixture([responseHeader,['','Karis','Schneider','','karis@example.invalid','']]);
  form.getResponses=()=>[];
  const writes=[];
  const context = {Date,Utilities:global.Utilities,target:ss,
    SpreadsheetApp:{openById:id=>id==='14WancXKUFazrPQPz09vSQve0Ae9jroSAufHhbYNrJYw'?ss:{},flush:()=>{}},
    LockService:{getScriptLock:()=>({waitLock:()=>{},releaseLock:()=>{}})},
    PropertiesService:{getScriptProperties:()=>({getProperty:()=>'contact-form'})},
    FormApp:{openById:()=>form},recordWrite:(...args)=>writes.push(args)};
  vm.createContext(context);
  vm.runInContext(fs.readFileSync('scripts/tohs/contact_workflow.js','utf8')+
    '\nsourceRoster = () => [{id:"468",first:"Karis",last:"Schneider",email:"",sources:{}}];'+
    '\nlegacySubmissions = () => []; readDecisions = () => ({}); writeGenerated = recordWrite; refreshContactWorkflow();',context);
  assert.equal(writes.find(w=>w[1]==='Master Contacts')[3][0][4],'karis@example.invalid');
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
function context(rows){const writes=[]; const sheet={getDataRange:()=>({getDisplayValues:()=>rows,getValues:()=>rows}),getRange:()=>({setValues:v=>writes.push(v)})};const c={ss:{getSheetByName:n=>n==='Contacts'?sheet:n==='Master Contacts'?{getDataRange:()=>({getDisplayValues:()=>[['roster_id'],['125']]})}:null}};vm.createContext(c);vm.runInContext(code,c);return {c,writes};}
const header=['Roster ID','First Name','Graduation Last Name','Current/Preferred Name','Primary Email','Phone','Reunion Interest','Committee Interest','Field Sources','Contact Status','Alternate Email','Last Contacted','Last Verified','Contact Source','Notes'];
test('direct Contacts edit becomes public coverage and keeps current name and tracking fields',()=>{const rows=[header,['125','Jessica','Dorthalina','Jessica Rogers','jessica@example.invalid','','Yes','No','{}','','other@example.invalid','2026-10-06','2026-10-07','Text','Follow up']];const {c,writes}=context(rows);vm.runInContext('const result = reconcileContacts(editableRoster(ss),[],{}); writeEditableContacts(ss,result);',c);assert.equal(vm.runInContext('result.publicRows[0].email_bool',c),true);assert.equal(writes[0][0][3],'Jessica Rogers');assert.equal(writes[0][0][9],'Email collected');assert.deepEqual([...writes[0][0].slice(10)],rows[1].slice(10));assert.equal(JSON.stringify(vm.runInContext('result.publicRows',c)).includes('@'),false);});
test('ID deletions and invalid primary emails refuse publication',()=>{let {c}=context([header]);assert.throws(()=>vm.runInContext('editableRoster(ss)',c),/IDs changed/);({c}=context([header,['125','Jessica','Dorthalina','','invalid']]));assert.throws(()=>vm.runInContext('editableRoster(ss)',c),/Invalid primary email/);});
test('short rows are padded and a conflict marks the existing contact for review',()=>{const {c,writes}=context([header,['125','Jessica','Dorthalina','','saved@example.invalid','','','','{}']]);vm.runInContext('writeEditableContacts(ss,{master:editableRoster(ss),review:[{roster_id:"125"}]})',c);assert.equal(writes[0][0].length,15);assert.equal(writes[0][0][9],'Needs review');});
test('review actions require a note and survive through the decision archive',()=>{
 const decisionRows=[['response_id','roster_id','action','reviewer_note'],['old','125','Approve fill','Previously verified']];
 let reviewRows=[['Response ID','Roster ID','Reason','First Name','Last Name','Current/Preferred Name','Submitted Email','Phone','Reunion Interest','Committee Interest','Action','Reviewer Note'],['new','125','','Jessica','Dorthalina','','','','','','Approve replacement','Confirmed by organizer']];
 const c={ss:{getSheetByName:n=>({getDataRange:()=>({getDisplayValues:()=>n==='Review Decisions'?decisionRows:reviewRows})})}};
 vm.createContext(c);vm.runInContext(code,c);
 assert.equal(vm.runInContext('readOrganizedDecisions(ss).new.note',c),'Confirmed by organizer');
 assert.equal(vm.runInContext('readOrganizedDecisions(ss).old.action',c),'Approve fill');
 reviewRows[1][11]='';assert.throws(()=>vm.runInContext('readOrganizedDecisions(ss)',c),/reviewer note required/);
});

}
