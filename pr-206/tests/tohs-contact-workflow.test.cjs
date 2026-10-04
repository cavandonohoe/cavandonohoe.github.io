const {test} = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const {reconcileContacts,literalCell} = require('../scripts/tohs/contact_workflow.js');
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
