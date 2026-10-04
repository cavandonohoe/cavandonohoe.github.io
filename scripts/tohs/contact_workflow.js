/* Paste this file into a standalone Apps Script project. No private data belongs here. */
const TOHS_SOURCE_ID = '1JwWeBjwwQHzmGgh8HPuO_0pghzemC3ikpLx_pXlQvsI';
const TOHS_WORKFLOW_ID = '14WancXKUFazrPQPz09vSQve0Ae9jroSAufHhbYNrJYw';
const TOHS_QUESTIONS = ['First name', 'Last name', 'Preferred/full name', 'Email',
  'Phone number', 'Reunion interest', 'Planning-committee interest'];
const TOHS_FIELDS = ['preferred', 'email', 'phone', 'reunion', 'committee'];

function textValue(value) { return value == null ? '' : String(value).trim(); }
function nameKey(value) { return textValue(value).normalize('NFKC').toLowerCase().replace(/\s+/g, ' '); }
function validEmail(value) { return /^[^\s@]+@[^\s@]+\.[^\s@]+$/.test(textValue(value)); }
function sameContact(field, a, b) {
  return field === 'phone' ? textValue(a).replace(/\D/g, '') === textValue(b).replace(/\D/g, '') :
    nameKey(a) === nameKey(b);
}

// Pure rebuild: original roster is immutable; derived contacts retain stable roster IDs.
function reconcileContacts(roster, submissions, decisions) {
  if (!roster.length || new Set(roster.map(r => r.id)).size !== roster.length ||
      roster.some(r => !r.id || !r.first || !r.last)) throw new Error('Invalid roster IDs or names');
  const master = roster.map(r => Object.assign({}, r, {sources: Object.assign({}, r.sources)}));
  const review = [], audit = [], seen = new Set();
  const sorted = submissions.slice().sort((a, b) =>
    textValue(a.timestamp).localeCompare(textValue(b.timestamp)) || a.response_id.localeCompare(b.response_id));
  for (const s of sorted) {
    if (!s.response_id || seen.has(s.response_id)) continue;
    seen.add(s.response_id);
    const d = (decisions || {})[s.response_id];
    if (d && d.action === 'Reject') { audit.push({response_id:s.response_id, status:'rejected'}); continue; }
    let match;
    if (d && ['Approve fill', 'Approve replacement'].includes(d.action)) {
      match = master.find(r => r.id === textValue(d.roster_id));
      if (!match) throw new Error('Review decision has unknown roster ID');
    } else {
      const byName = master.filter(r => nameKey(r.first + ' ' + r.last) === nameKey(s.first + ' ' + s.last));
      const byEmail = validEmail(s.email) ? master.filter(r => nameKey(r.email) === nameKey(s.email)) : [];
      if (byName.length === 1 && (!byEmail.length || (byEmail.length === 1 && byEmail[0].id === byName[0].id))) {
        match = byName[0];
      }
    }
    if (!match) {
      review.push(Object.assign({}, s, {roster_id:'', reason:'Unmatched, duplicate name, or name/email disagreement'}));
      audit.push({response_id:s.response_id, status:'needs identity review'}); continue;
    }
    const conflicts = [];
    for (const field of TOHS_FIELDS) {
      const incoming = textValue(s[field]), existing = textValue(match[field]);
      if (!incoming) continue; // Blank answers never erase existing data.
      if (field === 'email' && !validEmail(incoming)) { conflicts.push('invalid email'); continue; }
      if (['reunion','committee'].includes(field) && !['Yes','No','Maybe'].includes(incoming)) {
        conflicts.push('invalid ' + field); continue;
      }
      if (!existing || (d && d.action === 'Approve replacement')) {
        match[field] = incoming;
        match.sources[field] = s.response_id;
      } else if (!sameContact(field, incoming, existing)) conflicts.push(field);
    }
    const status = conflicts.length ? 'needs field review: ' + conflicts.join(', ') : 'matched';
    if (conflicts.length) review.push(Object.assign({}, s, {roster_id:match.id, reason:status}));
    audit.push({response_id:s.response_id, roster_id:match.id, status});
  }
  const publicRows = master.map(r => {
    for (const n of [r.first,r.last]) {
      if (n.length > 100 || /[@\r\n<>]|https?:|\d{5}/i.test(n)) throw new Error('Unsafe public name');
    }
    return {first_name:r.first, last_name:r.last, email_bool:!!textValue(r.email)};
  });
  return {master, review, audit, publicRows};
}

function sourceRoster(ss) {
  const rows = ss.getSheetByName('Full Grad Class').getDataRange().getDisplayValues();
  const h = rows[0];
  const col = label => { const i = h.indexOf(label); if (i < 0) throw new Error('Roster schema changed'); return i; };
  const c = ['ID','First Name','Last Name','New/Preferred Name','Email','Phone'].map(col);
  return rows.slice(2).filter(r => textValue(r[c[0]])).map(r => ({id:textValue(r[c[0]]),
    first:textValue(r[c[1]]),last:textValue(r[c[2]]),preferred:textValue(r[c[3]]),
    email:textValue(r[c[4]]),phone:textValue(r[c[5]]),reunion:'',committee:'',sources:{}}));
}

// Legacy IDs are response sequence numbers, NOT roster IDs. Match their names instead.
function legacySubmissions(ss) {
  const result = [];
  const specs = [
    ['Form Responses 1', ['First Name','Last Name','Preferred Full Name','Email','Phone Number',
      'Are you interested in joining the reunion?','Are you interested in joining the planning committee?']],
    ['Contact List (No-form)', ['First Name','Last Name','Preferred Name','email','phone number']]
  ];
  for (const [tab, labels] of specs) {
    const rows = ss.getSheetByName(tab).getDataRange().getDisplayValues(), h = rows[0];
    const indexes = labels.map(label => { const i = h.indexOf(label);
      if (i < 0) throw new Error('Legacy schema changed'); return i; });
    rows.slice(1).forEach((r,i) => {
      if (!textValue(r[indexes[0]]) || !textValue(r[indexes[1]])) return;
      const v = indexes.map(j => textValue(r[j]));
      if (!textValue(r[0])) throw new Error('Legacy contact lacks stable source ID');
      result.push({response_id:'legacy:' + tab + ':' + textValue(r[0]),timestamp:'',
        first:v[0],last:v[1],preferred:v[2],email:v[3],phone:v[4],reunion:v[5] || '',committee:v[6] || ''});
    });
  }
  return result;
}

function formSubmissions(form) {
  return form.getResponses().map(response => {
    const answers = {};
    response.getItemResponses().forEach(item => { answers[item.getItem().getTitle()] = textValue(item.getResponse()); });
    return {response_id:response.getId(),timestamp:response.getTimestamp().toISOString(),
      first:answers[TOHS_QUESTIONS[0]],last:answers[TOHS_QUESTIONS[1]],preferred:answers[TOHS_QUESTIONS[2]],
      email:answers[TOHS_QUESTIONS[3]],phone:answers[TOHS_QUESTIONS[4]],
      reunion:answers[TOHS_QUESTIONS[5]],committee:answers[TOHS_QUESTIONS[6]]};
  });
}

// Treat all user-supplied text as literal, including spreadsheet formula-like text.
function literalCell(value) { return typeof value === 'string' && /^[=+\-@]/.test(value) ? "'" + value : value; }
function writeGenerated(ss, name, header, rows) {
  const sheet = ss.getSheetByName(name) || ss.insertSheet(name);
  const values = [header].concat(rows).map(row => row.map(literalCell));
  if (sheet.getMaxRows() < values.length) sheet.insertRowsAfter(sheet.getMaxRows(), values.length - sheet.getMaxRows());
  if (sheet.getMaxColumns() < header.length) sheet.insertColumnsAfter(sheet.getMaxColumns(), header.length - sheet.getMaxColumns());
  // Only script-owned derived tabs are rewritten. No source or raw-response tabs are touched.
  const oldRows = sheet.getLastRow();
  sheet.getRange(1,1,values.length,header.length).setValues(values);
  if (oldRows > values.length) sheet.getRange(values.length+1,1,oldRows-values.length,header.length).clearContent();
  sheet.setFrozenRows(1);
  sheet.getRange(1,1,1,header.length).setFontWeight('bold').setBackground('#eeeeee');
}

function readDecisions(ss) {
  let sheet = ss.getSheetByName('Review Decisions');
  if (!sheet) { sheet = ss.insertSheet('Review Decisions');
    sheet.appendRow(['response_id','roster_id','action','reviewer_note']); sheet.setFrozenRows(1); }
  const rows = sheet.getDataRange().getDisplayValues(), result = {};
  for (const r of rows.slice(1)) {
    if (!textValue(r[0])) continue;
    if (result[r[0]]) throw new Error('Duplicate review decision');
    if (!['Approve fill','Approve replacement','Reject'].includes(r[2]) || !textValue(r[3]))
      throw new Error('Review action and reviewer note required');
    result[r[0]] = {roster_id:r[1],action:r[2],note:r[3]};
  }
  return result;
}

function refreshContactWorkflow() {
  const lock = LockService.getScriptLock(); lock.waitLock(30000);
  try {
    const source = SpreadsheetApp.openById(TOHS_SOURCE_ID), target = SpreadsheetApp.openById(TOHS_WORKFLOW_ID);
    const formId = PropertiesService.getScriptProperties().getProperty('TOHS_FORM_ID');
    const submissions = legacySubmissions(source).concat(formId ? formSubmissions(FormApp.openById(formId)) : []);
    const result = reconcileContacts(sourceRoster(source), submissions, readDecisions(target));
    const masterHeader = ['roster_id','first_name','last_name','preferred_name','email','phone',
      'reunion_interest','committee_interest','field_sources'];
    writeGenerated(target,'Master Contacts',masterHeader,result.master.map(r =>
      [r.id,r.first,r.last,r.preferred,r.email,r.phone,r.reunion,r.committee,JSON.stringify(r.sources)]));
    writeGenerated(target,'Match Review',['response_id','roster_id','reason','first_name','last_name','preferred_name',
      'email','phone','reunion_interest','committee_interest'],result.review.map(r =>
      [r.response_id,r.roster_id,r.reason,r.first,r.last,r.preferred,r.email,r.phone,r.reunion,r.committee]));
    writeGenerated(target,'Match Audit',['response_id','roster_id','status'],result.audit.map(r =>
      [r.response_id,r.roster_id || '',r.status]));
    const updated = new Date().toISOString();
    // Public Export contains ONLY the allowlisted public fields.
    writeGenerated(target,'Public Export',['first_name','last_name','email_bool','updated_at'],
      result.publicRows.map(r => [r.first_name,r.last_name,r.email_bool,updated]));
    writeGenerated(target,'Workflow Status',['key','value'],[['last_success',updated],
      ['roster_count',result.master.length],['review_count',result.review.length],
      ['form_url',formId ? FormApp.openById(formId).getPublishedUrl() : 'Form not installed']]);
    SpreadsheetApp.flush();
  } finally { lock.releaseLock(); }
}

function installContactWorkflow() {
  const props = PropertiesService.getScriptProperties();
  let formId = props.getProperty('TOHS_FORM_ID');
  if (!formId) {
    const form = FormApp.create('TOHS Class of 2012 — Reunion Contact Updates', false);
    // Persist immediately so a partial installation cannot silently create duplicate forms.
    formId = form.getId(); props.setProperty('TOHS_FORM_ID',formId);
  }
  const form = FormApp.openById(formId);
  if (form.getItems().length === 0) {
    form.setDescription('Use your name from graduation so we can match the class roster. Contact details and responses remain private to reunion organizers. The website shows only class names and whether we have an email.');
    for (const [i,title] of TOHS_QUESTIONS.entries()) {
      if (i < 5) {
        const item = form.addTextItem().setTitle(title).setRequired([0,1,3].includes(i));
        if (i === 3) item.setValidation(FormApp.createTextValidation().requireTextIsEmail().build());
      } else form.addMultipleChoiceItem().setTitle(title).setChoiceValues(['Yes','No','Maybe']).setRequired(true);
    }
  }
  if (form.getItems().map(i => i.getTitle()).join('|') !== TOHS_QUESTIONS.join('|'))
    throw new Error('Partial or changed Form schema: inspect before continuing');
  form.setPublishingSummary(false).setCollectEmail(false).setLimitOneResponsePerUser(false)
    .setAllowResponseEdits(false).setConfirmationMessage('Thank you! Your update has been received.');
  if (form.getDestinationId() !== TOHS_WORKFLOW_ID)
    form.setDestination(FormApp.DestinationType.SPREADSHEET,TOHS_WORKFLOW_ID);
  const triggers = ScriptApp.getProjectTriggers();
  if (!triggers.some(t => t.getHandlerFunction() === 'onContactSubmit'))
    ScriptApp.newTrigger('onContactSubmit').forForm(form).onFormSubmit().create();
  if (!triggers.some(t => t.getHandlerFunction() === 'refreshContactWorkflow'))
    ScriptApp.newTrigger('refreshContactWorkflow').timeBased().everyHours(1).create();
  refreshContactWorkflow();
  // Publish only after the linked destination, matching and recovery trigger are installed.
  form.setPublished(true).setAcceptingResponses(true);
  refreshContactWorkflow();
}

function onContactSubmit() { refreshContactWorkflow(); }

if (typeof module !== 'undefined') module.exports = {reconcileContacts,literalCell,nameKey};
