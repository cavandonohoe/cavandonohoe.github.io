/* Paste this file into a standalone Apps Script project. No private data belongs here. */
const TOHS_SOURCE_ID = '1JwWeBjwwQHzmGgh8HPuO_0pghzemC3ikpLx_pXlQvsI';
const TOHS_WORKFLOW_ID = '14WancXKUFazrPQPz09vSQve0Ae9jroSAufHhbYNrJYw';
const TOHS_EXPORT_ID = '1-LqsItnKUbSvGhMqMMKmHg-IkMk3uonioNJHD95st58';
const TOHS_QUESTIONS = ['First name', 'Last name', 'Preferred/full name', 'Email',
  'Phone number'];
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
      // Some old manually entered contacts have no ID. Hash their normalized
      // content rather than inventing a person ID or depending on row position.
      const sourceId = textValue(r[0]) || 'sha256:' + Utilities.base64EncodeWebSafe(
        Utilities.computeDigest(Utilities.DigestAlgorithm.SHA_256, JSON.stringify(v))
      );
      result.push({response_id:'legacy:' + tab + ':' + sourceId,timestamp:'',
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
      reunion:answers['Reunion interest'] || '',committee:answers['Planning-committee interest'] || ''};
  });
}

function contactSignature(s) {
  return JSON.stringify(['first','last','preferred','email','phone','reunion','committee']
    .map(field => field === 'phone' ? textValue(s[field]).replace(/\D/g, '') : nameKey(s[field])));
}

// API writes to the response Sheet are not Google Form responses. Read both,
// keeping native response IDs (and their review decisions) for duplicate rows.
function responseSheetSubmissions(ss, form, nativeResponses) {
  if (form.getDestinationId() !== ss.getId()) throw new Error('Form destination changed');
  const published = form.getPublishedUrl().split('?')[0];
  const sheets = ss.getSheets().filter(sheet => {
    const url = textValue(sheet.getFormUrl());
    return url && (url.includes('/' + form.getId() + '/') || url.split('?')[0] === published);
  });
  if (sheets.length !== 1) throw new Error('Expected exactly one linked contact response sheet');
  const sheet = sheets[0], rows = sheet.getDataRange().getValues(), header = rows[0];
  const index = title => header.indexOf(title);
  if (!['Timestamp',...TOHS_QUESTIONS].every(title => index(title) >= 0))
    throw new Error('Contact response sheet schema changed');
  const nativeSignatures = new Set(nativeResponses.map(contactSignature));
  const result = [];
  for (const row of rows.slice(1)) {
    const answer = title => index(title) < 0 ? '' : textValue(row[index(title)]);
    const fields = {first:answer('First name'),last:answer('Last name'),
      preferred:answer('Preferred/full name'),email:answer('Email'),phone:answer('Phone number'),
      reunion:answer('Reunion interest'),committee:answer('Planning-committee interest')};
    if (!Object.values(fields).some(Boolean)) continue;
    // Partial and invalid manual entries remain visible in the private review queue.
    const stamp = row[index('Timestamp')];
    const timestamp = stamp instanceof Date && !isNaN(stamp.getTime()) ? stamp.toISOString() : '';
    const signature = contactSignature(fields);
    if (nativeSignatures.has(signature)) continue;
    const digest = Utilities.base64EncodeWebSafe(Utilities.computeDigest(
      Utilities.DigestAlgorithm.SHA_256, JSON.stringify([timestamp || textValue(stamp),signature])));
    result.push(Object.assign(fields, {response_id:'sheet:' + sheet.getSheetId() + ':sha256:' + digest,timestamp}));
  }
  return result;
}

function assertSubmissionAudit(submissions, result) {
  const expected = new Set(submissions.map(s => s.response_id));
  const audited = new Set(result.audit.map(s => s.response_id));
  if (expected.has('') || expected.has(undefined) || result.audit.length !== audited.size ||
      expected.size !== audited.size || [...expected].some(id => !audited.has(id)))
    throw new Error('Contact submission missing from audit; publication refused');
  for (const entry of result.audit) {
    if (entry.status.startsWith('needs ') && !result.review.some(r => r.response_id === entry.response_id))
      throw new Error('Contact submission missing from review; publication refused');
  }
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
    const target = SpreadsheetApp.openById(TOHS_WORKFLOW_ID);
    const organized = !!(target.getSheetByName && target.getSheetByName('Contacts'));
    const source = organized ? target : SpreadsheetApp.openById(TOHS_SOURCE_ID);
    const formId = PropertiesService.getScriptProperties().getProperty('TOHS_FORM_ID');
    if (!formId) throw new Error('Contact Form is not installed');
    const form = FormApp.openById(formId), nativeResponses = formSubmissions(form);
    const sheetResponses = responseSheetSubmissions(target,form,nativeResponses);
    const submissions = (organized ? organizedLegacySubmissions(target) : legacySubmissions(source)).concat(nativeResponses,sheetResponses);
    const decisions = organized ? readOrganizedDecisions(target) : readDecisions(target);
    const result = reconcileContacts(organized ? editableRoster(target) : sourceRoster(source), submissions, decisions);
    assertSubmissionAudit(submissions,result);
    if (organized) writeEditableContacts(target,result);
    const masterHeader = ['roster_id','first_name','last_name','preferred_name','email','phone',
      'reunion_interest','committee_interest','field_sources'];
    writeGenerated(target,'Master Contacts',masterHeader,result.master.map(r =>
      [r.id,r.first,r.last,r.preferred,r.email,r.phone,r.reunion,r.committee,JSON.stringify(r.sources)]));
    if (organized) writeOrganizedReview(target,result,decisions);
    else writeGenerated(target,organized ? 'Needs Review' : 'Match Review',['response_id','roster_id','reason','first_name','last_name','preferred_name',
      'email','phone','reunion_interest','committee_interest'],result.review.map(r =>
      [r.response_id,r.roster_id,r.reason,r.first,r.last,r.preferred,r.email,r.phone,r.reunion,r.committee]));
    writeGenerated(target,'Match Audit',['response_id','roster_id','status'],result.audit.map(r =>
      [r.response_id,r.roster_id || '',r.status]));
    const updated = new Date().toISOString();
    // Public Export contains ONLY the allowlisted public fields.
    writeGenerated(target,'Public Export',['first_name','last_name','email_bool','updated_at'],
      result.publicRows.map(r => [r.first_name,r.last_name,r.email_bool,updated]));
    // Separate file: GitHub's reader cannot access the private contact workbook.
    writeGenerated(SpreadsheetApp.openById(TOHS_EXPORT_ID),'Public Export',
      ['first_name','last_name','email_bool','updated_at'],
      result.publicRows.map(r => [r.first_name,r.last_name,r.email_bool,updated]));
    writeGenerated(target,'Workflow Status',['key','value'],[['last_success',updated],
      ['roster_count',result.master.length],['review_count',result.review.length],
      ['form_url',form.getPublishedUrl()],['native_response_count',nativeResponses.length],
      ['sheet_only_response_count',sheetResponses.length],['audited_submission_count',result.audit.length]]);
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
      const item = form.addTextItem().setTitle(title).setRequired([0,1,3].includes(i));
      if (i === 3) item.setValidation(FormApp.createTextValidation().requireTextIsEmail().build());
    }
  }
  if (form.getItems().map(i => i.getTitle()).join('|') !== TOHS_QUESTIONS.join('|'))
    throw new Error('Partial or changed Form schema: inspect before continuing');
  form.setPublishingSummary(false).setCollectEmail(false).setLimitOneResponsePerUser(false)
    .setAllowResponseEdits(false).setConfirmationMessage('Thank you! Your update has been received.');
  let destinationId = '';
  try { destinationId = form.getDestinationId(); } catch (error) {
    if (!/no response destination/i.test(error.message)) throw error;
  }
  if (destinationId !== TOHS_WORKFLOW_ID)
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

if (typeof module !== 'undefined') module.exports = {reconcileContacts,literalCell,nameKey,
  responseSheetSubmissions,assertSubmissionAudit};


// Contacts is the only editable roster; generated sheets are views of it.
const TOHS_CONTACT_HEADERS = ['Roster ID','First Name','Graduation Last Name','Current/Preferred Name','Primary Email','Phone','Reunion Interest','Committee Interest','Field Sources','Contact Status','Alternate Email','Last Contacted','Last Verified','Contact Source','Notes'];
function editableRoster(ss) {
  const rows = ss.getSheetByName('Contacts').getDataRange().getDisplayValues();
  if (TOHS_CONTACT_HEADERS.some((h,i) => rows[0][i] !== h)) throw new Error('Contacts schema changed');
  const roster = rows.slice(1).filter(r => textValue(r[0])).map(r => ({id:textValue(r[0]),first:textValue(r[1]),last:textValue(r[2]),preferred:textValue(r[3]),email:textValue(r[4]),phone:textValue(r[5]),reunion:textValue(r[6]),committee:textValue(r[7]),sources:JSON.parse(r[8] || '{}')}));
  const baseline = ss.getSheetByName('Master Contacts');
  if (baseline) {
    const ids = baseline.getDataRange().getDisplayValues().slice(1).map(r=>textValue(r[0])).filter(Boolean);
    if (ids.length && (ids.length !== roster.length || ids.some(id=>!roster.some(r=>r.id===id)))) throw new Error('Contacts roster IDs changed; review before publishing');
  }
  if (roster.some(r=>r.email && !validEmail(r.email))) throw new Error('Invalid primary email in Contacts');
  return roster;
}
function organizedLegacySubmissions(ss) {
  const rows = ss.getSheetByName('Legacy Intake').getDataRange().getDisplayValues();
  const fields = ['response_id','timestamp','first','last','preferred','email','phone','reunion','committee'];
  if (fields.some((f,i)=>rows[0][i]!==f)) throw new Error('Legacy intake schema changed');
  return rows.slice(1).filter(r=>textValue(r[0])).map(r=>Object.fromEntries(fields.map((f,i)=>[f,textValue(r[i])])));
}
function writeEditableContacts(ss, result) {
  const sheet = ss.getSheetByName('Contacts');
  const previous = sheet.getDataRange().getValues().slice(1);
  const extras = new Map(previous.map(r=>[textValue(r[0]),Array.from({length:5},(_,i)=>r[i+10] == null ? '' : r[i+10])]));
  const pending = new Set(result.review.map(r=>textValue(r.roster_id)).filter(Boolean));
  const rows = result.master.map(r => [r.id,r.first,r.last,r.preferred,r.email,r.phone,r.reunion,r.committee,JSON.stringify(r.sources),pending.has(r.id)?'Needs review':r.email?'Email collected':'Missing email',...(extras.get(r.id)||['','','','',''])]);
  sheet.getRange(2,1,rows.length,15).setValues(rows.map(row=>row.map(literalCell)));
}

function readOrganizedDecisions(ss) {
  const decisions = readDecisions(ss);
  const sheet = ss.getSheetByName('Needs Review');
  if (!sheet) return decisions;
  for (const r of sheet.getDataRange().getDisplayValues().slice(1)) {
    const action = textValue(r[10]);
    if (!action) continue;
    if (!['Approve fill','Approve replacement','Reject'].includes(action) || !textValue(r[11])) throw new Error('Review action and reviewer note required');
    decisions[textValue(r[0])] = {roster_id:textValue(r[1]),action:action,note:textValue(r[11])};
  }
  return decisions;
}
function writeOrganizedReview(ss, result, decisions) {
  const sheet = ss.getSheetByName('Needs Review');
  const previous = sheet ? sheet.getDataRange().getValues().slice(1) : [];
  const extras = new Map(previous.map(r=>[textValue(r[0]),[r[10] || '',r[11] || '']]));
  writeGenerated(ss,'Review Decisions',['response_id','roster_id','action','reviewer_note'],Object.entries(decisions).map(([id,d])=>[id,d.roster_id,d.action,d.note]));
  writeGenerated(ss,'Needs Review',['Response ID','Roster ID','Reason','First Name','Last Name','Current/Preferred Name','Submitted Email','Phone','Reunion Interest','Committee Interest','Action','Reviewer Note'],result.review.map(r=>[r.response_id,r.roster_id,r.reason,r.first,r.last,r.preferred,r.email,r.phone,r.reunion,r.committee,...(extras.get(r.response_id)||['',''])]));
}
