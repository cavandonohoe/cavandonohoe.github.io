function installAcceptanceTest() {
  const p=PropertiesService.getScriptProperties();
  let id=p.getProperty('TOHS_ACCEPTANCE_FORM_ID');
  if (!id) {
    const f=FormApp.create('TEST ONLY — TOHS Meghan acceptance',false);
    p.setProperty('TOHS_ACCEPTANCE_FORM_ID',f.getId()); id=f.getId();
    f.setDescription('Isolated synthetic acceptance test. Does not update production contacts.');
    TOHS_QUESTIONS.forEach((title,i)=> {
      if(i<5) f.addTextItem().setTitle(title).setRequired([0,1,3].includes(i));
      else f.addMultipleChoiceItem().setTitle(title).setChoiceValues(['Yes','No','Maybe']).setRequired(true);
    });
    f.setDestination(FormApp.DestinationType.SPREADSHEET,TOHS_WORKFLOW_ID);
  }
  const f=FormApp.openById(id);
  if(!ScriptApp.getProjectTriggers().some(t=>t.getHandlerFunction()==='onAcceptanceSubmit'))
    ScriptApp.newTrigger('onAcceptanceSubmit').forForm(f).onFormSubmit().create();
  f.setPublishingSummary(false).setPublished(true).setAcceptingResponses(true);
  console.log(f.getPublishedUrl());
}
function onAcceptanceSubmit() {
  const s=SpreadsheetApp.openById(TOHS_SOURCE_ID),t=SpreadsheetApp.openById(TOHS_WORKFLOW_ID);
  const f=FormApp.openById(PropertiesService.getScriptProperties().getProperty('TOHS_ACCEPTANCE_FORM_ID'));
  const r=reconcileContacts(sourceRoster(s),legacySubmissions(s).concat(formSubmissions(f)),{});
  const m=r.master.find(x=>x.id==='90');
  if(!m || m.email!=='meghan.acceptance@example.invalid') throw new Error('Acceptance match failed');
  writeGenerated(t,'Acceptance Test Master',['roster_id','first_name','last_name','email','phone','test_only'],[[m.id,m.first,m.last,m.email,m.phone,true]]);
  const updated=new Date().toISOString();
  writeGenerated(t,'Acceptance Test Public',['first_name','last_name','email_bool','updated_at'],r.publicRows.map(x=>[x.first_name,x.last_name,x.email_bool,updated]));
  writeGenerated(t,'Acceptance Test Status',['key','value'],[['status','PASS — synthetic isolated test'],['response_count',f.getResponses().length],['coverage',r.publicRows.filter(x=>x.email_bool).length],['master_roster_id',m.id],['production_untouched',true]]);
  SpreadsheetApp.flush();
}
