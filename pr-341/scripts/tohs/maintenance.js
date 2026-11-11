// Existing manual maintenance helpers; never invoked by deployment.
function removeInterestQuestions() {
 const form=FormApp.openById(PropertiesService.getScriptProperties().getProperty('TOHS_FORM_ID'));
 const removed=form.getItems().filter(i=>['Reunion interest','Planning-committee interest'].includes(i.getTitle()));
 if(removed.length && form.getResponses().length) throw new Error('Archive existing new-Form answers before removing questions');
 removed.reverse().forEach(i=>form.deleteItem(i));
 installContactWorkflow();
 console.log('Five-question reunion Form saved; historical Sheets untouched');
}
function removeObsoleteAcceptanceTrigger() {
  for (const trigger of ScriptApp.getProjectTriggers()) {
    if (trigger.getHandlerFunction() === 'onAcceptanceSubmit') ScriptApp.deleteTrigger(trigger);
  }
  refreshContactWorkflow();
}
