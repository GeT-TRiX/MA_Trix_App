$(document).ready(function () {
  
  function updateGiff(){
  
  Shiny.addCustomMessageHandler('handler1', function (message){
    setTimeout(function() {
    $('#status').delay(message).fadeOut('slow')});
  });
  }
  updateGiff();
});