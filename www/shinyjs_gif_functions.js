shinyjs.init = function() {
$(window).resize(shinyjs.calcHeight);
}

shinyjs.calcHeight = function() {
Shiny.onInputChange('plotHeight', $(window).height());
}

shinyjs.gifrender = function(params) {
  $(document).ready(function() {
    setTimeout(function() {
     $("#" + params).fadeOut("fast")});
  });
}

shinyjs.gifrandom = function(params) {
var gif = ['banana.gif','dna.gif', 'atom.gif', 'neurons.gif'];
$('.gif').css({'background-image': 'url(' + gif[Math.floor(Math.random() * gif.length)] + ')'});
}
