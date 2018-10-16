#Source https://stackoverflow.com/questions/33839543/shiny-server-session-time-out-doesnt-work
# but seems to not work

inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 5000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
    window.close();  //close the window
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 5000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"

#The following html are sourced from https://github.com/aghozlane/shaman/blob/master/css/owncss.R
 
spincss <- "
  #plot-container {
z-index: 0;
position: relative;
}
#loading-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: -1;
margin-top: 33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
.recalculating {
z-index: -2;
background-color: #fff;
}
"

Errorcss <- 
".shiny-output-error { visibility: visible;  color: #3c8dbc;}

.shiny-output-error:before {
color: #3c8dbc;
visibility: visible;
#content: 'An error occurred. Please contact us at franck.soubes@inra.fr'; }
content: 'ok'; }
}
#topgenesvenn .shiny-output-error {

    visibility: visible;
    color: #3c8dbc;
}
"



appCSS <- "
#loading-content {
position: absolute;
background: #182b42;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
top: 30px;
height: 100%;
text-align: center;
color: #FFFFFF;
}
#loading-content-bar {
position: absolute;
background: #182b42;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

gaugeCSS <- "
.html-widget.gauge svg {
    height: 100%;
margin-top: -10px;
margin-bottom: -40px;
}
"


#' addNews is a function that render a pretty table for news
#'
#' @param date 
#' @param title 
#' @param text 
#' @author Amine Ghozlane 
#' Source https://github.com/aghozlane/shaman/blob/master/Rfunctions/Data_Management.R
#'
#' @return
#' @export
#'
#' @examples
#' 
addNews <- function(date ="",title="",text="")
{
  
  
  res=list()
  res$r1 = paste("<b><font size='+1'>",date,"</font></b>", " - ", "<b><font size='+1'>",title,"</font></b><br/>")
  res$r2 = paste("<p><font color='grey'>",text,"</font></p><hr/>")
  
  return(HTML(unlist(res)))
}


#InfoBoxCSS <- "
#.info-box:hover,
#.info-box:hover .info-box-icon {
#background-color: #aaa !important;
#}
#.info-box:active,
#.info-box:active .info-box-icon {
#background-color: #ccc !important;
#}
#"




