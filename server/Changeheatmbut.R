### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0

##################################
######## Modify buttons          #  
##################################



#' global is a ReactiveValues function
#'
#' @param clicked bool set to FALSE
#' @param heatm input triggered by click event
#'
#' @return clicked a boolean which can be TRUE or FALSE
#'
#' @export

global <- reactiveValues(clicked = FALSE)

observe({
  if(length(input$heatm)){
    if(input$heatm) global$clicked <- TRUE
  }
})


output$button <-  renderUI({ 
  if(!is.null(input$heatm) & global$clicked){
    shiny::actionButton("heatm", "Update Heatmap", icon = icon("repeat"), style = "color: #fff; background-color: #b77033; border-color: #b77033")
  }
  else{
    shiny::actionButton("heatm", "Print Heatmap", style = "color: #fff; background-color: #337ab7; border-color: #337ab7")
  }

})