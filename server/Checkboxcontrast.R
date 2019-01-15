### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


#################################
######## Select the comparisons #
#################################

# Render in the UI.R the levels for the pData Group 

observe({
  
groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)  
output$comphm <- renderUI(
  checkboxGroupInput(
    inputId = "selcomphm" ,
    label =  "Choose your comparison",
    choices =  colnames(subsetstat()[[1]]),
    #,selected = colnames(subsetstat()[, -1])
    inline = groupinline
  )
)
})


#Select all the contrasts

observeEvent(input$allcomphm, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "selcomphm",
    label = "Choose your comparison",
    choices = colnames(subsetstat()[[1]]),
    selected = colnames(subsetstat()[[1]]),
    inline = groupinline
  )
})

#Unselect all the contrasts
observeEvent(input$nocomphm, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(session,
                           "selcomphm",
                           label = "Choose your comparison",
                           choices = colnames(subsetstat()[[1]]),
                           inline= groupinline
                           )
})


#' selected_test is an eventreactive function in the aim of selecting different comparison after a clickable event
#'
#' @param selcomphm input id corresponding to the checkboxgroup for the different comparisons
#'
#' @return  a reactive value of type character for the different comparisons selected
#'
#' @export

selected_test <- reactive({
  return(input$selcomphm)
})








