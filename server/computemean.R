###############################
######## Adding mean by group #
###############################

#' Reactive function which aim is to return the user's input
#'
#' @param input$meangrp character depending on the user's choice to compute the mean for each different groups in the heatmap
#'
#' @return a reactive value of type string/char 
#'


output$value <- renderText({
  input$meangrp
})

#' mean_grp is a reactive function which aim is to return the output user in order to show this input in the UI
#'
#' @param ouput character depending on the user's choice
#'
#' @return mean_grp a reactive value of type character depending on the user's input
#'
#' @export

mean_grp <- reactive({
  return(output$value)
})


myalgo <- reactive({
  return(input$algomet)
  
})


