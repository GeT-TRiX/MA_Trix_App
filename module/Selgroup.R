#' checkboxElements is a global namespace containing a list of shiny widgets to check group or comparison(s)
#'
#' @param id Shiny id
#'
#' @return Widgets in the gui
#'
#' @export
#'
checkboxElements <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("usercheck")),
    actionButton(
      inputId = ns("allcomphm"),label= "Select all" ,icon = icon("check-square-o"),
      style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
    ),
    actionButton(
      inputId = ns("nocomphm"),label= "Clear selection" ,icon = icon("square-o"),
      style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
    )
  )
}

#' boxChooser is a shiny module which aims is to allows users to select group(s) or comparison(s)
#'
#' @param input Internal
#' @param output Internal
#' @param session Internal
#' @param label Label associate to the widget
#' @param data A Reactive expression containing a character vector of each group(s) or contrast(s)
#' @param group A Reactive dataframe corresponding to the pData
#' @param case A Numeric value
#' @param empty A boolean value (if true all the box are selected)
#'
#' @return A reactive vector containing the selected box
#'
#' @export
#'

boxChooser <- function(input, output, session, label, data, group, case, empty =F) {
  
  isinline <- reactiveValues(inline = F)
  
  observe({
    req(group()[[2]]$Grp)
    isinline$format <- ifelse(length(levels(group()[[2]]$Grp)) >= 6, T, F)
  })

  output$usercheck <- renderUI({
    
    ns <- session$ns
    groupinline <- ifelse(length(levels(group()[[2]]$Grp)) >= 6, T, F)
    case = ifelse(empty,  2, 1)
    checkboxGroupInput(
      ns("box"),
      label,
      choices =data(),
      selected =  switch(case,data(), NULL),
      inline = isinline$format
    )

  })

  #Select all the contrasts
  observeEvent(input$allcomphm, {
    #groupinline = ifelse(length(levels(group()[[2]]$Grp)) >= 6, T, F)
    updateCheckboxGroupInput(
      session,
      "box",
      label ,
      choices = data(),
      selected = data(),
      inline = isinline$format
    )
  })

  #Unselect all the contrasts
  observeEvent(input$nocomphm, {
    #groupinline = ifelse(length(levels(group()[[2]]$Grp)) >= 6, T, F)
    updateCheckboxGroupInput(session,
                             "box",
                             label ,
                             choices = data(),
                             inline= isinline$format
    )
  })



return(reactive({
  switch(case, group()[[2]][group()[[2]]$Grp %in% input$box,], input$box)
  }))
}
