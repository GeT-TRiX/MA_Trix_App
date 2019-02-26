checkboxRender <- function(id) {
  ns <- NS(id)
  uiOutput(ns("usercheck"))
}

allBox <- function(id, label = "Select all"){
  ns <- NS(id)
  actionButton(
    inputId = ns("allcomphm"),label ,icon = icon("check-square-o"),
    style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
  )
}

noBox <- function(id, label = "Clear selection"){
  ns <- NS(id)
  actionButton(
    inputId = ns("nocomphm"),label ,icon = icon("square-o"),
    style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
  )
}

boxChooser <- function(input, output, session, label, data, group) {
  output$usercheck <- renderUI({
    ns <- session$ns
    checkboxGroupInput(
      ns("box"),
      label,
      choices =  colnames(data()),
      inline = ifelse(length(levels(group()$Grp)) > 6, T, F)
    )
    
  })
  
  #Select all the contrasts
  
  observeEvent(input$allcomphm, {
    groupinline = ifelse(length(levels(group()$Grp)) > 6, T, F)
    updateCheckboxGroupInput(
      session,
      "box",
      label ,
      choices = colnames(data()),
      selected = colnames(data()),
      inline = groupinline
    )
  })
  
  #Unselect all the contrasts
  observeEvent(input$nocomphm, {
    groupinline = ifelse(length(levels(group()$Grp)) > 6, T, F)
    updateCheckboxGroupInput(session,
                             "box",
                             label , 
                             choices = colnames(data()),
                             inline= groupinline
    )
  })
  
  return(reactive({
    input$selcomphm
  }))
}