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

boxChooser <- function(input, output, session, label, data, group, case, Venn =F) {
  
  output$usercheck <- renderUI({
    ns <- session$ns
    checkboxGroupInput(
      ns("box"),
      label,
      choices =data(),
      selected = switch(case,data(), NULL),
      inline = ifelse(length(levels(group()[[2]]$Grp)) > 6, T, F)
    )
    
  })
  
  #Select all the contrasts
  
  observeEvent(input$allcomphm, {
    groupinline = ifelse(length(levels(group()[[2]]$Grp)) > 6, T, F)
    updateCheckboxGroupInput(
      session,
      "box",
      label ,
      choices = data(),
      selected = data(),
      inline = groupinline
    )
  })
  
  #Unselect all the contrasts
  observeEvent(input$nocomphm, {
    groupinline = ifelse(length(levels(group()[[2]]$Grp)) > 6, T, F)
    updateCheckboxGroupInput(session,
                             "box",
                             label , 
                             choices = data(),
                             inline= groupinline
    )
  })
  
 
  
return(reactive({
  switch(case, group()[[2]][group()[[2]]$Grp %in% input$box,], input$box)
  }))
}