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