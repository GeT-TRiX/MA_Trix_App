library(shiny)
shinyApp(
  ui = fluidPage(
    uiOutput("button")
  ),
  server = function(input, output) {
    global <- reactiveValues(clicked = FALSE)
    
    observe({
      if(length(input$RunFullModel)){
        if(input$RunFullModel) global$clicked <- TRUE
      } 
    })
    
    output$button <-  renderUI({
      if(!is.null(input$RunFullModel) & global$clicked){
        actionButton(inputId= "RunFullModel", label =icon("tree-deciduous", lib = "glyphicon"), 
                     style = "color: white; 
                     background-color: #35e51d; 
                     position: relative; 
                     left: 3%;
                     height: 35px;
                     width: 35px;
                     text-align:center;
                     text-indent: -2px;
                     border-radius: 6px;
                     border-width: 2px")        
      }
      else{
        actionButton(inputId= "RunFullModel", label =icon("tree-deciduous", lib = "glyphicon"))
      }
      
    })
    }
    )