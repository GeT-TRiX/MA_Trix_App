library(shiny);library(shinyjs)

shinyApp(
  ui = fluidPage(
    sidebarLayout(
      sidebarPanel(
        actionButton("showPanel","Show Panel!")
      ),
      mainPanel(
        useShinyjs(),
        absolutePanel(id = "panel", class = "panel panel-default",
                      style = "z-index:100; display: none;",
                      
                      div(style="margin:10px;",
                          h3("Hello World")
                      )
        )
      )
    )
  ),
  server = function(input, output) {
    
    observeEvent(input$showPanel,{
      toggle("panel")
    }) 
    
  }
)