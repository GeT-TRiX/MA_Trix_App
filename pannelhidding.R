library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  actionButton("test", label = "test"),
  shinyjs::hidden(wellPanel(id = "panelA", "I AM PANEL A")),
  wellPanel(id="panelB", "I AM PANEL B")
)

server <- function(input,output){
  observeEvent(input$test, {
    shinyjs::toggle(id= "panelA")
    shinyjs::toggle(id= "panelB")
  })
}

shinyApp(ui=ui,server=server)