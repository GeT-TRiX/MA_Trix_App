library(shiny)
ui <- shinyUI(
  fluidPage(
    actionButton("btn_change", "Change Values"),
    actionButton("btn_print","Print values")
  )
)

server <- function(input, output) {
  a <- 1
  
  data <- reactiveVal()
  data(a)
  
  observeEvent(input$btn_change, {
    old_value = data()
    data(old_value+1)
  })
  
  your_reactive <- reactive({
    data()*2+1
  })
  
  
  observeEvent(input$btn_print, {
    print(paste("data = ", your_reactive()))
  })
  
}

shinyApp(ui, server)