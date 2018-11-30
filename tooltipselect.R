library("shiny")
library("shinydashboard")
library("shinyBS")

ui <- fluidPage(
  box(
    title = uiOutput("title"),
    selectInput(
      inputId = "SELECT",
      label = NULL,
      choices = c("Option1" = "Option1",
                  "Option2" = "Option2"
      ),
      multiple = FALSE
    )
  )
)
server <- function(input, output, session){
  TIP <- reactiveValues()
  observe({
    TIP$a <- ifelse(input$SELECT =="Option1","Hello World","Hello Mars")
  })
  
  
  output$title <- renderUI({span("My box",
                                 tipify(el = icon(name = "info-circle", lib = "font-awesome"), title = TIP$a))})
  
  
}
shinyApp(ui, server)