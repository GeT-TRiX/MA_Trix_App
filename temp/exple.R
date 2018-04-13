library(shiny)

ui <- shinyUI(fluidPage(
  titlePanel("aaaaaaaaaaaaaaaa"),
  tabsetPanel(
    navbarMenu("Means",
               tabPanel("One Mean"),
               tabPanel("Two Means",
                        wellPanel(
                          checkboxInput(inputId = "s1", label = "S1"  , value = FALSE),
                          checkboxInput(inputId = "s2", label = "S2", value = FALSE)
                        ),
                        sidebarPanel(
                          p(strong("Error Rates")),
                          numericInput("alpha", label="Alpha", min=0, max=1,value=0.05),
                          numericInput("power", "Power", 0.8),
                          actionButton("submit","Submit")
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Main",
                                     htmlOutput("header"),
                                     tableOutput("Table"),
                                     htmlOutput("Text")
                            )
                          )
                        )
               )
    ))))

server <- shinyServer(function(input, output) {
  output$header <- renderText({
    if(input$submit > 0) {
      HTML(paste0("<h3>","Numeric Results","</h3>"))
    }
  })
})

shinyApp(ui = ui, server = server)  
  