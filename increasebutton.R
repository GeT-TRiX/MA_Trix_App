library(shiny)
library(shinyjs)

ui <- shinyUI(
  fluidPage(
    useShinyjs(),
    sidebarPanel(
      actionButton('btn','Click me')
    ),
    mainPanel(
      verbatimTextOutput("nText")
    )
  )
)

server <- shinyServer(function(input,output,session){
  n <- 0
  makeReactiveBinding('n')
  
  observeEvent(input$btn, { 
    if(n < 5){
      info('Msg')
    } else if(n > 5){
      hide('btn')
    }
    n <<- n + 1 
  })
  
  output$nText <- renderText({
    n
  })
  
})

shinyApp(ui=ui,server=server)