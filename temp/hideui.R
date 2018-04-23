library(shiny)
runApp(list(ui = fluidPage(
  conditionalPanel(condition='output.bool',
                   HTML("Hello world"),
                   HTML("Hello world")),
  actionButton("btn","Press me to toggle")
),
server = function(input, output, session) {
  value=T
  output$bool <- eventReactive(input$btn,{
    print(value)
    value    
  })
  outputOptions(output,"bool",suspendWhenHidden=F)
  
  observeEvent(input$btn,
               value <<- !value        
  )  
  
}))