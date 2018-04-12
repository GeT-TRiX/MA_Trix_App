#################################
######## Select the comparisons #
#################################


output$testout <- renderUI(
  checkboxGroupInput(
    inputId = "test" ,
    label =  "Choose your comparison",
    choices =  colnames(adjusted()[[1]][,-1])
    #,selected = colnames(adjusted()[, -1])
    
  )
)

observeEvent(input$allTests, {
  updateCheckboxGroupInput(
    session,
    "test",
    label = "Choose your comparison",
    choices = colnames(adjusted()[[1]][,-1]),
    selected = colnames(adjusted()[[1]][,-1])
  )
})

observeEvent(input$noTests, {
  updateCheckboxGroupInput(session,
                           "test",
                           label = "Choose your comparison",
                           choices = colnames(adjusted()[[1]][, -1]))
})