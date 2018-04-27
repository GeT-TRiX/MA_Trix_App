#################################
######## Select the comparisons #
#################################

# Render in the UI.R the levels for the pData Group 
output$testout <- renderUI(
  checkboxGroupInput(
    inputId = "test" ,
    label =  "Choose your comparison",
    choices =  colnames(adjusted()[[1]][,-1])
    #,selected = colnames(adjusted()[, -1])
    
  )
)

#Select all the contrasts
observeEvent(input$allTests, {
  updateCheckboxGroupInput(
    session,
    "test",
    label = "Choose your comparison",
    choices = colnames(adjusted()[[1]][,-1]),
    selected = colnames(adjusted()[[1]][,-1])
  )
})

#Unselect all the contrasts
observeEvent(input$noTests, {
  updateCheckboxGroupInput(session,
                           "test",
                           label = "Choose your comparison",
                           choices = colnames(adjusted()[[1]][, -1]))
})

#' Reactive function in the aim of selecting different comparison 
#'
#'
#' @return \input`$test` a string of the different comparisons selected when clicked on the heatmap button
#'


choix_test <- eventReactive(input$heatm, {
  return(input$test)
}, ignoreNULL = F)


# choix_test <- reactive({
#   return(input$test)
# })

  
# output$test <- renderText({
#   my_final <<- paste(choix_test(),as.character(),  sep=",") 
# 
# })




