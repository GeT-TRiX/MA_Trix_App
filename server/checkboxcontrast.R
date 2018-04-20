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

#' Reactive function in the aim of selecting different comparison
#'
#' @param input specific of the comparison data frame
#'
#' @return \string of the different comparisons selected ### à verifier
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




