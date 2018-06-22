#################################
######## Select the comparisons #
#################################

# Render in the UI.R the levels for the pData Group 
observe({
  
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)  

output$testout <- renderUI(
  checkboxGroupInput(
    inputId = "test" ,
    label =  "Choose your comparison",
    choices =  colnames(adjusted()[[1]][,-1]),
    #,selected = colnames(adjusted()[, -1])
    inline = groupinline
  )
)
})

#Select all the contrasts

observeEvent(input$allTests, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "test",
    label = "Choose your comparison",
    choices = colnames(adjusted()[[1]][,-1]),
    selected = colnames(adjusted()[[1]][,-1]),
    inline = groupinline
  )
})

#Unselect all the contrasts
observeEvent(input$noTests, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(session,
                           "test",
                           label = "Choose your comparison",
                           choices = colnames(adjusted()[[1]][, -1]),
                           inline= groupinline
                           )
})

#' choix_test is a reactive function in the aim of selecting different comparison 
#'
#'
#' @return \input`$test` a reactive value of type character for the different comparisons selected 
#'


#choix_test <- eventReactive(input$heatm, {
choix_test <- reactive({
  return(input$test)
})
#, ignoreNULL = F)


# choix_test <- reactive({
#   return(input$test)
# })

  
# output$test <- renderText({
#   my_final <<- paste(choix_test(),as.character(),  sep=",") 
# 
# })




