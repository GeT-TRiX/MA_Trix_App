#################################
######## Select the groups      #
#################################


output$individusel <- renderUI(
  checkboxGroupInput(
    inputId = "indiv" ,
    label =  "Choose your group to visualize",
    # choices =  colnames(csvf()[[1]][,-1]),
    # selected = colnames(csvf()[[1]][,-1])
    choices =  levels(csvf()[[2]]$Grp),
    selected = levels(csvf()[[2]]$Grp)
    
  )
)

observeEvent(input$allIndividus, {
  updateCheckboxGroupInput(
    session,
    "indiv",
    label = "Choose your group to visualize",
    #choices = colnames(csvf()[[1]][,-1]),
    #selected = colnames(csvf()[[1]][,-1])
    choices =  levels(csvf()[[2]]$Grp),
    selected = levels(csvf()[[2]]$Grp)
  )
})

observeEvent(input$noIndividus, {
  updateCheckboxGroupInput(session,
                           "indiv",
                           label = "Choose your group to visualize",
                           #choices = colnames(csvf()[[1]][,-1]))
                           choices =  levels(csvf()[[2]]$Grp))
})