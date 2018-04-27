#################################
######## Select the groups      #
#################################


# Render in the UI.R the levels for the pData Group 
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
# Select all groups
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

# Unselect all groups
observeEvent(input$noIndividus, {
  updateCheckboxGroupInput(session,
                           "indiv",
                           label = "Choose your group to visualize",
                           #choices = colnames(csvf()[[1]][,-1]))
                           choices =  levels(csvf()[[2]]$Grp))
})

#' Reactive function which aim is to select/unselect groups
#'
#' @param input'$indiv' specific of the individuals data frame
#'
#' @return string/char of the different individuals selected
#'

choix_grp <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  return(input$indiv)
})



#' Reactive function in the aim of having selected groups in a list
#'
#' @param input specific of the individuals data frame
#'
#' @return a list of the different individuals selected
#'


list_ind <- reactive({
  return(list(input$indiv))
})



#' Reactive function that select specific groups in the data frame
#'
#' @param csvf Data frame corresponding to the pData table
#'
#' @return \newgroup a new factor with the corresponding groups 
#'

new_group <- eventReactive(input$heatm, {
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(),]
}, ignoreNULL = F)





#' Reactive function that  select specific individuals in the data frame
#'
#' @param \csvf Data frame corresponding to the Workingset
#'
#' @return \newdata a data frame with specific columns depending on the user's choices
#'


new_data <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  #subset(csvf()[[1]],select = choix_individus())
  select(csvf()[[1]], as.character(factor(new_group()$X)))
})

