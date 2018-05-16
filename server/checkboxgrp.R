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

#' choix_grp is a reactive function which aim is to select/unselect groups
#'
#' @param input'$indiv' specific of the individuals data frame
#'
#' @return a reactive  character value for the different individuals selected
#'

choix_grp <- reactive({
  req(input$indiv)
  
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  
  return(input$indiv)
})



#' list_ind is a reactive function in the aim of having selected groups in a list
#'
#' @param input specific of the individuals data frame
#'
#' @return a reactive list for the different individuals selected
#'


list_ind <- reactive({
  return(list(input$indiv))
})



#' new_group is an eventreactive function that select specific groups in the data frame
#'
#' @param csvf Data frame corresponding to the pData table
#'
#' @return \newgroup an eventreactive factor with the corresponding groups selected
#'

#new_group <- eventReactive(input$heatm, {
new_group <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(),]
})
#}, ignoreNULL = F)





#' new_data is a reactive function that aim is to select specific individuals in the data frame
#'
#' @param \csvf Data frame corresponding to the Workingset
#'
#' @return \newdata a reactive data frame with specific columns depending on the user's choices
#'


new_data <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  #subset(csvf()[[1]],select = choix_individus())
  select(csvf()[[1]], as.character(factor(new_group()$X)))
})

