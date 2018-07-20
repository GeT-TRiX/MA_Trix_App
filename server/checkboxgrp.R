#################################
######## Select the groups      #
#################################


# Render in the UI.R the levels for the pData Group
observe({
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  
  output$individusel <- renderUI(
    checkboxGroupInput(
      inputId = "indiv" ,
      label =  "Choose your group to visualize",
      # choices =  colnames(csvf()[[1]][,-1]),
      # selected = colnames(csvf()[[1]][,-1])
      choices =  levels(csvf()[[2]]$Grp),
      selected = levels(csvf()[[2]]$Grp),
      inline = groupinline
    )
  )
  
})

# Select all groups

  
observeEvent(input$allIndividus, {
    
    groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
    
    updateCheckboxGroupInput(
      session,
      "indiv",
      label = "Choose your group to visualize",
      #choices = colnames(csvf()[[1]][,-1]),
      #selected = colnames(csvf()[[1]][,-1])
      choices =  levels(csvf()[[2]]$Grp),
      selected = levels(csvf()[[2]]$Grp),
      inline = groupinline
    )
  })


  # Unselect all groups
  observeEvent(input$noIndividus, {
    groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
    updateCheckboxGroupInput(session,
                             "indiv",
                             label = "Choose your group to visualize",
                             #choices = colnames(csvf()[[1]][,-1]))
                             choices =  levels(csvf()[[2]]$Grp),
                             inline = groupinline )
    
  })

#' choix_test is a reactive function in the aim of selecting different groups
#' 
#' @param indiv input id corresponding to the checkboxgroup for the different groups
#'
#' @return  a reactive value of type character for the different groups selected
#'
#' @export

choix_grp <- reactive({
  req(input$indiv)
  
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  
  return(input$indiv)
})



#' list_ind is a reactive function in the aim of having selected groups in a list
#'
#' @param indiv input id corresponding to the checkboxgroup for the different groups
#'
#' @return a reactive list for the different individuals selected
#'
#' @export


list_ind <- reactive({
  return(list(input$indiv))
})



#' new_group is an eventreactive function that select specific groups in the data frame
#' 
#' @param csvf a Data frame corresponding to the pData table
#' @param choix_grp a reactive value of type character for the different groups selected
#'
#' @return new_group an eventreactive factor with the corresponding groups selected
#'
#' @export


new_group <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(), ]
})


#' new_data is a reactive function that aim is to select specific individuals in the data frame
#'
#' @param csvf Data frame corresponding to the Workingset
#' @param new_group a reactive factor with the corresponding groups selected
#'
#' @return new_data a reactive data frame with specific columns depending on the user's choices
#'
#' @export


new_data <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  #subset(csvf()[[1]],select = choix_individus())
  select(csvf()[[1]], as.character(factor(new_group()$X)))
})
