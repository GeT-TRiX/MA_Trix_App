### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#############################################################################
######## align group name in the pannel if there is more than 6 groups      #
#############################################################################


# Render in the UI.R the levels for the pData Group

observe({
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  
  output$grpselhm <- renderUI(
    checkboxGroupInput(
      inputId = "grouphm" ,
      label =  "Choose your group to visualize",
      choices =  levels(csvf()[[2]]$Grp),
      selected = levels(csvf()[[2]]$Grp),
      inline = groupinline
    )
  )
  
})

# Select all groups
  
observeEvent(input$allgrphm, {
    
    groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
    
    updateCheckboxGroupInput(
      session,
      "grouphm",
      label = "Choose your group to visualize",
      choices =  levels(csvf()[[2]]$Grp),
      selected = levels(csvf()[[2]]$Grp),
      inline = groupinline
    )
  })


  # Unselect all groups

  observeEvent(input$nogrphm, {
    groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
    updateCheckboxGroupInput(session,
                             "grouphm",
                             label = "Choose your group to visualize",
                             choices =  levels(csvf()[[2]]$Grp),
                             inline = groupinline )
    
  })

  
  
#################################
######## Select the groups      #
#################################  
  


#' list_ind is a reactive function in the aim of having selected groups in a list
#'
#' @param indiv input id corresponding to the checkboxgroup for the different groups
#'
#' @return a reactive list for the different individuals selected
#'
#' @export


list_ind <- reactive({
  return(list(input$grouphm))
})



#' subsetgroup_hm is an eventreactive function that select specific groups in the data frame
#' 
#' @param csvf a Data frame corresponding to the pData table
#' @param grouphm an input value of type character for the different groups selected
#'
#' @return subsetgroup_hm an eventreactive factor with the corresponding groups selected
#'
#' @export


subsetgroup_hm <- reactive({
  req(csvf())
  csvf()[[2]][csvf()[[2]]$Grp %in% input$grouphm, ]
})


#' subsetwset is a reactive function that aim is to select specific individuals in the data frame
#'
#' @param csvf Data frame corresponding to the Workingset
#' @param subsetgroup_hm a reactive factor with the corresponding groups selected
#'
#' @return subsetwset a reactive data frame with specific columns depending on the user's choices
#'
#' @export


subsetwset <- reactive({
  req(csvf())
  select(csvf()[[1]], as.character(factor(subsetgroup_hm()$X)))
})
