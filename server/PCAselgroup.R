### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0



#########################################
######## PCA part                       #
#########################################

observe({
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  
  output$grpselpca <- renderUI(
    checkboxGroupInput(
      inputId = "groupca" ,
      label = NULL,
      choices =  levels(csvf()[[2]]$Grp),
      inline   = groupinline
    )
  )
})

# Select all groups
observeEvent(input$allgrpca, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "groupca",
    label = "Choose your group to visualize",
    choices =  levels(csvf()[[2]]$Grp),
    selected = levels(csvf()[[2]]$Grp),
    inline = groupinline
  )
})

# Unselect all groups
observeEvent(input$nogrpca, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "groupca",
    label = "Choose your group to visualize",
    choices =  levels(csvf()[[2]]$Grp),
    inline = groupinline
  )
})
  


#' choix_grpca is a reactive function in the aim of selecting different groups
#'
#' @param indivpca a vector input corresponding to the selected groups
#'
#' @return  a reactive value of type character for the different groups selected
#'
#' @export

choix_grpca <- reactive({
  req(csvf(),input$groupca)
  return(input$groupca)
})




#' list_ind is a reactive function in the aim of having selected groups in a list
#'
#' @param input specific of the individuals data frame
#'
#' @return a reactive list for the different individuals selected
#'
#' @export


list_ind <- reactive({
  return(list(input$groupca))
})



#' subsetgroup_pca is a reactive function that select specific groups in the data frame
#' @param heatm  a clickable input button
#' @param csvf a Data frame corresponding to the pData table
#'
#' @return subsetgroup_pca a reactive factor with the corresponding groups selected
#'
#' @export


subsetgroup_pca <- reactive({
  req(csvf())
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grpca(),]
})




#' PCAres is a reactive function that computed a PCA of non-normalized data
#'
#' @param csvf a data frame corresponding to the WorkingSet
#' @param new_datapca a reactive data frame
#'
#' @return PCAres a reactive data frame with PCA attributes
#'
#' @export


PCAres <- reactive({
  req(csvf())
  mypca = res.pca(new_datapca(), scale = F)
  return(mypca)
})


#' Scree_plot is a reactive function which aim is to display the eigenvalues of the data
#'
#' @param PCAres a reactive data frame with PCA attributes
#'
#' @return Screeplot a reactive plot
#'
#' @export

Scree_plot <- reactive({
  req(PCAres())
  mybar = eboulis(PCAres())
  return(mybar + theme_classic())

})



#' new_datapca is a reactive function that aim is to select specific individuals in the data frame
#'
#' @param csvf Data frame corresponding to the Workingset
#'
#' @return new_datapca a reactive data frame
#'
#' @export
#'


new_datapca <- reactive({
  req(csvf())
  select(csvf()[[1]], as.character(factor(subsetgroup_pca()$X)))
})


callModule(downoutputfiles, "downloadplots", projectname = projectname , suffix= "_screeplot." , data = Scree_plot  )



output$eigpca <- renderPlot({
  
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(length(input$groupca) >0 ,'You need to select groups!') %next%
      need(length(unique(
        subsetgroup_pca()$Grp
      )) > 1, 'You need to select more than one group!')
  )

  plot(Scree_plot())

},  height = plotHeight)

js$calcHeight()


#' labeled is a reactive function which aim is to display or not the labels in the PCA render plot
#'
#' @param label a boolean input
#'
#' @return Labeled a reactive  boolean depending of the user's choice to display or not the labels
#'
#' @export

labeled <- reactive({
  if (input$label == T)
    showlab = "all"
  else
    showlab = "none"

  return (showlab)
})


output$PCA <- renderPlot({
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(length(unique(
        subsetgroup_pca()$Grp
      )) > 0, 'You need to select groups!') %next%
      need(length(unique(
        subsetgroup_pca()$Grp
      )) > 1, 'You need to select more than one group!')
  )

  plot(PCAplot() + theme_minimal())

},  height = plotHeight)


output$PCAvarender <- renderPlot({
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(length(unique(
        subsetgroup_pca()$Grp
      )) > 0, 'You need to select groups!') %next%
      need(length(unique(
        subsetgroup_pca()$Grp
      )) > 1, 'You need to select more than one group!')
  )

  plot(PCAvarplot())

},  height = plotHeight)


callModule(downoutputfiles, "savepca", projectname = projectname , suffix= "_pca." , data = PCAplot  )


