#' ### Author: Franck Soubès
#' ### Bioinformatics Master Degree - University of Bordeaux, France
#' ### Link: https://github.com/GeT-TRiX/MA_Trix_App/
#' ### Where: GET-TRiX's facility
#' ### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
#' ### Licence: GPL-3.0
#' 
#' 
#' 
#' #########################################
#' ######## PCA part                       #
#' #########################################


subsetgroup_pca <- callModule(boxChooser, "selgrouppca", label = "Choose your group to visualize", data = csvf , group = csvf, case = 1 )


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
      need(length(subsetgroup_pca()) >0 ,'You need to select groups!') %next%
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

