#########################################
######## PCA part                       #
#########################################

#' PCAres is a reactive function that computed a PCA of non-normalized data
#'
#' @param csvf a data frame corresponding to the WorkingSet
#'
#' @return \PCAres a reactive data frame with PCA attributes
#'

PCAres <- reactive({
  req(csvf())
  if (is.null(csvf()[[1]]))
    return(NULL)
  
  mypca = res.pca(csvf()[[1]][,-1], scale =F)
  return(mypca)
})


#' Scree_plot is a reactive function which aim is to display the eigenvalues of the data
#'
#' @param PCAres a reactive data frame with PCA attributes
#'
#' @return \Screeplot a reactive plot
#'

Scree_plot <- reactive({
  req(PCAres())
  mybar = eboulis(PCAres())
  return(mybar+theme_classic())
})


output$savescre <- downloadHandler(
  
  filename <- function() {
    paste0(basename(file_path_sans_ext(projectname())), '_screeplot.png', sep='')    
  },
  content <- function(file) {
    
    png(file,
        width =1400,
        height = 1400,
        units = "px",
        pointsize= 12,
        res=100
    )
    
    plot(Scree_plot())
    dev.off()
  })


output$eigpca <- renderPlot({
  
  validate(
    need(csvf(), 'You need to import data to visualize this plot!'))
  
  plot(Scree_plot())
  
}, width = 1200 , height = 600, res = 100)


#' labeled is a reactive function which aim is to display or not the labels in the PCA render plot
#'
#' @param input$label a boolean
#'
#' @return \labeled a reactive  boolean depending of the user's choice to display or not the labels
#'

labeled <- reactive({
  
  if(input$label == T)
    showlab = "all"
  else 
    showlab = "none"
  
  return (showlab)
})


output$PCA <- renderPlot({

  validate(
    need(csvf(), 'You need to import data to visualize this plot!'))
  
  plot(PCAplot())
  
}, width = 1200 , height = 800, res = 100)


output$savepca <- downloadHandler(
  
  filename <- function() {
    paste0(basename(file_path_sans_ext(projectname())), '_pca.png', sep='')    
  },
  content <- function(file) {
    
    png(file,
        width =1400,
        height = 1400,
        units = "px",
        pointsize= 12,
        res=100
    )
    
    plot(PCAplot())
    dev.off()
  })