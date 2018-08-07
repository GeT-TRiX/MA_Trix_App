### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


###############################
########  PCA function        #
###############################


#' PCAplot is a function that return a factoextra object of PCA type
#'
#' @param brew.pal a color object from the RcolorBrewer package
#' @param PCAres a data frame with PCA attributes
#' @param label a boolean value depending of the user's choice to display or not the labels
#' @param habillage a data frame corresponding to the pData
#' @param addEllipseda a boolean input to add ellipse to the data distribution  for the different groups
#' @param ellipse.level a numeric value set to 0.8
#' @param repel a boolean input to avoid overlaps between the label points
#' @param axes a numeric input vector of length 2 specifying the dimensions to be plotted
#' @param labelsize a numeric input representing the police size to display for the different labels
#' @param pointsize a numeric input representing the diameter of each points displayed in the graph
#' @param mean.point a boolean input use to display or not the mean point
#'
#' @return p a factoextra object
#'
#' @export


PCAplot <- function() {
  
  pcapal = brewer.pal(10, "Paired") %>%
    list(brewer.pal(8, "Dark2")) %>%
    unlist()
  
  empty <- reactive ({
    if (is.null(colorspca()[[1]])) {
      palpca = pcapal
    }
    else
      palpca = unlist(colorspca())
    return(palpca)
    
  })
  
  p <- fviz_mca_ind(
    PCAres(),
    label = labeled(),
    habillage = droplevels(new_grouppca()$Grp),
    addEllipses = input$ellipse ,
    ellipse.level = 0.8,
    repel = input$jitter,
    axes = c(as.integer(input$dim1), as.integer(input$dim2)),
    labelsize = input$labelsiize,
    pointsize = input$pointsiize,
    mean.point = input$meanpoint
  )
  
  return(p + scale_color_manual(values = empty()))
}





