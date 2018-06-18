###############################
########  PCA function        #
###############################


#' PCAplot is a function that return a factoextra object of PCA type
#'
#'@param brew.pal a color object from the RcolorBrewer package
#'@param PCAres a data frame with PCA attributes
#'@param label a boolean value depending of the user's choice to display or not the labels
#'@param habillage a data frame corresponding to the pData
#'@param addEllipseda a boolean value to add ellipse to the data distribution  for the different groups
#'@param ellipse.level a numeric value set to 0.8
#'@param repel a boolean value to avoid overlaps between the label points
#'@param axes a numeric vector of length 2 specifying the dimensions to be plotted
#'@param labelsize a numeric value representing the police size to display for the different labels
#'@param pointsize a numeric value representing the diameter of each points displayed in the graph
#'
#'@return \p a factoextra object
#' 


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
    pointsize = input$pointsiize
  )
  
  return(p + scale_color_manual(values = empty()))
}





