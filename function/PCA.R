### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#' res.pca is a function that computed a PCA of non-normalized data with the FactoMineR package
#'
#' @param workingset A data frame corresponding to the WorkingSet
#' @param scale A boolean value; by default this value is set to False for non-normalized data
#'
#' @return A data frame with PCA attributes
#'
#' @export

res.pca <- function(workingset, scale = F) {

  myt = transpose(workingset)
  row.names(myt) = colnames(workingset)

  PCAres = PCA(myt,
               scale.unit = F,
               graph = F)

  return(PCAres)
}


#' eboulis is a function which aim is to display the eigenvalues of the data with the package factoextra
#'
#' @param PCAres A data frame with PCA attributes
#'
#' @return A ggplot object
#'
#' @export

eboulis <- function(PCAres){

  p <- fviz_eig(PCAres, addlabels=TRUE, hjust = -0.3, barfill="white", barcolor ="darkblue", linecolor ="red")
  p + theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),panel.border = element_blank(),
            panel.background = element_blank())
  p + labs(title = "Variances - PCA", x = "Principal Components", y = "% of variances")

  return(p)
}

#' PCAplot is a function that return a factoextra object of PCA type
#'
#' @param PCAres A data frame with PCA attributes
#' @param myax A numeric vector of length 2 specifying the dimensions to be plotted
#' @param elips A boolean value to add ellipse to the data distribution  for the different groups; default = False
#' @param rep A boolean value to avoid overlaps between the label points
#' @param mylevel A data frame corresponding to the pData
#' @param mylabsize A numeric value representing the police size to display for the different labels
#' @param dispelip A numeric value representing the ellipsoid dispersion
#' @param labeled A character to display labels and/or points
#' @param pal A color object from the RcolorBrewer package
#'
#' @return A ggplot object
#'
#' @export

PCAplot <- function(PCAres, myax = c(1,2), elips = T , rep = T , mylevel = groups$Grp,  mylabsize = 4, dispelip = 0.8 , labeled = 'all', pal = brewer.pal(8, "Dark2")){


  p <- fviz_mca_ind(PCAres, label= labeled , habillage = mylevel, addEllipses= elips ,
                    ellipse.level= 0.8, repel = rep, axes = myax, pointsize = 2 , labelsize = mylabsize)


  return(p + scale_color_manual(values=pal))
}


# res.pca <- function(workingset, restab ,scale = F, variable = F) {
#
#   myt = transpose(workingset)
#   row.names(myt) = colnames(workingset)
#
#   if(variable)
#     colnames(myt) =  make.names(restab$GeneName, unique=TRUE)
#
#   PCAres = PCA(myt,
#                scale.unit = F,
#                graph = F)
#
#   return(PCAres)
# }










