require(FactoMineR)
require(factoextra)


#' res.pca is a function that computed a PCA of non-normalized data with the FactoMineR package
#'
#' @param workingset a data frame corresponding to the WorkingSet
#' @param scale a boolean; by default this value is set to False non-normalized data
#'
#' @return \PCAres a data frame with PCA attributes
#' 

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
#' @param PCAresa a data frame with PCA attributes
#'
#' @return \p a factoextra object
#' 

eboulis <- function(PCAres){
  
  p <- fviz_eig(PCAres, addlabels=TRUE, hjust = -0.3, barfill="white", barcolor ="darkblue", linecolor ="red")
  p + theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),panel.border = element_blank(),
            panel.background = element_blank())
  p + labs(title = "Variances - PCA", x = "Principal Components", y = "% of variances")

  return(p)
}

#' PCAplot is a function that return a factoextra object of PCA type
#'
#' @param PCAres a data frame with PCA attributes
#' @param myax a numeric vector of length 2 specifying the dimensions to be plotted
#' @param elips a boolean value to add ellipse to the data distribution  for the different groups; default = False
#' @param rep a boolean value to avoid overlaps between the label points
#' @param mylevel a data frame corresponding to the pData
#' @param mylabsize a numeric value representing the police size to display for the different labels
#' @param dispelip a numeric value representing the ellipsoid dispersion
#' @param labeled a character to display labels and/or points
#' @param pal a color object from the RcolorBrewer package
#'
#' @return
#' 

PCAplot <- function(PCAres, myax = c(1,2), elips = T , rep = T , mylevel = groups$Grp,  mylabsize = 4, dispelip = 0.8 , labeled = 'all', pal = brewer.pal(8, "Dark2")){
  
  
  p <- fviz_mca_ind(PCAres, label= labeled , habillage = mylevel, addEllipses= elips ,
                    ellipse.level= 0.8, repel = rep, axes = myax, pointsize = 2 , labelsize = mylabsize)

  
  return(p + scale_color_manual(values=pal))
}





