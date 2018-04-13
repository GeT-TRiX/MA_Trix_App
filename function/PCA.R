require(FactoMineR)
require(factoextra)


PCAplot <- function(workingset, scale = F) {
  PCAres = PCA(transpose(workingset[, -1]),
               scale.unit = F,
               graph = F)
  
  return(PCAres)
}

eboulis <- function(PCAres){
  myvar = PCAres$eig[, 2]
  barplot(
    myvar,
    names.arg = 1:nrow(PCAres$eig),
    main = "ACP sur les individus",
    ylab = "% variance",
    xlab = "axes"
  )
}


