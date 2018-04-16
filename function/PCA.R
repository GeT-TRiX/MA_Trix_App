require(FactoMineR)
require(factoextra)


res.pca <- function(workingset, scale = F) {
  
  myt = transpose(workingset)
  row.names(myt) = colnames(workingset)
  
  PCAres = PCA(myt,
               scale.unit = F,
               graph = F)
  
  return(PCAres)
}


eboulis <- function(PCAres){
  
  p <- fviz_eig(PCAres, addlabels=TRUE, hjust = -0.3, barfill="white", barcolor ="darkblue", linecolor ="red")
  p + theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),panel.border = element_blank(),
            panel.background = element_blank())
  p + labs(title = "Variances - PCA", x = "Principal Components", y = "% of variances")

  return(p)
}

PCAplot <- function(PCAres, myax = myax, elips = T , rep = T , mylevel = groups$Grp){
  

  p <- fviz_mca_ind(PCAres, label= "all", habillage = mylevel, addEllipses= elips ,
                    ellipse.level= 0.8, repel = T, axes = myax, pointsize = 2 )
  p + scale_color_brewer(palette="Dark2")
  p + theme_minimal()
  p + labs(title = "Variances - PCA")
  
  return(p)
}


# Xname=fData(MAtreated_AllChip$WorkingSet)$GeneName
# 
# plotVar(res_splsda_n5,comp=1:2, legend=T,var.names=list(name=Xname),cutoff=0.7,comp.select=1:2)



