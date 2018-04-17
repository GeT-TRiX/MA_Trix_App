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

PCAplot <- function(PCAres, myax = c(1,2), elips = T , rep = T , mylevel = groups$Grp,  mylabsize = 4, dispelip = 0.8 , labeled = 'all', pal = brewer.pal(8, "Dark2")){
  

  p <- fviz_mca_ind(PCAres, label= labeled , habillage = mylevel, addEllipses= elips ,
                    ellipse.level= 0.8, repel = rep, axes = myax, pointsize = 2 , labelsize = mylabsize)

  
  return(p + scale_color_manual(values=pal))
}


# Xname=fData(MAtreated_AllChip$WorkingSet)$GeneName
# 
# plotVar(res_splsda_n5,comp=1:2, legend=T,var.names=list(name=Xname),cutoff=0.7,comp.select=1:2)



