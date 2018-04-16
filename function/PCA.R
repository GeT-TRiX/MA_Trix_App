require(FactoMineR)
require(factoextra)


res.pca <- function(workingset, scale = F) {
  
  PCAres = PCA(transpose(workingset),
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

PCAplot <- function(PCAres){
  firef
  
}


# require(impute)
# 
# PCAdata = as.data.frame(impute.knn(as.matrix(X))$data)
# 

# pca_res=pca(PCAdata,ncomp=2, scale=F)
# 
# png("sPLSDA/All_pca_dim12.png")
# 
# plotIndiv(pca_res,comp=1:2,ind.names=T,group=Y, style="graphics", legend=T, scale=F)
# 
# dev.off()
# 
# Xname=fData(MAtreated_AllChip$WorkingSet)$GeneName
# 
# plotVar(res_splsda_n5,comp=1:2, legend=T,var.names=list(name=Xname),cutoff=0.7,comp.select=1:2)



