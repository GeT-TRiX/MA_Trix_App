require(VennDiagram)

Vennplot <- function(pval,adj){
  
  myl=list()
  for(i in 1:ncol(adj)){
      myl[[i]] = which(adj[[i]] < 0.05)
  }
  
  indexnull = which( sapply(myl ,length) == 0)
  myl <- myl[sapply(myl, length) > 0]

  g = venn.diagram(x = myl, filename = NULL, scaled = T, 
                   category.names = colnames(adj[,-c(indexnull)]),fill = 2:5, alpha = 0.3)
  
  grid.arrange(gTree(children=g), top="Venn Diagram", bottom="DEG BH 0.05")
  
}

