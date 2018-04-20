require(VennDiagram)

Vennlist <- function(pval,adj){
  myl=list()
  for(i in 1:ncol(adj)){
      myl[[i]] = which(adj[[i]] < 0.05)
  }
  return(myl)
}


Vennfinal <- function(myl,adj, cex ){
  indexnull = which( sapply(myl ,length) == 0)
  myl <- myl[sapply(myl, length) > 0]
  final = length(myl)-1
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  g = venn.diagram(x = myl, filename = NULL, scaled = F, 
                   category.names = colnames(adj[,-c(indexnull)]),fill = 2:(2+final), alpha = 0.3, sub="lol", cex=1, 
                   fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop") # na= stop
  final = grid.arrange(gTree(children=g), top="Venn Diagram", bottom="DEG BH 0.05")
  
  return(final)
}

# pval <- read.csv2("data/All_topTableAll.csv")
# adj = pval[,grep("^adj.P.Val", names(pval), value=TRUE)]
# View(adj[,-1][,-3])
# 
# 
# myven = Vennlist(pval, adj)
# Vennfinal(myven,adj)

