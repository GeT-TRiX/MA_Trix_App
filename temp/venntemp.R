pval = read.csv2("/home/franck1337/MA_Trix_App/data/All_topTableAll.csv")
adj = pval[,grep("^adj.P.Val_", names(pval), value=TRUE)]

library(gplots)
library(seqinr)
getAnywhere(drawVennDiagram)
gplots::plot.venn(gv)


help(benchmark)

mygensim = matrix(FALSE,nrow(adj),ncol(adj)) # creation d'une matrice binaire
mygensim                  
foreach(i = 1:ncol(adj)) %do% { 
  mygensim[ adj[,i]< 0.05,i]=1 #mat[ligne,colonne]=resultat
  mygensim[ adj[,i]> 0.05,i]=0 
}
View(mygensim)
which(p.mat[["p"]] < alpha)
myl = list()
which(mygensimm$adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL. == 1)
is.null(which(mygensimm[[3]] == 1))
mygensim <- mygensimm[,-3]
View(mygensim)

myl=list()
for(i in 1:ncol(mygensim)){
  myl[[i]] = which(mygensim[[i]] ==1)
}

grid.draw(venn.diagram(x = myl, filename = NULL, scaled = T, category.names = colnames(mygensim),fill = 2:5, alpha = 0.3))

Vstem <- venn(myl)

plot(Vstem, doWeights = TRUE, type = "circles", names= colnames(mygensim))

venn.diagram(myl,filename = NULL)

colnames(mygensim)=colnames(adj)
rownames(mygensim)=pval$ProbeName


typeof(mygensimm)


help("venn.diagram")
venn.plot <- venn(mygensim)
drawVenn

library(limma)
help("vennDiagram")

mygensimm = as.data.frame(mygensim)
final <- vennCounts(mygensim, include=c("both"))

final

g = vennDiagram(final, circle.col = palette, cex = 0.7)
help(vennDiagram)
par(mar=c(1,1,1,1))

draw.quad.venn(mygensim)


venn.diagram(megensim)
#grid.arrange(gTree(children=g), top="Title", bottom="subtitle")

source("conv.R")

mygensim

mygensim[,c(1,3,5)]


