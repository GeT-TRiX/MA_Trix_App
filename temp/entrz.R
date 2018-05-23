musmuscu <- read.csv2("data/TOXA_HEGU_MA0191_AllChip_WorkingSet.csv")
pval <- read.csv2("data/All_topTableAll.csv")
groupss <- read.csv2("data/TOXA_HEGU_MA0191_AllChip_pData.csv", sep= ";" , dec = ",",header= T)
adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|^adj.P.Val_LKO_CTRL.LWT_CTRL", names(pval), value=TRUE)]
adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.", names(pval), value=TRUE)]
require(Biobase)
source("function/heatmtruncated.R")
source("function/cutheat.R")
source("function/formating.R")
library(dplyr)
library("mgug4122a.db")


x <- mgug4122a.db
# Get the entrez gene identifiers that are mapped to a gene symbol
mapped_genes <- mappedkeys(x)
View(mapped_genes)
# Convert to a list
xx <- as.list(x[mapped_genes])


formating = function( adj, musmuscu,pval){
  
  passingval = adj %>%
    apply(2,FUN = function(x){return(x < 0.05)}) %>%
    apply(1,sum) 
  
  passingval = which( passingval > 0)
  cat("Il y a",length(passingval),"gène significatifs")
  
  #row.names(musmuscu) = musmuscu$X
  musmuscu <- data.matrix(musmuscu[,-1])
  
  newlist = list(passingval, musmuscu )
  return(newlist)
}
row.names(musmuscu) = musmuscu$X
treated = formating(adj,musmuscu,pval= 0.05)



hmbis = truncatedhat(treated[[2]],treated[[1]],groupss$Grp,workingPath=wd_path,genename = pval,k=4)





testos = c("green","red","orange","blue")
x11()


hm01 = plotHeatmaps(hmbis[[1]],treated[[1]],groupss$Grp,workingPath=wd_path,mypal = testos,
                    showcol = F, showrow = T,genename=pval, rowv = hmbis[[4]], ColvOrd = hmbis[[3]],
                    gpcol = hmbis[[5]], gpcolr = hmbis[[6]], distfunTRIX = hmbis[[2]],geneSet = hmbis[[7]] , height = hmbis[[8]])


library(dplyr)
test <- hm01  %>% select(GeneName, cluster)%>% filter(cluster == 1)
View(test)

genlist <- hm01[!duplicated(hm01[2]),]
genlist <-genlist %>% select(cluster, GeneName)   %>% filter(cluster == 1)

View(genlist)

hmp01_All$rowDendrogram[[2]]
cut02 = cut(hmp01_All$rowDendrogram, h = hmbis[[8]] )

testas = c("dfd","fdfdf","fddf")
print(testas)
library(GOstats)
library(GO.db)
hsens=mgug4122a.db
print(hsens)
my.symbols <- test$GeneName
print(my.symbols)
cols(mgug4122a.db)

select(hsens,  
       keys = my.symbols, 
       columns = c("ENTREZID", "SYMBOL", "GENEID"), 
       keytype = "SYMBOL")





