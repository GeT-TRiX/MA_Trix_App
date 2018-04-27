source("function/compat.R")
source("function/cutheat.R")
library(dplyr)

musmuscu <- read.csv2("data/TOXA_HEGU_MA0191 _AllChip_WorkingSet.csv")
pval <- read.csv2("data/All_topTableAll.csv")
write.csv2(head(musmuscu[1:8]),row.names = F, digits=2)
write.table(format(head(musmuscu[1:8]), digits=4),  sep=';',row.names=F)
write.table(format(head(pval[1:6]), digits=4),  sep=';',row.names=F)

groupss <- read.csv2("data/TOXA_HEGU_MA0191 _AllChip_pData.csv", sep= ";" , dec = ",",header= T)
View(head(musmuscu))

#adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|^adj.P.Val_LKO_CTRL.LWT_CTRL", names(pval), value=TRUE)]
adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.", names(pval), value=TRUE)]

View(pval)
View(musmuscu)

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
treated = formating(adj,musmuscu,pval= 0.05)
row.names(musmuscu) = musmuscu$X

testos = c("green","red","orange","blue")
x11()
hmp01_All= plotHeatmaps(treated[[2]],treated[[1]],groupss$Grp,workingPath=wd_path,prefix,suffix, mypal = testos,
                        showcol = F, showrow = F,genename=pval$GeneName)


source("function/cutheat.R")

test = cutHeatmaps(hmp01_All,height = 5, exprData = musmuscu[,-1], groups = groupss$Grp, 
            DEGres = pval[,-1], type = "Heatmap", num =1 )
test
print(test)
draw(test)

#hc02 = as.hclust(hmp01_All$rowDendrogram)
plot(hc02)
x11()
ggplotly(test,1200,800)
plot(hmp01_All$rowDendrogram, hang=-1, labels = T, sub = paste("hclust method: Ward2\n", subdist = "dist method 1-cor"),xlab ="",main="")

hts=rev(tail(hmp01_All$rowDendrogram,15))
barplot(hts,names.arg = 1:length(hts))
print(test)
x11()
ggplotly(test,1200,800)
print(sum(test$data$Expression),na.rm=T)
print(test)
Rowv=str(cut02$lower[[2]])

cut02=cut(hmp01_All$rowDendrogram,h=1)
HCgroupsLab=lapply(cut02$lower,function(x)
  print(labels(x)))
  
print(HCgroupsLab)
labels(cut02$lower)

print(labels(hmp01_All$rowDendrogram))
treated[[2]]labels(hmp01_All$rowDendrogram)




#cut02=cut(hmp01_All$rowDendrogram,h=height)
#print(cut02)

