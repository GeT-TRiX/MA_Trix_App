source("function/heatmtruncated.R")
source("function/cutheat.R")
source("function/formating.R")
#source("function/compat.R")
library(dplyr)

musmuscu <- read.csv2("data/TOXA_HEGU_MA0191_AllChip_WorkingSet.csv")
length(musmuscu)
class(musmuscu)
typeof(musmuscu$X)
pval <- read.csv2("data/All_topTableAll.csv")
View(pval)
write.csv2(head(musmuscu[1:8]),row.names = F, digits=2)
write.table(format(head(musmuscu[1:8]), digits=4),  sep=';',row.names=F)
write.table(format(head(pval[1:6]), digits=4),  sep=';',row.names=F)

groupss <- read.csv2("data/TOXA_HEGU_MA0191_AllChip_pData.csv", sep= ";" , dec = ",",header= T)
class(groupss)
typeof(groupss)
View(head(musmuscu))
View(pval)
adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|^adj.P.Val_LKO_CTRL.LWT_CTRL", names(pval), value=TRUE)]
#adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.", names(pval), value=TRUE)]

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
exprData=treated[[2]][treated[[1]],]
View(exprData)
ColvOrd = exprData %>%
  t() %>%
  distcor()
print(ColvOrd)
row.names(musmuscu) = musmuscu$X


hmbis = truncatedhat(treated[[2]],treated[[1]],groupss$Grp,workingPath=wd_path)
View(hmbis)
testos = c("green","red","orange","blue")
x11()
hm01 = plotHeatmaps(hmbis[[1]],treated[[1]],groupss$Grp,workingPath=wd_path,mypal = testos,
                    showcol = F, showrow = T,genename=pval, rowv = hmbis[[4]], ColvOrd = hmbis[[3]],
                    gpcol = hmbis[[5]], gpcolr = hmbis[[6]], distfunTRIX = hmbis[[2]] )

View(hm01)
x11()
hmp01_All= plotHeatmaps(treated[[2]],treated[[1]],groupss$Grp,workingPath=wd_path,mypal = testos,
                        showcol = F, showrow = T,genename=pval)


cut02 = cut(hmp01_All$rowDendrogram, h = 1)
cat("\n -> size of", length(cut02$lower), "sub-dendrograms\n")
print(sapply(cut02$lower, function(x)
  length(labels(x))))

HCgroupsLab = lapply(cut02$lower, function(x)
  labels(x))
print(HCgroupsLab)
View(HCgroupsLab)
print(HCgroupsLab)
print(final)
rev(HCgroupsLab)

exprData=treated[[2]][treated[[1]],]
genename=pval$GeneName

rowIds = genename[treated[[1]]]
View(exprData)
print(treated[[1]])
final = exprData[rev(hmp01_All$rowInd), hmp01_All$colInd]

mygen = row.names(final)
print(mygen)
View(pval)
seq(length(HCgroupsLab))

mycsv = heatmtoclust( hmp01_All, treated[[1]],treated[[2]], pval, h=2)
mycsv = heatmtoclust(cut02, treated[[1]],treated[[2]], pval)
typeof(mycsv)
class(mycsv)
write.csv(mycsv,file ="myclust.csv", row.names = F)
View(mycsv)

HCgroupsLab = lapply(cut02$lower, function(x)
  labels(x))

exprData=treated[[2]][treated[[1]],]
class(treated[[2]])
class(treated[[1]])

View(treated[[2]])
print(treated[[1]])
exprData[rev(hmp01_All$rowInd), hmp01_All$colInd]

final = exprData[rev(hmp01_All$rowInd), hmp01_All$colInd]
View(final)
my_last= as.integer(lapply(seq(length(HCgroupsLab)), function(x)
{return(tail(HCgroupsLab[[x]],1))}))

mygen = as.integer(row.names(final))
View(pval)
test = pval %>%
  select (X,GeneName) %>%
  filter( X %in% mygen) %>%
  left_join(data.frame(X=mygen), . , by="X") %>%
  arrange(-row_number())

i = 1
for(row in 1:nrow(test)){
  if(test$X[row] == my_last[i] ){
    test$cluster[row] = i
    i = i+1
  }
  else
    test$cluster[row] = i
}





test$cluster = c(1,1,2,2,2,2)
View(test)
test$dataframe =NULL


View(test)
print(mygen)
test= filter(pval, X %in% mygen)
View(test)

df <- data.frame(name=letters[1:4], value=c(rep(TRUE, 2), rep(FALSE, 2)))
View(df)
target <- c("b", "c", "a", "d")
left_join(data.frame(name=target),df,by="name")


hmp01_All$rowDendrogram
View(pval)
source("function/cutheat.R")

test = cutHeatmaps(hmp01_All,height = 5, exprData = musmuscu[,-1], groups = groupss$Grp, 
            DEGres = pval[,-1], type = "Boxplot", num =2 )

test
print(test)
draw(test)

#hc02 = as.hclust(hmp01_All$rowDendrogram)
plot(hc02)
x11()
ggplotly(test,600,600)
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

which(pval,rowsum >0 )


adj = pval[,grep("^adj.P.Val.", names(pval), value=TRUE)]

myl = c()
for(i in 1:ncol(adj)){
  myl[[i]] = which(adj[[i]] < 0.05)
}
View(myl)
indexnull = which( sapply(myl ,length) == 0)
print(indexnull)
indexnull= c(1,3)
final = colnames(adj[,-c(indexnull)])

help("colSums")
colnames(adj[final])
colnames(adj[1:indexnull])

#cut02=cut(hmp01_All$rowDendrogram,h=height)
#print(cut02)

