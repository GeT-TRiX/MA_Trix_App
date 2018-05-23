source("function/heatmtruncated.R")
source("function/cutheat.R")
source("function/formating.R")
#source("function/compat.R")

# source("http://bioconductor.org/biocLite.R")
#biocLite("topGO")
source("http://bioconductor.org/biocLite.R")
biocLite()

source("http://bioconductor.org/biocLite.R")
biocLite("goseq")
library(goseq)

source("https://bioconductor.org/biocLite.R")
biocLite("DEGseq")

source("https://bioconductor.org/biocLite.R")
biocLite("GO.db")
library(GO.db)

source("https://bioconductor.org/biocLite.R")
biocLite("ALL")

library(dplyr)
library(topGO)
#library(AnnotationDbi)
library(ALL)

source("https://bioconductor.org/biocLite.R")
biocLite("org.Mm.eg.db")
require("org.Mm.eg.db") 




musmuscu <- read.csv2("data/TOXA_HEGU_MA0191_AllChip_WorkingSet.csv")
pval <- read.csv2("data/All_topTableAll.csv")
groupss <- read.csv2("data/TOXA_HEGU_MA0191_AllChip_pData.csv", sep= ";" , dec = ",",header= T)
adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|^adj.P.Val_LKO_CTRL.LWT_CTRL", names(pval), value=TRUE)]
adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.", names(pval), value=TRUE)]
require(Biobase)

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
print(length(levels(groupss$Grp)))
padj = seq(0,by=1.4,length.out = length(levels(groupss$Grp)))
print(padj)

hm01 = plotHeatmaps(hmbis[[1]],treated[[1]],groupss$Grp,workingPath=wd_path,mypal = testos,
                    showcol = F, showrow = T,genename=pval, rowv = hmbis[[4]], ColvOrd = hmbis[[3]],
                    gpcol = hmbis[[5]], gpcolr = hmbis[[6]], distfunTRIX = hmbis[[2]],geneSet = hmbis[[7]] , height = hmbis[[8]])

hmp01_All$rowDendrogram[[2]]
hm01$ProbeName
cut02 = cut(hmp01_All$rowDendrogram, h = hmbis[[8]] )

print(cut02)

print(hm01$rowDendrogram[[2]])
print(hm01$colDendrogram[[1]])
plot(hm01$rowDendrogram)
plot(hm01$rowDendrogram,hang=-1,labels=FALSE,sub=paste("hclust method: ward2\n", subdist),xlab="",main="")
hcgp=rect.hclust(hm01$rowDendrogram,k=2,border="red")



affyLib <- paste(annotation(ALL), "db", sep = ".")
library(package = affyLib, character.only = TRUE)
View(data(ALL))
print(affyLib)


filter(cluster == 1) 
genlist <- hm01[! duplicated(hm01[ 2]), ]
genlist <- genlist %>% dplyr::select(cluster,GeneName)  %>%`rownames<-`((genlist$GeneName)) 

#final = as.integer(genlist$cluster)
length(genlist$cluster)
final = as.double(matrix(0, length(genlist$cluster)))
names(final) = (row.names(genlist))

print(length(final))
is(final)
View(final)
require("org.Mm.eg.db")

all_genes <- rownames(exprs(as.character((genList))))

affyLib <- paste(annotation(final), "db", sep = ".")
library(package = "org.Mm.eg.db", character.only = TRUE)
print(affyLib)

affyLib <- paste(annotation (ALL), "db", sep = ".")
library(package = affyLib, character.only = TRUE)
data(geneList)

genlist$pvalue = 0.001

genons = genlist$pvalue
names(genons) <- as.character(genlist$GeneName)
View(genons)
typeof(genlist$GeneName)
typeof(genons)
class(genons)
View(genlist)
class(genlist$cluster)

typeof(final)
class(final)

View(final)
print(topDiffGenes)
sum(topDiffGenes(geneList))
affyLib = ""
View(genons)
onts = c( "MF", "BP", "CC" )
topDiffGenes
library("org.Mm.eg.db")
View(org.Mm.eg.db)
names(org.Mm.eg.db)
View(final)
data("geneList")
length(intersect(names(geneList), final))
str(factor(final))
final <- factor(final)
View(final)

final = as.double(matrix(0.001, length(genlist$cluster)))
names(final) = (row.names(genlist))
final[1] <- 0.02
final
final <- factor(final)
final

for(i in 1:3){
  
sampleGOdata <- new("topGOdata",
    ontology = onts[i],
  allGenes = (final), 
   nodeSize = 5,geneSel = topDiffGenes,
  annot=annFUN.org, mapping="org.Mm.eg.db", ID = "ensembl" ) 
}
str(final)
final
print(affyLib)
print(geneSelectionFun)
library(topGO)
go2genes <- annFUN.gene2GO(whichOnto = "MF", gene2GO = as.vector(genlist$GeneName))




myInterestingGenes <- as.vector(genlist$GeneName)




go.obj <- new("topGOdata", ontology="MF",
               allGenes = final
              ,geneSel = topDiffGenes,annot=annFUN.gene2GO,
              gene2GO="org.Mm.eg.db"
)




pwf <- final %>% nullp("mm9", "geneSymbol") %>% na.omit()
finalons <-

test <- goseq(pwf, "mm9" , "geneSymbol", use_genes_without_cat = T)


Definition(test[[1]])

enriched.GO = test$category[p.adjust(test$over_represented_pvalue, method = "BH") < 0.05]
head(enriched.GO)
GOID(test[[1]])


test$category[1:10]

library(GO.db)
for(go in test$category){
  print(go)
  print(Definition(test[[1]][[go]]))
}

test <- as.list(GOTERM)
Term(test[[1]][1])



GOTERM[['GO:0005575']]
View(GOTERM)

paste(clusterlist[[4]]$term, sep=",")

cat(paste(shQuote(paste(clusterlist[[4]]$category,clusterlist[[4]]$term), type="sh"), collapse="\n"), file=con)

View(clusterlist[[1]])

for (i in 1:length(clusterlist)){
  clusterlist[[i]] = arrange(clusterlist[[i]], desc(numInCat))
}
View(clusterlist[[3]])

#pwf = nullp(final,"mm9", "geneSymbol")

test <- final %>% nullp("mm9", "geneSymbol") %>%
  na.omit()
View(test)
pwf[,1]

View(pwf)
pwf_1 <<- pwf[which(pwf[,1] == 1),]
View(final)
finaled <- goseq(final,"mm9" , "geneSymbol",use_genes_without_cat =T)
View(finaled)
print(finaled)
#mettre une liste de species
#

select()

finaled$category

Species <- reactive({ 
  if(input$Genome == "hg19"){ # human
    require("org.Hs.eg.db")
  }
  else if (input$Genome == "mm9"){ # mouse
    require("org.Mm.eg.db")
  }
  else if(input$Genome == "danRer6"){ #Zebra fish
    require("org.Dr.eg.db")
  }
  else if(input$Genome == "galGal3"){ # chicken
    require("org.Gg.eg.db")  
  }
  else if(input$Genome == "equCab2"){ # horse
    require("org.Gg.eg.db")  
  }
  else if(input$Genome == "ce6"){ # cC elegans
    require("org.Gg.eg.db")  
  }
  else if(input$Genome == "rn4"){ # Rat
    require("org.Gg.eg.db")  
  }
  else if(input$Genome == "Pig"){ # Rat
    require("org.Ss.e")  
  }
  else if(input$Genome == "rn4"){ # Rat
    require("org.Gg.eg.db")  
  }
  else if(input$Genome == "rn4"){ # Rat
    require("org.Gg.eg.db")  
  }
})


clusterlist <- list()

for (i in 1:NROW(unique(hm01$cluster))) {
  print(i)
}

lapply(NROW(unique(hm01$cluster), FUN = function(x)
  return(x)))

lapply(1:NROW(unique(hm01$cluster)), function(x)
{return(x)})

library(parallel)

parallel::detectCores()
no_cores <- detectCores()
cl <- makeCluster(no_cores)

registerDoParallel(no_cores)


#tested <- lapply(1:4, function(i){
#system.time({
for (i in 1:NROW(unique(hm01$cluster))) {

  genlist <- hm01[!duplicated(hm01[2]),]
  genlist$DEgenes = 1
  genlist$bias.data = 100
  genlist$pwf = 0.01
  row.names(genlist)= genlist$GeneName
  pwf <-genlist %>% dplyr::select(cluster,GeneName,DEgenes, bias.data,pwf)   %>% filter(cluster == i)
  row.names(pwf)= pwf$GeneName
  
  clusterlist[[i]] <- goseq(pwf[,-1,-2], "mm9" , "geneSymbol", use_genes_without_cat = T)
  
  #clusterlist[[i]] = filter(finalons, numInCat >4 ) %>%
    #arrange(desc(numInCat))
  #return(clusterlist)
}

length(test[[1]]$category)


test= gosearch(hm01,"mm9", "geneSymbol",5,35)


for (i in 1:NROW(unique(hm01$cluster))) {
  
  genlist <- hm01[!duplicated(hm01[2]),]
  genlist <- genlist %>% dplyr::select(cluster, GeneName)   %>% filter(cluster == i)
  final = as.double(matrix(1, length(genlist$cluster)))
  names(final) = (genlist$GeneName)
  pwf <- final %>% nullp("mm9", "geneSymbol") %>% na.omit()
  finalons <-goseq(pwf, "mm9" , "geneSymbol", use_genes_without_cat = F)
  clusterlist[[i]] = filter(finalons, numInCat >5 ) %>%
    arrange(desc(numInCat))
  
}

View(clusterlist)
wclust(clusterlist , "toast.txt",top=1,min=5)


genlist <- hm01[!duplicated(hm01[2]),]
genlist2 <- genlist %>% dplyr::select(cluster, GeneName)   %>% filter(cluster == 4)
View(genlist2)
final = as.double(matrix(1, length(genlist2$cluster)))
names(final) = (genlist2$GeneName)

pwf <- final %>% nullp("mm9", "geneSymbol") %>% na.omit()
View(pwf)
finalons <-goseq(pwf, "mm9" , "geneSymbol", use_genes_without_cat = T)
View(finalons)

View(clusterlist[[1]]$category)
genlist <- hm01[!duplicated(hm01[2]),]
genlist$DEgenes = 1
genlist$bias.data = 100
genlist$pwf = 0.01


length(final)
genlist <- hm01[!duplicated(hm01[2]),]
genlist <-genlist %>% dplyr::select(cluster, GeneName)   %>% filter(cluster == 1) 
row.names(genlist)= genlist$GeneName
genlist$DEgenes = 1
genlist$bias.data = 100
genlist$pwf = 0.01
View(pwf)
colnames(pwf)
pwf <- genlist[3:5]
View(polo)
View(genlist)[3:6]
goseq(genlist[3:6])

test <- goseq(pwf,"mm9", "geneSymbol")
View(test)



pwf <- final %>% nullp("mm9", "geneSymbol",plot.fit = F) %>% na.omit()
finalons <-
  goseq(pwf, "mm9" , "geneSymbol", use_genes_without_cat = T)
View(finalons)

final = as.double(matrix(1, length(genlist$cluster)))
names(final) = (genlist$GeneName)
pwf <- final %>% nullp("mm9", "geneSymbol",plot.fit = F) %>% na.omit()
View(pwf)
View(final)

finalons <- goseq(pwf, "mm9" , "geneSymbol", use_genes_without_cat = T)


cat("Clustering", file="test.txt")
sink("test.txt")
sink()
con <- file("test.txt", "w")

for (i in 1:NROW(clusterlist)){
#mclapply(1:NROW(clusterlist),mc.cores = 8,function(i){
 
  if(!i==1)
    cat("--------------------------------------\n",file=con)
  cat(paste("cluster:", i),  file=con)
  if(!i==1)
    cat("\n--------------------------------------\n",file=con)
  cat("\n--------------------------------------\n",file=con)


  if(!length(clusterlist[[1]][[i]]) == 0) {
    for(go in 1:length(clusterlist[[i]][[1]])) {
      cat(paste("GOID:",as.character(GOID(clusterlist[[i]][[1]][[go]]))),file=con)
      cat("\n",file=con)
      cat(paste("Term:",as.character(Term(clusterlist[[i]][[1]][[go]]))),file=con)
      cat("\n",file=con)
      cat(paste("Definition:",as.character(Definition(clusterlist[[i]][[1]][[go]]))),file=con)
      cat("\n",file=con)
      cat("--------------------------------------\n",file=con)
    }
    cat("\n",file=con)
  }
}

close(con)


x <-replicate(10000, list(rnorm(24)) )  # list length 10000 
library(zoo)
library(parallel)
system.time({
  tmpp = lapply(x, function(x) {
    rollmean(x, 7)
  })
})
#   user  system elapsed 
#  8.250   0.044   8.249 
parallel::detectCores()
#[1] 8
system.time({
  tmpp = mclapply(x, mc.cores=8, function(z) {
    rollmean(z, 7)
  })
})

library(foreach)
library(doParallel)
nocore <- detectCores()

cl<- makeCluster(nocore)
registerDoParallel(nocore)




grp1 = foreach(i = iter(adj[elem], by = "col"), .combine = c) %dopar%  
  (sign= {i < pv}) %>%
  as.data.frame() %>%
  filter(. == T) %>%
  nrow()

return(grp1) 

