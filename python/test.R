library(reticulate)
os <- import("os")
os$listdir(".")
use_python("/usr/local/bin/python")
use_virtualenv("myenv")
source_python('./python/add.py')
hm01$GeneName


test = list("D130043K22Rik","Fads2","Mif","NAP062858-1","Tead2","Fam82a1","Cyp4a13","Mgst3","Slc50a1","Nipal1","Cyp2b10","Mal","Dhrs7","Timp3","Esyt2","Aldob","Ppia","Spc25","Ak2","Rpl10","Gm5623","Fabp2","A_55_P2128224","Lrrn3","Ugp2","Htra2","Atp5g2","Fam13a")



library(Biobase)
library(org.Mm.eg.db)
library(KEGG.db)
library(GO.db)
library(GOstats)
library(DBI)
library(AnnotationDbi)
library(MmAgilentDesign026655.db)
library(GOstats)

pack <- "MmAgilentDesign026655.db" ## more genes



enrpck = gsub(".db",'',pack) 



require(dplyr)
myval <- hm01 %>% dplyr::select(ProbeName, cluster) %>% filter(cluster == 2) 

entrezids <- as.character(myval$ProbeName) %>%
  mget( MmAgilentDesign026655ENTREZID , ifnotfound=NA) %>%
  unlist() %>%
  unique() %>%
  as.list() 

entrezids <- entrezids[lapply(entrezids,function(x) length(grep("NA",x,value=FALSE))) == 0]
entrezids <- entrezids[-1]
print(entrezids)



genlist <- hm01 %>% 
  dplyr::select(cluster, GeneName)   %>% 
  filter(cluster == 2) 


final = genlist$cluster %>%
  length() %>%
  matrix(1,.) %>%
  as.double() 

names(final) = genlist$GeneName 
final = as.list(names(final))

final <- final[lapply(final,function(x) length(grep("chr",x,value=FALSE))) == 0]

enrichmentdav(final)


final = matrix(1, length(genlist$cluster))
print(genlist)
length(genlist$GeneName)
length(genlist$cluster)
final = as.double(matrix(1, length(genlist$cluster)))
print(final)
names(final) = (genlist$GeneName)
test =as.list(names(final))

test <- test[lapply(test,function(x) length(grep("chr",x,value=FALSE))) == 0]
test <- test[-1][-2]


print(test)


enrichmentdav(test)

enrichmentdav(entrezids)


