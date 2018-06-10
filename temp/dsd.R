### R code from vignette source 'RDavidWS-vignette.Rnw'
### Encoding: UTF-8

library("org.Hs.eg.db")
library("AnnotationForge")
library("GO.db")
available.dbschemas()

###################################################
### code chunk number 1: General R options for Sweave
###################################################
options(prompt="R> ", continue="+  ", width=70, useFancyQuotes=FALSE, digits=4)

###################################################
### code chunk number 2: Loading library
###################################################
suppressMessages(library("RDAVIDWebService"))


###################################################
### code chunk number 3: TermCluster1
###################################################
library("RDAVIDWebService")
fileName<-system.file("files/termClusterReport1.tab.tar.gz",
                      package="RDAVIDWebService")
untar(fileName)
termCluster<-DAVIDTermCluster(untar(fileName, list=TRUE))
termCluster
head(summary(termCluster))
View(pval)

unique(hm01$ProbeName)
myval <- hm01[[1]] %>% dplyr::select(GeneName, cluster) %>% filter(cluster == 1) 

print(myval)
View(myval)
what <- hm01$ProbeName
print(what)
as.character(what)
david <- DAVIDWebService$new(email = "franck.soubes@inra.fr", url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
RDAVIDWebService::setTimeOut(david, 100000)
help(setTimeOut)
getListName(david)

is.connected(david)
RDAVIDWebService::summary(david)


typeof(as.character(myval$GeneName))

result<-addList(david, entrezids, idType="ENTREZ_GENE_ID", listName="testList", listType="Gene")
selectedSpecie= ("Mus musculus")
backgroundLocation=grep(selectedSpecie,RDAVIDWebService::getBackgroundListNames(david))
specieLocation=grep(selectedSpecie,RDAVIDWebService::getSpecieNames(david))
setCurrentSpecies(object=david, species=specieLocation);setCurrentBackgroundPosition(object=david,position=backgroundLocation)
getSpecieNames(david)
setAnnotationCategories(david,c("GOTERM_MF_ALL") )
mfObject<- as.data.frame(cbind(getFunctionalAnnotationChart(object=david, threshold=1, count=0L)))


entrezids = probnamtoentrez(hm01[[1]],org.Mm.egALIAS2EG)
test = org.Mm.egALIAS2EG
test

  david <- DAVIDWebService$new(email = "franck.soubes@inra.fr", url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
  RDAVIDWebService::setTimeOut(david, 90000)
  result<-addList(david, entrezids[[x]], idType="ENTREZ_GENE_ID", listName="testList", listType="Gene")
  selectedSpecie= ("Mus musculus")
  backgroundLocation=grep(selectedSpecie,RDAVIDWebService::getBackgroundListNames(david))
  specieLocation=grep(selectedSpecie,RDAVIDWebService::getSpecieNames(david))
  setCurrentSpecies(object=david, species=specieLocation);setCurrentBackgroundPosition(object=david,position=backgroundLocation)
  getSpecieNames(david)
  setAnnotationCategories(david,c("GOTERM_MF_ALL") )
  as.data.frame(cbind(getFunctionalAnnotationChart(object=david, threshold=1, count=0L)))  %>%
    filter(Count>1) %>%
    arrange(desc(Count))


mydavfunc = davidquery(entrezids , "Mus musculus")

format(mygodavid[[3]], digits = 3)
library(xlsx)
View(mydavfunc[[1]])


final = lapply(1:NROW(mydavfunc),function(x)
  return(format(mydavfunc[[x]], digits = 3)))


for(i in 1:length(mydavfunc)){
  if(i == 1)
    write.xlsx(file="test.xlsx",final[[i]], sheetName= paste("Cluster", i))
  else
    write.xlsx(file="test.xlsx",final[[i]], sheetName= paste("Cluster", i),  append=TRUE)
}


help("write.xlsx2")
format(mydavfunc[[1]], digits = 2)

test = lapply(1:NROW(mydavfunc),function(x)
  return(format(mydavfunc[[x]], digits = 3)))
View(test[[1]])

as.data.frame.numeric()

View(mydavfunc[[2]][, -c(4,6)])
mytestfunc = mydavfunc[[2]]
View(mytestfunc[,c(4:13)])
View(format(mytestfunc[,c(4:13)], format = "f", digits=3))
mytot = mydavfunc
round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  
  df[,nums] <- round(df[,nums], digits = digits)
  
  (df)
}

final = (round_df(mydavfunc, 5))
View(fixp(mydavfunc[[1]],2))
View(final[[1]])

View(mydavfunc[[1]])
as.numeric(formatC(mydavfunc[[1]], format = "g"))


test = lapply(1:NROW(mydavfunc),function(y)
  return(fixp(mydavfunc[[y]])))

View(format(mfObject, digits = 2))


lapply(1:NROW(mydavfunc),function(y){
  
  format(mydavfunc, digits =2 )
  
})





test <- RDAVIDWebService::getClusterReport(david)
test <- RDAVIDWebService::getBackgroundListNames(david)


selectedSpecie= ("Mus musculus")
backgroundLocation=grep(selectedSpecie,RDAVIDWebService::getBackgroundListNames(david))
specieLocation=grep(selectedSpecie,RDAVIDWebService::getSpecieNames(david))
setCurrentSpecies(object=david, species=specieLocation);setCurrentBackgroundPosition(object=david,position=backgroundLocation)
getSpecieNames(david)
setAnnotationCategories(david,c("GOTERM_MF_ALL") )
mfObject<- as.data.frame(cbind(getFunctionalAnnotationChart(object=david, threshold=1, count=0L)))
View(mfObject)

print(test)


# Set species and backround
selectedSpecie="Escherichia coli str. K-12 substr. MG1655"
backgroundLocation=grep(selectedSpecie,RDAVIDWebService::getBackgroundListNames(david))
specieLocation=grep(selectedSpecie,RDAVIDWebService::getSpecieNames(david))
setCurrentSpecies(object=david, species=specieLocation);
setCurrentBackgroundPosition(object=david,position=backgroundLocation)

sprintf('%.2f', mfObject[c(10:13)])

print(mfObject, digits = 2)
print(mfObject)

round(mfObject, digits = 2)


mfObject %>% mutate_at(13) %>% round(digits = 2)


v

View(mfObject)

print(entrezids)

setCurrentSpecies(david,as.numeric("hg19"))
getSpecieNames(david)

help("addList")
  data("demoList1")
print(demoList1)  
print(as.character(test$GeneName))  
  # a demo gene list comes with 'RDAVIDWebService' package
  # upload this list to david
print(myval$GeneName)
help("addList")
myterm<- addList(
    david,
    myval$GeneName,#demoList1,
    idType = "ENTREZ_GENE_ID",
    listName = "Gene",
    listType = "Gene")




test <- RDAVIDWebService::getClusterReport(david)
View(test)
myval$GeneName
View(myval$GeneName)
cat(unlist(as.character(myval$GeneName)))

test <- RDAVIDWebService::getFunctionalAnnotationChart(david,fileName="davidFunctionalAnnotationChart.tsv")


termCluster <- addList(
  david,
  demoList1,
  idType = "AFFYMETRIX_3PRIME_IVT_ID",
  listName = "demoList1",
  listType = "Gene"
)

termCluster
test <- getClusterReport(david, type = "Term")


plot2D(termCluster, 2)  

idType =AGILENT_CHIP_ID , AGILENT_ID , ENTREZ_GENE_ID

###################################################
### code chunk number 4: plot2Dview
###################################################
clustNumber<-2
toust <- plot2D(termCluster, clustNumber)
print(test[[1]])

###################################################
### code chunk number 5: plotGO
###################################################

davidGODag<-DAVIDGODag(members(termCluster)[[clustNumber]], 
                       pvalueCutoff=0.1, "CC")
plotGOTermGraph(g=goDag(davidGODag),
                r=davidGODag, max.nchar=40, node.shape="ellipse")


###################################################
### code chunk number 6: Session Info
###################################################
sessionInfo()





  data("demoList1") # a demo gene list comes with 'RDAVIDWebService' package
  # upload this list to david
  
   addList(
    david,
    as.character(what),#demoList1,
    #idType = "AFFYMETRIX_3PRIME_IVT_ID",
    idType = "AGILENT_ID",
    listName = "what",
    listType = "Gene")
  
termCluster<-  getClusterReport(david, type = "Term")
print(termCluster)

plot2D(termCluster, 9)
print(test[[3]])

observe({
  req(termCluster())
  # retrieve the number of clusters
  updateSliderInput(session, "clusterNumber", max = nrow(summary(termCluster())))
})
##########
## GOSTATS
##########
library(Biobase)
library(org.Mm.eg.db)
library(KEGG.db)
library(GO.db)
library(GOstats)
library(DBI)
library(AnnotationDbi)
library(MmAgilentDesign026655.db)
library(GOstats)
library(org.Ce.eg.db)



org.Mm.egENSEMBL
org.Ce.egENSEMBL


test = noquote(paste0('MmAgilentDesign026655','ENTREZID',collapse = ""))
unlist(test)
cat(test)


pack <- "MmAgilentDesign026655.db" ## more genes
x <- "mgug4122a.db" ##less genes
x <- "mgug4122a.db"



# Get the entrez gene identifiers that are mapped to a gene symbol
mapped_genes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_genes])


enrpck = gsub(".db",'',pack) 
print(enrpck)
View(MmAgilentDesign026655ENTREZID)
test=c(MmAgilentDesign026655ENTREZID,MmAgilentDesign026655ENTREZID)
View(test[[1]])

org.Mm.egALIAS2EG
mget(x=as.character(myval$GeneName),envir=org.Mm.egALIAS2EG,ifnotfound=NA) %>% unlist() %>%unique() %>% .[!is.na(.)]


probnamtoentrez(hm01[[1]],1,org.Mm.egALIAS2EG)


View(hm01[[1]])
require(dplyr)
myval <- hm01[[1]] %>% dplyr::select(ProbeName,GeneName, cluster) %>% filter(cluster == 1) 

symb = as.symbol(paste0(enrpck, "ENTREZID", collapse = ""))
mget(as.character(myval$ProbeName), MmAgilentDesign026655ENTREZID, ifnotfound=NA)

selectedEntrezIds <- unlist(mget(rownames(smPV),hgu95av2ENTREZID)) # source
# https://www.bioconductor.org/help/course-materials/2010/SeattleJan10/day2/GeneSetEnrichment.pdf
myval$GeneName
library(mgug4121a.db)


entrezids <- as.character(myval$GeneName) %>%
  mget( org.Mm.egENSEMBL , ifnotfound=NA) %>%
  unlist() %>%
  unique()

entrezids=entrezids[!is.na(entrezids)]
print(entrezids)
params <- new('GOHyperGParams', geneIds=entrezids, ontology="BP", pvalueCutoff=0.05, 
              conditional=F, testDirection='over', annotation=enrpck)

params
hgOver <- try(hyperGTest(params))
hgOver


test <- summary(hgOver)
View(test)

mart = useMart("ensembl", dataset="mmusculus_gene_ensembl")
G_list<- getBM(attributes=attributes, filters="hgnc_symbol",values=myval$GeneName,
               mart=mart, uniqueRows=T)

resadj <- as.data.frame(resadj)
res$entrez <- mapIds(org.Mm.eg.db, 
                     keys= as.vector(myval$GeneName), 
                     column="ENTREZID", 
                     keytype="SYMBOL",
                     multiVals="first")
  
  
  
attributes=c('ensembl_gene_id','ensembl_transcript_id','hgnc_symbol')

source("https://bioconductor.org/biocLite.R")
biocLite("AnnotationDbi")



entrezids=entrezids[!is.na(entrezids)]
test <- gosearch(hm01 = hm01, "mm9", "geneSymbol")
params <- new('GOHyperGParams', geneIds=unique(entrezids), ontology="BP", pvalueCutoff=0.05, 
              conditional=F, testDirection='over', annotation=Anpkg)
params
hgOver <- hyperGTest(params)
hgOver
test <- summary(hgOver)
View(test)
