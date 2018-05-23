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
myval <- hm01 %>% dplyr::select(GeneName, cluster) %>% filter(cluster == 1) 

print(myval)
View(myval)
what <- hm01$ProbeName
print(what)
as.character(what)
david <- DAVIDWebService$new(email = "franck.soubes@inra.fr", url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")


setCurrentSpecies(david,as.numeric("hg19"))


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
    idType = "GENE",
    listName = "Gene",
    listType = "Gene")
getSpecieNames(david)

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

idType =AGILENT_CHIP_ID , AGILENT_ID

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

pack <- "MmAgilentDesign026655.db" ## more genes
x <- "mgug4122a.db" ##less genes
x <- "mgug4122a.db"

# Get the entrez gene identifiers that are mapped to a gene symbol
mapped_genes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_genes])


enrpck = gsub(".db",'',pack) 



require(dplyr)
myval <- hm01 %>% dplyr::select(ProbeName, cluster) %>% filter(cluster == 1) 




selectedEntrezIds <- unlist(mget(rownames(smPV),hgu95av2ENTREZID)) # source
# https://www.bioconductor.org/help/course-materials/2010/SeattleJan10/day2/GeneSetEnrichment.pdf


entrezids <- as.character(myval$ProbeName) %>%
  mget( MmAgilentDesign026655ENTREZID , ifnotfound=NA) %>%
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
