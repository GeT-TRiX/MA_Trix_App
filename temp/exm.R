### R code from vignette source 'GOstatsForUnsupportedOrganisms.Rnw'

###################################################
### code chunk number 1: available Schemas
###################################################
library("AnnotationForge")
available.dbschemas()


###################################################
### code chunk number 2: Acquire annotation data
###################################################
library("org.Hs.eg.db")
frame = toTable(org.Hs.egGO)
goframeData = data.frame(frame$go_id, frame$Evidence, frame$gene_id)
head(goframeData)


###################################################
### code chunk number 3: transformGOFrame
###################################################
goFrame=GOFrame(goframeData,organism="Homo sapiens")
goAllFrame=GOAllFrame(goFrame)


###################################################
### code chunk number 4: Make GSC
###################################################
library("GSEABase")
gsc <- GeneSetCollection(goAllFrame, setType = GOCollection())


###################################################
### code chunk number 5: <make parameter
###################################################
library("GOstats")
universe = Lkeys(org.Hs.egGO)
genes = universe[1:500]
params <- GSEAGOHyperGParams(name="My Custom GSEA based annot Params", 
                             geneSetCollection=gsc, 
                             geneIds = genes, 
                             universeGeneIds = universe, 
                             ontology = "MF", 
                             pvalueCutoff = 0.05, 
                             conditional = T, 
                             testDirection = "over")


###################################################
### code chunk number 6: call HyperGTest
###################################################
Over <- hyperGTest(params)
View(Over)
head(summary(Over))




