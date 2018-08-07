### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



app <- ShinyDriver$new("../", loadTimeout = 1e+05)
app$snapshotInit("mytest")

app$setInputs(sidebarCollapsed = FALSE)
app$snapshot()
app$uploadFile(file = c("All_topTableAll.csv", "TOXA_HEGU_MA0191_AllChip_pData.csv", "TOXA_HEGU_MA0191_AllChip_WorkingSet.csv")) # <-- This should be the path to the file, relative to the app's tests/ directory
app$snapshot()
app$setInputs(pval1 = 0.01)
app$setInputs(method = "None")
Sys.sleep(2)
app$snapshot()
app$setInputs(side = "PCA")
app$setInputs(indivpca = c("LKO_CTRL", "LKO_MCD ", "LWT_CTRL"))
app$setInputs(colpca_2 = "#7570B3")
app$snapshot()
app$setInputs(side = "Venn")
app$setInputs(allCont = "click")
app$setInputs(dispvenn = "genes")
app$setInputs(fcvenn = 2.8)
app$setInputs(fcvenn = 1)
Sys.sleep(2)
app$snapshot()
app$setInputs(fcvenn = 2.7)
Sys.sleep(2)
app$snapshot()
app$setInputs(intscol = "LWT_MCD-LWT_CTRL")
app$setInputs(intscol = c("LWT_MCD-LWT_CTRL", "LKO_MCD-LKO_CTRL"))
app$setInputs(Vennd = "vennbarplotpan")
app$setInputs(topdegenes = "click")
app$snapshot()
app$setInputs(side = "Heatmap")
app$setInputs(test = "LKO_CTRL-LWT_CTRL")
app$setInputs(allTests = "click")
app$setInputs(fc = 2.7)
app$setInputs(pval = 0.03)
app$setInputs(maxgen = 99)
app$setInputs(maxgen = 100)
app$setInputs(heatm = "click")
Sys.sleep(2)
app$snapshot()
app$setInputs(clusters = 4)
app$setInputs(meangrp = TRUE)
app$setInputs(colname = "hide")
app$setInputs(colname = "show")
app$setInputs(rowname = "show")
app$setInputs(heatm = "click")
app$snapshot()
app$setInputs(indiv = c("LKO_CTRL", "LKO_MCD ", "LWT_CTRL"))
app$setInputs(heatm = "click")
app$snapshot()
