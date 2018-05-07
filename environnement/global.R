require(dplyr)
require(RColorBrewer)

wd_path= getwd()
firstdim = 1
secdim = 2

# palette = c("#0072C2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442",
#              "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray",
#              "burlywood1","darkkhaki", "#CC0000" )

palette = brewer.pal(8,"Dark2") %>%
  list(brewer.pal(10,"Paired")) %>%
  unlist()

firstcol = "green"
intercol = "black"
lastcol = "red"
suffix = "test"
prefix = "toast"

cutheatmlist = list( Boxplot = c( `True` = 'Boxplot'), Heatmap=c(`True` = "Heatmap"), 
                     Stripchart=c(`Without boxplot`="LB", `With boxplot` = "WB"))


list.of.packages <- c("shiny","dplyr","shinythemes","shinyjs","ggplot2","shinyBS","markdown"
                      ,"BH","rCharts","data.table","DT","readr","rbenchmark","colourpicker",
                      "tools","devEMF","R.devices","FactoMineR","factoextra","heatmaply",
                      "RColorBrewer","foreach","doParallel","VennDiagram","gridExtra","plotly")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
##"foreach","doParallel"