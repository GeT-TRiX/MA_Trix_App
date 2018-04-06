wd_path= getwd()
firstdim = 1
secdim = 2
palette = c( "#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442",
             "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray", 
             "burlywood1","darkkhaki", "#CC0000" )
firstcol = "green"
intercol = "black"
lastcol = "red"
suffix = "test"
prefix = "toast"
list.of.packages <- c("shiny","dplyr","shinythemes","shinyjs","ggplot2","shinyBS","markdown"
                      ,"BH","rCharts","data.table","DT","readr","rbenchmark","colourpicker")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 
##"foreach","doParallel"