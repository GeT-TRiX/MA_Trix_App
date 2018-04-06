wd_path= getwd()
firstdim = 1
secdim = 2
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