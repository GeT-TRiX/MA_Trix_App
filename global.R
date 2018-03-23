wd_path= getwd()
prefix = "Heatmap"
suffix = "number1"
list.of.packages <- c("shiny","dplyr","shinythemes","shinyjs","ggplot2","shinyBS","markdown","BH","rCharts","data.table","DT")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 


# readr