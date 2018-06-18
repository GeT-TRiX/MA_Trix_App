
#sudo apt-get install libv8-dev

list.of.packages <- c("shiny","shinythemes","shinyjs","AnnotationDbi","ggplot2","shinyBS","markdown"
                      ,"BH","data.table","DT","readr","colourpicker","shinydashboard","shinytoastr",
                      "tools","devEMF","R.devices","FactoMineR","factoextra","gplots","V8",
                      "RColorBrewer","foreach","doParallel","VennDiagram","gridExtra","plotly","dplyr","reticulate","Hmisc")

#"goseq","GO.db","rbenchmark","heatmaply"




new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 



