#########################################
######## Source files                   #
#########################################

source("function/formating.R")
source("environnement/global.R")
source("function/PCA.R")
source("www/loadingcss.R")
source("function/heatmtruncated.R")
source("function/formating.R")
source("function/PCA.R")
source("function/decideTestTrix.R")
source("function/vennplot.R")
source("function/create_forked_task.R")
source("function/cutheat.R")
source("function/gosearch.R")



#########################################
######## Global Environment packages    #
#########################################



list.of.packages <- c("shiny","shinythemes","shinyjs","AnnotationDbi","ggplot2","shinyBS","markdown"
                      ,"BH","data.table","DT","readr","colourpicker",
                      "tools","devEMF","R.devices","FactoMineR","factoextra","gplots",
                      "RColorBrewer","foreach","doParallel","VennDiagram","gridExtra","plotly","dplyr","reticulate","Hmisc")

#"goseq","GO.db","rbenchmark","heatmaply"

#Warning: Error in py_run_file_impl: ImportError: No module named requests
# 
# Detailed traceback: 
#   File "<string>", line 3, in <module>
#   
#   122: <Anonymous>


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){library(x,character.only=TRUE)}) 


#########################################
######## Global Environment variables   #
#########################################

firstcol = "green"
intercol = "black"
lastcol = "red"
wd_path= getwd()
firstdim = 1
secdim = 2

`%next%` <- shiny:::`%OR%`


cutheatmlist = list( Boxplot = c( `True` = 'Boxplot'), Heatmap=c(`True` = "Heatmap"), 
                     Stripchart=c(`Without boxplot`="LB", `With boxplot` = "WB"))

# palette = c("#0072C2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442",
#              "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray",
#              "burlywood1","darkkhaki", "#CC0000" )

palette = brewer.pal(8,"Dark2") %>%
  list(brewer.pal(10,"Paired")) %>%
  unlist()



textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}