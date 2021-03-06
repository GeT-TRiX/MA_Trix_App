### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0

# increase loading files

options(shiny.maxRequestSize=128000000)
options(digits=3)

userId <- Sys.getenv("SHINYPROXY_USERNAME")
root <- ifelse(userId != "", paste("/root/MA_Trix_App/data/", userId, sep = ""), "/home/fsoubes/dockerize_MATRiX/MA_Trix_App/data/gettrix")
print(root)

cutheatmlist = list( Boxplot = c( `True` = 'Boxplot'), Heatmap=c(`True` = "Heatmap"),
                     Stripchart=c(`Without boxplot`="LB", `With boxplot` = "WB"))


categoerygen = c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY")

# Loading packages

#sudo apt-get install libv8-dev for V8 package

list.of.packages <- c("AnnotationDbi","shiny","shinythemes","shinyjs","ggplot2","shinyBS","plyr","shinyFiles","xlsx","stringr",
                      "BH","data.table","DT","readr","colourpicker","shinydashboard","heatmaply",
                      "tools","R.devices","FactoMineR","factoextra","gplots","V8","RColorBrewer","foreach","doParallel",
                      "gridExtra","plotly","dplyr","reticulate","Hmisc", "devEMF")



new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){
suppressPackageStartupMessages(library(x,character.only=TRUE))})

source("css/csstips.R")
source("function/PCA.R")
source("function/heatmtruncated.R")
source("function/formatingtables.R")
source("function/decideTestTrix.R")
source("function/ggstrip_groups.r")
source("function/vennplot.R")
source("function/create_forked_task.R")
source("function/cutheat.R")
source("function/gosearch.R")
source("function/highchartconverter.R")
source("function/EnhancedVolcano.R")
source("./module/csvmodules.R")
source("./module/savemodules.R")
source("./module/Selgroup.R")
source("./module/enrichmodule.R")
source("./module/groupcol.R")
source("./module/magnificentables.R")
source("./module/Savemoduletable.R")
source("./module/getdegenes.R")




################################
######## Chat env             ##
################################

# Globally define a place where all users can share some reactive data.
vars <- reactiveValues(chat=NULL, users=NULL)

# Restore the chat log from the last session.

if (file.exists("chat.Rds") && userId == ""){
  vars$chat <- readRDS("chat.Rds")
} else if(file.exists("./chat/chat.Rds") && userId != ""){
    vars$chat <- readRDS("./chat/chat.Rds")
} else {
  vars$chat <- "Welcome to MATRiX Chat!"
}



#' Get the prefix for the line to be added to the chat window. Usually a newline
#' character unless it's the first line.
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

################################
######## Variables env        ##
################################


firstcol = "green"
intercol = "black"
lastcol = "red"
wd_path= getwd()
firstdim = 1
secdim = 2


`%next%` <- shiny:::`%OR%`


palette = brewer.pal(8,"Dark2") %>%
  list(brewer.pal(10,"Paired")) %>%
  unlist()



textInputRow<-function (inputId, label, value = "") {
  div(style="display:inline-block",
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}
