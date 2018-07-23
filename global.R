# Allow to upload 50M files only shaman server
#if(Sys.info()["nodename"] == "ShinyPro"){
#  options(shiny.maxRequestSize=1000000000*1024^2)
#}else{
# Limit with the raw data submission to 2Gb
options(shiny.maxRequestSize=2000000000)


cutheatmlist = list( Boxplot = c( `True` = 'Boxplot'), Heatmap=c(`True` = "Heatmap"), 
                     Stripchart=c(`Without boxplot`="LB", `With boxplot` = "WB"))


categoerygen = c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY")


source("css/owncss.R")
source('LoadPackages.R')
source("function/formating.R")
source("function/PCA.R")
source("function/heatmtruncated.R")
source("function/formating.R")
source("function/PCA.R")
source("function/decideTestTrix.R")
source("function/vennplot.R")
source("function/create_forked_task.R")
source("function/cutheat.R")
source("function/gosearch.R")
source("function/highchartconverter.R")



################################
######## Chat env             ##
################################

# Globally define a place where all users can share some reactive data.
vars <- reactiveValues(chat=NULL, users=NULL)

# Restore the chat log from the last session.
if (file.exists("chat.Rds")){
  vars$chat <- readRDS("chat.Rds")
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
#mysess <- sessionInfo()

`%next%` <- shiny:::`%OR%`


palette = brewer.pal(8,"Dark2") %>%
  list(brewer.pal(10,"Paired")) %>%
  unlist()



textInputRow<-function (inputId, label, value = "") 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}