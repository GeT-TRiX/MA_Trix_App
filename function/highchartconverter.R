### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#' DftoHighjson is a function which aims is to reshape a DAVID restable dataframe to a json format
#'
#' @param data A Functional Annotation Summary dataframe (DAVID output)
#' @param method A character to specify if users want to display top term based on the pvalue or the fold Enrichment
#'
#' @return Json object to be pass to highchart library
#'
#' @export
#'


DftoHighjson <- function(data, method) {

  tempData <- (data[grep("Fold.Enrichment",colnames(data))])
  colnames(tempData) <- paste0("y",1)
  tempData$id <- data$Category
  tempData$Term <- data$Term
  tempData$pvalue <- data$PValue
  tempData$FE <- data$Fold.Enrichment



  if(method == "FoldE")
    tempData$xval <- data$Fold.Enrichment
  else
    tempData$xval <- data$PValue


  tempData$Topgenes <-  data$Top
  tempData$percent <- data$percent


  unifiedData <- reshape(tempData, varying=paste0("y",1),
                      direction="long", idvar="Top",sep="",timevar="x")


  unifiedData <- unifiedData[order(unifiedData$id),]
  unifiedData$x <- as.double(as.character(unifiedData$xval))
  unifiedData$y <- as.numeric(unifiedData$Topgenes)
  unifiedData$z <-  as.numeric(as.character(unifiedData$percent))


  unifiedData$GO  = sapply(unifiedData$Term, FUN= function(x) if(grepl("^mmu|^rno|^hsa|^ssc|^ptr|^gga|^cel|^ecb", x)) return(strsplit(as.character(x), ":")%>% unlist() %>% .[1])
                        else return(strsplit(as.character(x), "~")%>% unlist() %>% .[1]))

  unifiedData$Term  = sapply(unifiedData$Term, FUN= function(x) if(grepl("^mmu|^rno|^hsa|^ssc|^ptr|^gga|^cel|^ecb", x)) return(strsplit(as.character(x), ":")%>% unlist() %>% .[2])
                        else return(strsplit(as.character(x), "~")%>% unlist() %>% .[2]))

  unifiedData$Pvalue =  format(tempData$pvalue, digits = 3)

  unifiedData$FE = as.integer(as.character(unifiedData$FE))

  return(highchartsConvert(unifiedData))
}


#' formatPoint is a function which aims is to reutnr a list of information for each category (top 10)
#'
#' @param index A dataframe to be processed
#'
#' @return A list of usefull information from the david restable
#'
#' @export
#'


formatPoint <- function(index) {

  return( list( x=index$x, y=index$y,z=index$z, GO=index$GO, term=index$Term, pvalue = index$Pvalue, FE = index$FE))
}


#' formatCategory is a function which aims is to return a json structure
#'
#' @param dframeCategory A dataframe to be processed
#'
#' @return Json structure
#'
#' @export
#'

formatCategory <- function(dframeCategory) {

  categoryTemplate <- list(name="",data={})
  categoryTemplate$name <- dframeCategory$id[[1]]
  categoryTemplate$visible <- F
  dataPoints <- dlply(dframeCategory,.(1:nrow(dframeCategory)),formatPoint)
  names(dataPoints) <- NULL
  categoryTemplate$data <- dataPoints

  return(categoryTemplate)
}



#' highchartsConvert is a function which aims is to convert a dataframe object to the json format
#' by spliting the dataframe by categories to be passed to the highchart library
#'
#' @param DftoHighjson A dataframe to be processed
#'
#' @return A json object example:[{"GOTERM_BP_ALL" : [{ "x" : 0.005 , "y" : 2 , "Term" : "inactivation of MAPK activity" , "Go" : "GO:0000188"}], ..., "GOTERM_CC_ALL : [{ ...}}]]
#'
#' @export
#'


highchartsConvert <- function(DftoHighjson) {

  json <- dlply (DftoHighjson, .(id), formatCategory)
  names(json) <- NULL
  return(json)

}
