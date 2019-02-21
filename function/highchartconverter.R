### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


DftoHighjson <- function(data, param, method) {

  foldEnrichment <- param$search
  tempData <- (data[grep(param$search,colnames(data))])
  colnames(tempData) <- paste0("y",1)
  tempData$id <- data$Category
  tempData$Term <- data$Term
  tempData$pvalue <- data$PValue
  #tempData$FE <- data$Fold.Enrichment # PValue
  #tempData$FE <- data$PValue # PValue
  tempData$FE <- ifelse(method == "FoldE",  data$Fold.Enrichment , data$PValue )
  tempData$Topgenes <-  data$Top
  tempData$percent <- data$percent


  unifiedData <- reshape(tempData, varying=paste0("y",1),
                      direction="long", idvar="Top",sep="",timevar="x")


  unifiedData <- unifiedData[order(unifiedData$id),]


  unifiedData$x <- as.numeric(as.character(unifiedData$FE)) # as.double pour P.value
  #unifiedData$x <- as.double(as.character(unifiedData$FE)) # as.double pour P.value
  unifiedData$y <- as.numeric(unifiedData$Topgenes)
  unifiedData$z <-  as.numeric(as.character(unifiedData$percent))


  unifiedData$GO  = sapply(unifiedData$Term, FUN= function(x) if(grepl("^mmu|^rno|^hsa|^ssc|^ptr|^gga|^cel|^ecb", x)) return(strsplit(as.character(x), ":")%>% unlist() %>% .[1])
                        else return(strsplit(as.character(x), "~")%>% unlist() %>% .[1]))

  unifiedData$Term  = sapply(unifiedData$Term, FUN= function(x) if(grepl("^mmu|^rno|^hsa|^ssc|^ptr|^gga|^cel|^ecb", x)) return(strsplit(as.character(x), ":")%>% unlist() %>% .[2])
                        else return(strsplit(as.character(x), "~")%>% unlist() %>% .[2]))

  unifiedData$Pvalue =  format(tempData$pvalue, digits = 3)



  return(highchartsConvert(unifiedData))
}


formatPoint <- function(index) {
  return( list( x=index$x, y=index$y,z=index$z, GO=index$GO, term=index$Term, pvalue = index$Pvalue))
}


formatCategory <- function(dframeCategory) {

  categoryTemplate <- list(name="",data={})
  categoryTemplate$name <- dframeCategory$id[[1]]
  categoryTemplate$visible <- F
  dataPoints <- dlply(dframeCategory,.(1:nrow(dframeCategory)),formatPoint)
  names(dataPoints) <- NULL
  categoryTemplate$data <- dataPoints

  return(categoryTemplate)
}



highchartsConvert <- function(DftoHighjson) {

  json <- dlply (DftoHighjson, .(id), formatCategory)
  names(json) <- NULL
  return(json)

}
