
DftoHighjson <- function(data, param) {
  
  foldEnrichment <- param$search 
  tempData <- (data[grep(param$search,colnames(data))])
  colnames(tempData) <- paste0("y",1)
  tempData$id <- data$Category 
  tempData$Term <- data$Term
  tempData$pvalue <- data$Benjamini
  tempData$FE <- data$Fold.Enrichment 
  tempData$Topgenes <-  data$Top

  unifiedData <- reshape(tempData, varying=paste0("y",1), 
                      direction="long", idvar="Top",sep="",timevar="x")
  
  
  unifiedData <- unifiedData[order(unifiedData$id),]

  
  unifiedData$x <- as.numeric(as.character(unifiedData$FE))
  unifiedData$y <- as.numeric(unifiedData$Topgenes)
  unifiedData$z <-  as.numeric(as.character(unifiedData$FE))
  unifiedData$GO  = sapply(unifiedData$Term, FUN= function(x) if(grepl("^mmu", x)) return(strsplit(as.character(x), ":")%>% unlist() %>% .[1]) 
                        else return(strsplit(as.character(x), "~")%>% unlist() %>% .[1])) 
  
  unifiedData$Term  = sapply(unifiedData$Term, FUN= function(x) if(grepl("^mmu", x)) return(strsplit(as.character(x), ":")%>% unlist() %>% .[2]) 
                        else return(strsplit(as.character(x), "~")%>% unlist() %>% .[2])) 
  unifiedData$Pvalue =  tempData$pvalue
  

  #DftoHighjson <- highchartsConvert(unifiedData)
  
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
