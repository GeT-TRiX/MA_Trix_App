### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



#########################################
######## From Shiny to highcharts       #
#########################################

axisParameters <- list(
  topcatdav = list( min = 0, max = 12, legend = 'Source: <a href="https://www.highcharts.com/"  target="_blank">Plot produce with highcharts</a> and <a href="https://shiny.rstudio.com/" target= "_blank">Shiny</a>', title= "top genes")
)

filteredata<- reactive({
  
  #d <- NULL
  req(myresdavitab())
  # paraltest <- myresdavitab()
  # cl <- makeCluster(getOption("cl.cores", 4))
  # clusterExport(cl,c("paraltest"),envir=environment())
  # clusterEvalQ(cl, library(dplyr))
   reumdiff = lapply(1:length(myresdavitab()),function(x)return(sapply(length(myresdavitab()[[x]]$Count), function(y){
     return(as.numeric(as.character(myresdavitab()[[x]]$Count))/as.numeric(as.character(myresdavitab()[[x]]$List.Total))*100)})) %>%  
       mutate(myresdavitab()[[x]],percent = .)) %>% rbind.fill() 
  # d = parLapply(cl, 1:length(paraltest),function(x)return(sapply(length(paraltest[[x]]$Count), function(y){
  #   return(as.numeric(as.character(paraltest$Count))/as.numeric(as.character(paraltest[[x]]$List.Total))*100)})) %>%  
  #     mutate(paraltest[[x]],percent = .)) %>% rbind.fill() 
  # stopCluster(cl)
  #})
  #d
  return(reumdiff)
})

plotDataenrichment <- reactive({
  req(filteredata())
  param <- list(search= "Fold.Enrichment", n_points=length(filteredata()$Fold.Enrichment), x_start=min(as.numeric(filteredata()$Fold.Enrichment)))
  filtered <- filteredata()
  return(DftoHighjson(filtered,param))
  
})

observe({
  req(filteredata())
  print(filteredata())
  
})

observe({
  req(plotDataenrichment())
  newData <- c(axisParameters$topcatdav, list(series=plotDataenrichment()))
  islab = input$addlabelhigh 
  session$sendCustomMessage(type="updateVariable", newData)
  session$sendCustomMessage("handler1", islab)
})
