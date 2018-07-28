#########################################
######## From Shiny to highcharts       #
#########################################

axisParameters <- list(
  topcatdav = list( min = 0, max = 12, legend = 'Source: <a href="https://www.highcharts.com/"  target="_blank">Plot produce with highcharts</a> and <a href="https://shiny.rstudio.com/" target= "_blank">Shiny</a>', title= "top genes")
)

filteredata<- reactive({
  req(myresdavitab())
  reumdiff = lapply(1:length(myresdavitab()),function(x)return(sapply(length(myresdavitab()[[x]]$Count), function(y){
    return(as.numeric(as.character(myresdavitab()[[x]]$Count))/as.numeric(as.character(myresdavitab()[[x]]$List.Total))*100)})) %>%  
      mutate(myresdavitab()[[x]],percent = .)) %>% rbind.fill() 
  return(reumdiff)
})

plotDataenrichment <- reactive({
  
  param <- list(search= "Fold.Enrichment", n_points=length(filteredata()$Fold.Enrichment), x_start=min(as.numeric(filteredata()$Fold.Enrichment)))
  filtered <- filteredata()
  return(DftoHighjson(filtered,param))
  
})

observe({
  req(plotDataenrichment())
  newData <- c(axisParameters$topcatdav, list(series=plotDataenrichment()))
  islab = input$addlabelhigh 
  session$sendCustomMessage(type="updateVariable", newData)
  session$sendCustomMessage("handler1", islab)
})
