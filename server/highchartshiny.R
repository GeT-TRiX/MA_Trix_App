observe({
  session$sendCustomMessage(type = 'testmessage',
                            message = list(a = 1, b = 'hello',
                                           controller = input$controller))
})


axisParameters <- list(
  topcatdav = list( min = 0, max = 12, legend = 'Source: <a href="https://www.highcharts.com/"  target="_blank">Plot produce with highcharts</a> and <a href="https://shiny.rstudio.com/" target= "_blank">Shiny</a>', title= "top genes")
)

filteredata<- reactive({
  req(myresdavitab())
  myresdavitab() %>% rbind.fill()
})

plotDataenrichment <- reactive({
  
  param <- list(search= "Fold.Enrichment", n_points=length(filteredata()$Fold.Enrichment), x_start=min(as.numeric(filteredata()$Fold.Enrichment)))
  filtered <- filteredata()
  return(DftoHighjson(filtered,param))
  
})

observe({
  req(plotDataenrichment())
  
  newData <- c(axisParameters$topcatdav, list(series=plotDataenrichment()))
  session$sendCustomMessage(type="updateVariable", newData)
})