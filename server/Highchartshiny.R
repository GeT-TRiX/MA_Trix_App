### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0

#########################################
######## From Shiny to highcharts       #
#########################################




addpercentpop <- reactive({

  req(myresdavitab())
  reumdiff = lapply(1:length(myresdavitab()),function(x)return(sapply(length(myresdavitab()[[x]]$Count), function(y){
    return(as.numeric(as.character(myresdavitab()[[x]]$Count))/as.numeric(as.character(myresdavitab()[[x]]$Pop.Hits))*100)})) %>%
      mutate(myresdavitab()[[x]],percent = .)) %>% bind_rows()
})

dfenrichtojson <- reactive({

  req(addpercentpop())
  filtered <- addpercentpop()
  return(DftoHighjson(filtered, input$enrichbased))

})


observe({
  req(dfenrichtojson(), input$enrichbased )

  axisParameters <- list(
    topcatdav = list( min = 0, max = 12, legend = 'Source: <a href="https://www.highcharts.com/"  target="_blank">Plot produce with highcharts</a> and <a href="https://shiny.rstudio.com/" target= "_blank">Shiny</a>',
                      title= "top genes", xaxis = ifelse(input$enrichbased == "FoldE", "Fold Enrichment", "pvalue"))
  )

  newData <- c(axisParameters$topcatdav, list(series=dfenrichtojson()))
  islab = input$addlabelhigh
  session$sendCustomMessage(type="updateVariable", newData) # send to javascript data
  session$sendCustomMessage("handler1", islab) #send to javascript labels
})
