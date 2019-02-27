### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


observe({
  req(Venncluster())
  updateSliderInput(session, "clusterNumber", max = nrow(summary(Venncluster())))
})



callModule(downoutputfiles, "saveclustvenn", projectname = projectname , suffix=paste0( '_venn', input$clusterNumber , "cluster.", sep='' ), data = acyclgo , w =12, h = 12 , clustvenn = T)


output$clusterPlot <- renderPlot({
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
    need(choix_cont(), 'Set your thresholds and then select your comparison to display the Venn diagram!')%next%
    need(input$selcontjv ,'You need to click on a number (Venn diagram) to display the data table!')) 
    req(Venncluster())
    
    plot2D(Venncluster(), input$clusterNumber)
})


davidtag<- reactive({
  req(Venncluster())
  davidGODag<-DAVIDGODag(members(Venncluster())[[input$clusterNumber]],  pvalueCutoff=0.1, input$catvenn ) }) 



acyclgo <- function() {
  req(davidtag())
  result = plotGOTermGraph(g=goDag(davidtag()),r=davidtag(), max.nchar=40, node.shape="ellipse")
  return(result)
}


Venncluster <- callModule(queryDavid, "vennanalysis", data = vennfinal , parent_session = session )
  