### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


observe({
  req(Venncluster())
  updateSliderInput(session, "clusterNumber", max = nrow(summary(Venncluster())))
})

output$saveclusterchoose <- downloadHandler(filename <- function() {
  paste0(basename(file_path_sans_ext(projectname())), '_venn', input$clusterNumber, 'cluster.', input$formvennclus, sep =
           '')
},
content <- function(file) {
  

  if (input$formvennclus == "pdf")

    pdf(file,
        width = 12,
        height = 12,
        pointsize = 12)


  else if (input$formvennclus == "png")

    png(
      file,
      width = 1200,
      height = 1200,
      units = "px",
      pointsize = 12,
      res = 100
    )
  else
    eps(file,
        width = 12,
        height = 12,
        pointsize = 12)

  #if(typeof(acyclgo()) !="S4") return(NULL)  

    acyclgo() 
    dev.off()
  
})




output$clusterPlot <- renderPlot({
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(input$GOvenn ,'You need to click on the run Analysis button!'))
  req(Venncluster())

  plot2D(Venncluster(), input$clusterNumber)
})

#output$acyclicgo <- renderPlot({

#davidGODag<-DAVIDGODag(members(Venncluster())[[input$clusterNumber]],  pvalueCutoff=0.1)
#plotGOTermGraph(g=goDag(davidGODag),r=davidGODag, max.nchar=50, node.shape="ellipse")

#})

davidtag<- reactive({req(Venncluster())
  davidGODag<-DAVIDGODag(members(Venncluster())[[input$clusterNumber]],  pvalueCutoff=0.05) })


acyclgo <- reactive({
  req(davidtag())
  result = tryCatch({
    plotGOTermGraph(g=goDag(davidtag()),r=davidtag(), max.nchar=40, node.shape="ellipse")
  }, warning = function(warning_condition) {
    cat("web url is wrong, can't get\n")
    return(F)
  })
  
  return(result)
})

observe({
  req(acyclgo())
  if(typeof(acyclgo()) !="S4") shinyjs::disable("saveclusterchoose") else shinyjs::enable("saveclusterchoose")
})

output$debug <- renderPrint({

  req(Venncluster())
  summary(Venncluster())
})


#' Venncluster is an event reactive function which aim is to interogate David web services database to collect relevant information about the list of genes for a specific intersection
#'
#' @param GOvenn clickable event button
#' @param vennfinal a list of two data frames
#' @param Species list of annotated elements
#' @param Speciesvenn character input
#'
#' @return david enrichment object
#' @export
#'

Venncluster <- eventReactive(input$GOvenn, {

    req(vennfinal())

    withProgress(message = 'Performing GO enrichment:',
                 value = 0, {
                   n <- NROW(50)
                   for (i in 1:n) {
                     incProgress(1 / n, detail = "Please wait...")
                   }
                   library(RDAVIDWebService)

                   timeoutdav <- function(y)
                     if (any(grepl("Read timed out", y)))
                       invokeRestart("muffleWarning")

                   tryCatch({
                     mygodavid = probnamtoentrezvenn(vennfinal()[[1]]$GeneName , Species()[[1]]) %>%
                     davidqueryvenn(input$Speciesvenn) %>% withCallingHandlers(error = timeoutdav)
                   }, warning = function(e) {

                     shinyjs::alert("David's server is busy")
                     warning("David's server is busy")
                     return(NULL)

                   })
                 })


    return(mygodavid)
  })
