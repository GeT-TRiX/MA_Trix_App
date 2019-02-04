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
    
    
    acyclgo()
    dev.off()
  
})


output$clusterPlot <- renderPlot({
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
    need(choix_cont(), 'Set your thresholds and then select your comparison to display the Venn diagram!')%next%
    need(input$selcontjv ,'You need to click on a number (Venn diagram) to display the data table!') %next%
    need(input$GOvenn ,'You need to click on the run Analysis button!')) 
    req(Venncluster())
    plot2D(Venncluster(), input$clusterNumber)
})



davidtag<- reactive({req(Venncluster())
  davidGODag<-DAVIDGODag(members(Venncluster())[[input$clusterNumber]],  pvalueCutoff=0.1, input$catvenn ) })



acyclgo <- function() {
  req(davidtag())
  result = plotGOTermGraph(g=goDag(davidtag()),r=davidtag(), max.nchar=40, node.shape="ellipse")
  return(result)
}


observe({
  req(acyclgo())
  pdf(NULL)
  if(class(acyclgo()) == "graphNEL")
    shinyjs::disable("saveclusterchoose")
  else
    shinyjs::enable("saveclusterchoose")
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
                     mygodavid = probnamtoentrezvenn(vennfinal()[[1]]$GeneName , Speciesvenn()[[1]]) %>%
                     davidqueryvenn(input$Speciesvenn) %>% withCallingHandlers(error = timeoutdav)
                   }, warning = function(e) {

                     shinyjs::alert("David's server is busy")
                     warning("David's server is busy")
                     return(NULL)

                   })
                 })
    
    pdf(NULL)
    
    updateTabsetPanel(session, "Vennd",
                      selected = "venngopanel")
    
    return(mygodavid)
  })


#' Species is a reactive function which aim is to return annotated packages for a specific genome
#'
#' @param Species character input
#' @param Speciesvenn character input
#'
#' @return
#' @export
#'


Speciesvenn <- reactive({
  if ( input$Speciesvenn == "Homo sapiens") {
    # human
    library("org.Hs.eg.db")
    return(list(org.Hs.egALIAS2EG, org.Hs.egSYMBOL))
  }
  else if ( input$Speciesvenn == "Mus musculus" ) {
    # Mouse
    library("org.Mm.eg.db")
    return( list(org.Mm.egALIAS2EG, org.Mm.egSYMBOL))
  }
  else if (input$Speciesvenn == "Danio rerio") {
    #Zebra fish
    library("org.Dr.eg.db")
    return(list(org.Dr.egALIAS2EG, org.Dr.egSYMBOL))
  }
  else if (input$Speciesvenn == "Gallus gallus") {
    # chicken
    library("org.Gg.eg.db")
    return(list(org.Gg.egALIAS2EG, org.Gg.egSYMBOL))
  }
  else if ( input$Speciesvenn == "equCab2") {
    # horse
    library("org.Gg.eg.db")
    return(list(org.Gg.eg.dbALIAS2EG))
  }
  else if (input$Speciesvenn == "Caenorhabditis elegans") {
    # cC elegans
    library("org.Ce.eg.db")
    
    return(list(org.Ce.egALIAS2EG, org.Ce.egSYMBOL))
  }
  else if ( input$Speciesvenn == "Rattus norvegicus") {
    # Rat
    library("org.Rn.eg.db")
    return(list(org.Rn.egALIAS2EG, org.Rn.egSYMBOL ))
  }
  else if (input$Speciesvenn == "Sus scrofa") {
    # Pig
    library("org.Ss.eg.db")
    return(list(org.Ss.egALIAS2EG, org.Ss.egSYMBOL))
  }
  
})
