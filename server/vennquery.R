observe({
  req(Venncluster())
  updateSliderInput(session, "clusterNumber", max = nrow(summary(Venncluster())))
})

output$clusterPlot <- renderPlot({
  req(Venncluster())
  if(input$clusterNumber == 1)
    shinyjs::alert("There's not enough genes in your interaction(s)")
  plot2D(Venncluster(), input$clusterNumber)
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
                     mygodavid = probnamtoentrezvenn(vennfinal()$GeneName , Species()[[1]]) %>%
                     davidqueryvenn(input$Speciesvenn) %>% withCallingHandlers(error = timeoutdav)
                   }, warning = function(e) {
                     
                     shinyjs::alert("David's server is busy")
                     warning("David's server is busy")
                     return(cbind("David's server is busy") %>% as.data.frame() %>% setNames("Error"))
                     
                   })
                 })
    
    
    return(mygodavid)
  })
