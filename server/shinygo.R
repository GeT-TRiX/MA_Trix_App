#obsC <- observe(quote({ print(hmobj$hm) }), quoted = TRUE)

#gores <- reactiveValues()

url <- reactiveValues()
gores <- reactiveValues()

observe({
  req(url)
  
  output$DAVID <- renderUI({
    shiny::actionButton(
      inputId = 'DAVID',
      "Open DAVID",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
      onclick = paste("window.open(", url$myurl)
    )
  })
})

# observe({
#   req(clustergrep())
#   print(length(clustergrep()))
#
#   if (length(clustergrep()) > 400)
#     shinyjs::disable("DAVID")
#
# })


observe({
  totalclust <- reactive({
    req(hmobj$hm)
    
    n <- unique(hmobj$hm$cluster)
    selectInput("cutgo",
                "Choose your cluster",
                choices =  seq(1, NROW(n) , by = 1))
    
  })
  
  
  output$cutgo <- renderUI({
    totalclust()
  })
  
})




# observe({ TODOOOO
#   req(input$mainhmtabset)
#   if (grepl("hmmainpan",  input$mainhmtabset)) {
#     updateTabsetPanel(session, "tabset25",
#                       selected = "hmpan")
#   }
#   else if (grepl("cuthmmainpan",  input$mainhmtabset)) {
#     #|dfhmclu|maingo
#     updateTabsetPanel(session, "tabset25",
#                       selected = "cutpan")
#   }
# })


clustergrep <- reactive({
  req(hmobj$hm, input$cutgo)
  
  genlist <- hmobj$hm[!duplicated(hmobj$hm$GeneName),] %>%
    dplyr::select(cluster, GeneName)   %>%
    filter(cluster == input$cutgo)
  
  mygensymb = genlist$cluster %>%
    length() %>%
    matrix(1, .) %>%
    as.double() %>%
    setNames(genlist$GeneName) %>%
    names() %>% as.list() %>%
    .[lapply(., function(x)
      length(grep("chr", x, value = FALSE))) == 0]
  
  return(mygensymb)
})

davidwebservice <-
  eventReactive(input$GO, {
    #Warning: Error in .jcall: org.apache.axis2.AxisFault: Read timed out
    
    req(hmobj$hm)
    
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
                     mygodavid = probnamtoentrez(hmobj$hm, Species()) %>%
                       davidquery(input$Species) %>% withCallingHandlers(error = timeoutdav)
                   }, warning = function(e) {
                     warning("David's server is busy")
                     
                     return(cbind("David's server is busy") %>% as.data.frame() %>% setNames("Error"))
                     
                   })
                 })
    
    final = lapply(1:NROW(mygodavid), function(x)
      return(format(mygodavid[[x]], digits = 3)))
    
    return(final)
  })


observe({
  req(davidwebservice())
  print(colnames(davidwebservice()))
  
  
})


davidurl <- reactive({
  req(clustergrep())
  
  source_python('./python/add.py')
  mydavurl = enrichmentdav(clustergrep())
  mygloburl <- paste(`mydavurl`, ",", "'_blank')")
  
  return(mygloburl)
})


observe({
  req(davidurl())
  url$myurl = davidurl()
})


output$clustgo <- renderPrint({
  validate(
    need(csvf(), 'You need to import data to visualize the data!') %next%
      need(
        input$cutgo,
        'You need to click on the heatmap button! then on the run GO button'
      )
  )
  gores$obj <- isolate(testad())
  
  req(input$cutgo, input$slidergo)
  x <- input$cutgo
  if (!is.null(testad()[[as.integer(x)]])) {
    for (go in input$slidergo[[1]]:input$slidergo[[2]]) {
      if (Ontology(testad()[[as.integer(x)]][[1]][[go]]) == input$onto) {
        cat(paste("GOID:", (GOID(
          gores$obj[[as.integer(x)]][[1]][[go]]
        ))))
        cat("\n")
        cat(paste("Term:", (Term(
          gores$obj[[as.integer(x)]][[1]][[go]]
        ))))
        cat("\n")
        cat(paste("Ontology:", (Ontology(
          gores$obj[[as.integer(x)]][[1]][[go]]
        ))))
        cat("\n")
        cat(paste("Definition:", (Definition(
          gores$obj[[as.integer(x)]][[1]][[go]]
        ))))
        cat("\n")
        cat(paste("Synonym:", (Synonym(
          gores$obj[[as.integer(x)]][[1]][[go]]
        ))))
        cat("\n")
        
        cat("--------------------------------------\n")
      }
    }
  }
  else
    print("Sorry, no enriched genes for this cluster")
  
})


output$savego = downloadHandler(
  'go.xlsx',
  content = function(file) {
    library(xlsx)
    
    for (i in 1:length(davidwebservice())) {
      if (i == 1)
        write.xlsx(file = file,
                   davidwebservice()[[i]],
                   sheetName = paste("Cluster", i))
      else
        write.xlsx(
          file = file,
          davidwebservice()[[i]],
          sheetName = paste("Cluster", i),
          append = TRUE
        )
    }
    
    
    
  }
)
#gores$obj <- NULL

#Species <- eventReactive(input$DAVID,{
Species <- reactive({
  if (input$Species == "Homo sapiens") {
    # human
    library("org.Hs.eg.db")
    mypack = org.Hs.egALIAS2EG
    return(mypack)
  }
  else if (input$Species == "Mus musculus") {
    # Mouse
    library("org.Mm.eg.db")
    mypack = org.Mm.egALIAS2EG
    return(mypack)
  }
  else if (input$Species == "Danio rerio") {
    #Zebra fish
    library("org.Dr.eg.db")
    mypack = org.Dr.egALIAS2EG
    return(mypack)
  }
  else if (input$Species == "Gallus gallus") {
    # chicken
    library("org.Gg.eg.db")
    mypack = org.Gg.egALIAS2EG
    return(mypack)
  }
  else if (input$Species == "equCab2") {
    # horse
    library("org.Gg.eg.db")
    mypack = org.Mm.egALIAS2EG
    return(mypack)
  }
  else if (input$Species == "Caenorhabditis elegans") {
    # cC elegans
    library("org.Ce.eg.db")
    mypack = org.Ce.egALIAS2EG
    return(mypack)
  }
  else if (input$Species == "Rattus norvegicus") {
    # Rat
    library("org.Rn.eg.db")
    mypack = org.Rn.egALIAS2EG
    return(mypack)
  }
  else if (input$Species == "Sus scrofa") {
    # Pig
    library("org.Ss.eg.db")
    mypack = org.Ss.egALIAS2EG
    return(mypack)
  }
  
})
