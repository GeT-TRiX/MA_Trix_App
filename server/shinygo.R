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
                     mygodavid = probnamtoentrez(hmobj$hm, Species()[[1]]) %>%
                       davidquery(input$Species, input$catinfo) %>% withCallingHandlers(error = timeoutdav)
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



mytransf <- reactive({
  req( davidwebservice())
  
    
  myselectedrows = (davidwebservice()[[as.numeric(input$cutgo)]][input$davidgo_rows_selected, c("Genes", "Term"),  drop = FALSE]) 

  if(length(myselectedrows["Genes"][[1]])>0){
    
    myentreztosymb = lapply(1:NROW(myselectedrows),function(x){
      myselectedrows$Genes[[x]] %>% strsplit( ", ") %>% unlist() %>% mget(x= .,envir = Species()[[2]],ifnotfound = NA) %>%  unlist() %>%
        unique() %>% cbind(myselectedrows$Term[[x]]) %>% as.data.frame() %>% setNames(., c("Genes", "Term"))
    
    })
    
    return(myentreztosymb)
  }
  else{

    return(NULL)
  }
  
})

output$printmessage <- renderPrint({
  req(davidwebservice())
  cat("You can select the rows in the table above in order to display the gene names")
  cat("\n")
  cat("\n")
  
})


output$printselected <- renderPrint({
  
  req(mytransf())  
  # cat("You can select the rows in the table above in order to display the gene names")
  # cat("\n")
  # cat("\n")
    for(i in 1:length(mytransf())){
      cat(paste("GOID and Term: " , unique(mytransf()[[i]]$Term)))
      cat("\n")
      cat("Genes: ")
      cat(paste( mytransf()[[i]]$Genes, collapse = " ,"))
      cat("\n")
      cat("\n")
    }

})



output$savegohmdav = downloadHandler( paste0(basename(file_path_sans_ext(projectname())), '_go.',"xlsx", sep = ''),
  content = function(file) {
    
    withProgress(message = 'Creation of the xlsx table:',
                 value = 0, {
                   n <- NROW(50)
                   for (i in 1:n) {
                     incProgress(1 / n, detail = "Please wait...")
                   }
    
    
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
    })
    
    
    
  }
)

# output$savegofiltered = downloadHandler( paste0(basename(file_path_sans_ext(projectname())),
#                                         '_go_filtered.',
#                                         "xlsx",
#                                         sep = ''),
#                                  content = function(file) {
#                                    
#                                    withProgress(message = 'Creation of the xlsx table:',
#                                                 value = 0, {
#                                                   n <- NROW(50)
#                                                   for (i in 1:n) {
#                                                     incProgress(1 / n, detail = "Please wait...")
#                                                   }
#                                                   
#                                                   
#                                                   library(xlsx)
#                                                   
#                                                   for (i in 1:length(davidwebservice())) {
#                                                     if (i == 1)
#                                                       write.xlsx(file = file,
#                                                                  davidwebservice()[[i]],
#                                                                  sheetName = paste("Cluster", i))
#                                                     else
#                                                       write.xlsx(
#                                                         file = file,
#                                                         davidwebservice()[[i]],
#                                                         sheetName = paste("Cluster", i),
#                                                         append = TRUE
#                                                       )
#                                                   }
#                                                 })
#                                    
#                                    
#                                    
#                                  }
# )
# 
# 
# output$downloadvennset = downloadHandler(
#   'venns-filtered.csv',
#   content = function(file) {
#     s = input$davidgo_rows_all
#     write.csv2(vennfinal()[s, , drop = FALSE], file)
#   }
# )


#gores$obj <- NULL

#Species <- eventReactive(input$DAVID,{


Species <- reactive({
  if (input$Species == "Homo sapiens" || input$Speciesvenn == "Homo sapiens") {
    # human
    library("org.Hs.eg.db")
    mypack = list(org.Hs.egALIAS2EG, org.Hs.egSYMBOL)
    return(mypack)
  }
  else if (input$Species == "Mus musculus" || input$Speciesvenn == "Mus musculus" ) {
    # Mouse
    library("org.Mm.eg.db")
    mypack = list(org.Mm.egALIAS2EG, org.Mm.egSYMBOL)
    return(mypack)
  }
  else if (input$Species == "Danio rerio" || input$Speciesvenn == "Danio rerio") {
    #Zebra fish
    library("org.Dr.eg.db")
    mypack = list(org.Dr.egALIAS2EG, org.Dr.egSYMBOL)
    return(mypack)
  }
  else if (input$Species == "Gallus gallus" || input$Speciesvenn == "Gallus gallus") {
    # chicken
    library("org.Gg.eg.db")
    mypack = list(org.Gg.egALIAS2EG, org.Gg.egSYMBOL)
    return(mypack)
  }
  else if (input$Species == "equCab2" || input$Speciesvenn == "equCab2") {
    # horse
    library("org.Gg.eg.db")
    mypack = org.Mm.egALIAS2EG
    return(mypack)
  }
  else if (input$Species == "Caenorhabditis elegans" || input$Speciesvenn == "Caenorhabditis elegans") {
    # cC elegans
    library("org.Ce.eg.db")
    mypack = list(org.Ce.egALIAS2EG, org.Ce.egSYMBOL)
    return(mypack)
  }
  else if (input$Species == "Rattus norvegicus" || input$Speciesvenn == "Rattus norvegicus") {
    # Rat
    library("org.Rn.eg.db")
    mypack = list(org.Rn.egALIAS2EG, org.Rn.egSYMBOL )
    return(mypack)
  }
  else if (input$Species == "Sus scrofa" || input$Speciesvenn == "Sus scrofa") {
    # Pig
    library("org.Ss.eg.db")
    mypack = list(org.Ss.egALIAS2EG, org.Ss.egSYMBOL)
    return(mypack)
  }
  
})
