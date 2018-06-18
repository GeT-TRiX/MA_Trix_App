shinyServer(function(input, output,session) {
  
  hide(id = "loading-content", anim = TRUE, animType = "fade",time=2)
  hide(id = "loading-content-bar", anim = TRUE, animType = "fade",time=2)
  
  #####################################################
  ##
  ##                    LOAD FILES
  ##
  #####################################################
  
  
  
  source(file.path("server", "csvFile.R"), local = TRUE)$value #
  
  
  #########################################
  ######## HOME page                      #
  #########################################
  
  output$downloadData <- downloadHandler(filename <- function() {
    paste("sampleData", ".zip", sep = '')
  },
  content <- function(file) {
    file.copy("data/sampleData.zip", file)
  },
  contentType = "zip")
  
  observeEvent(input$resetAll, {
    reset("form")
  })
  
  source(file.path("server", "datasummary.R"), local = TRUE)$value #
  source(file.path("server", "renderertable.R"), local = TRUE)$value #
  source(file.path("server", "checkboxgrp.R"), local = TRUE)$value #
  
  
  ###############################
  ######## PCA page             #
  ###############################
  
  source(file.path("server", "PCAshiny.R"), local = TRUE)$value #
  source(file.path("server", "plotandsave.R"), local = TRUE)$value #
  
  source(file.path("server", "PCAsandp.R"), local = TRUE)$value #
  source(file.path("server", "colforpca.R"), local = TRUE)$value #
  
  
  
  ###############################
  ######## Venn page           #
  ###############################
  
  source(file.path("server", "Venn.R"), local = TRUE)$value #
  source(file.path("server", "Vennrender.R"), local = TRUE)$value #
  source(file.path("server", "grepcol.R"), local = TRUE)$value # adjusted
  
  
  vennchoice <- reactive({
      if (is.null (input$intscol))
        return(NULL)
      else
        return(input$intscol)
    })
  
  output$myselvenn <- renderUI({
    req(user_cont())
    #intscol <- names(user_cont())#names(adjusted()[[1]][,-1])
    selectInput(
      'intscol',
      'Specify your interaction(s):',
      choices = names(user_cont()),
      multiple = TRUE
    )
  })
  
  venninter <- reactive({
    req(vennlist(), user_cont())
    myelist <- setvglobalvenn(vennlist(), user_cont())
    return(myelist)
  })
  
  
  vennfinal <- reactive({
    req(vennchoice())
    if (is.null(vennchoice))
      return(NULL)
    
    reordchoice <- vennchoice() %>%
      factor(levels = names(adjusted()[[1]][, -1])) %>%
      sort() %>%
      paste(collapse = "")
    
    resfinal = csvf()[[3]] %>%
      filter(ProbeName %in% venninter()[[reordchoice]]) %>%
      select(ProbeName, GeneName, paste0("logFC_", vennchoice())) %>%
      mutate_if(is.numeric, funs(format(., digits = 3)))
      #mutate_if(is.numeric, funs(formatC(., format = "f")))
    return(resfinal)
  })
  
  
  output$topgenesvenn <- renderUI({
    req(vennfinal(), vennchoice())
    
    numericInput(
      'topgenes',
      'Top genes',
      50,
      min = 1,
      max = length(vennfinal()$ProbeName)
    )
  })
  
  
  venntopgenes <-
    reactive({
      if (is.null (input$topgenes))
        return(NULL)
      else
        return(input$topgenes)
    })
  
  output$vennresinter <-
    DT::renderDataTable(DT::datatable(vennfinal(), list(lengthMenu =  c('5', '15', '50'))), server =
                          F)
  
  
  output$downloadvennset = downloadHandler(
    'venns-filtered.csv',
    content = function(file) {
      s = input$vennresinter_rows_all
      write.csv2(vennfinal()[s, , drop = FALSE], file)
    }
  )
  
  
  plottopgenes <- eventReactive(input$topdegenes, {
    req(vennfinal(), vennchoice(), venntopgenes())
    mycont = paste0("logFC_", vennchoice())
    myplot <-
      topngenes(vennfinal()[input$vennresinter_rows_all, , drop = FALSE], mycont, venntopgenes(), input$meandup)
    return(myplot)
    
  })
  
  observeEvent(input$topdegenes, {
    isolate(output$barplotvenn <- renderPlot({
      req(plottopgenes())
      plotOutput(plottopgenes())
      
    }))
    
  })
  
 
  observe({
  
    if (input$fcvenn <=2)
      updateSliderInput(session, "fcvenn", label = "FC treshold", value = NULL,
                        min = 1, max = 10, step = .1)
    else
      updateSliderInput(session, "fcvenn", label = "FC treshold", value = NULL,
                        min = 1, max = 10, step = 1)
    
    if (input$fc <=2)
      updateSliderInput(session, "fc", label = "FC treshold", value = NULL,
                        min = 1, max = 10, step = .1)
    else
      updateSliderInput(session, "fc", label = "FC treshold", value = NULL,
                        min = 1, max = 10, step = 1)
    
  })
  
  
  
  observe({
    validate(need(csvf(), 'You need to import data to visualize this plot!'))
    
    output$savebarplot <- downloadHandler(filename <- function() {
      paste0(basename(tools::file_path_sans_ext(projectname())),
             '_venn_barplot.',
             input$formvenbar,
             sep = '')
    },
    content <- function(file) {
      if (input$formvenbar == "pdf")
        
        pdf(file,
            width = 16,
            height = 7,
            pointsize = 12)
      
      else if (input$formvenbar == "png")
        png(
          file,
          width = 1600,
          height = 700,
          units = "px",
          pointsize = 12,
          res = 100
        )
      else
        eps(
          file,
          width = 16,
          height = 7,
          pointsize = 12
        )
      
      print(plottopgenes())
      
      dev.off()
    })
    
  })
  source(file.path("server", "trackervenn.R"), local = TRUE)$value #
  
  
  #########################################
  ######## Grep project name              #
  #########################################
  
  observeEvent(input$heatm, {
    print(colnames(adjusted()[[1]]))
    cat(colnames(adjusted()[[1]]))
  })
  
  file_name <- reactive({
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL)
    else
      return (tools::file_path_sans_ext(inFile$name))
  })
  
  
  
  projectname <- reactive({
    req(file_name())
    projed <- strsplit(file_name(), "_")
    proj = grepl("^MA", projed[[2]])
    index = which(proj == T)
    myproj = list(projed[[2]][index], proj)
    if(length(myproj[[1]]) == 0){
      return(Sys.Date())
    }
    else
      return(myproj)
    
  })
  
  
  
  #########################################
  ######## citation packages              #
  #########################################
  
  
  
  mypacklist <- reactive({
    mysess <- sessionInfo()
    dfpack <- names(sessionInfo()$otherPkgs) %>%
      lapply(function(x)
        return(
          paste(mysess$otherPkgs[[x]]$Package, mysess$otherPkgs[[x]]$Version)
        )) %>%
      unlist() %>%
      cbind(., unlist(lapply(names(mysess$otherPkgs), function(x)
        return(paste(mysess$otherPkgs[[x]]$Title))))) %>%
      as.data.frame() %>%
      setNames(c('Version', "Title"))
    
    return(dfpack)
  })
  
  
  observeEvent(input$session, {
    req(mypacklist())
    output$sessinfo <- renderDataTable(mypacklist())
  })
  

  ###############################
  ######## Heatmap page         #
  ###############################
  
  
  
  source(file.path("server", "checkboxcontrast.R"), local = TRUE)$value #
  source(file.path("server", "changeheatmbut.R"), local = TRUE)$value #
  source(file.path("server", "hidevent.R"), local = TRUE)$value #
  source(file.path("server", "heatmapshiny.R"), local = TRUE)$value #
  source(file.path("server", "tracker.R"), local = TRUE)$value #
  source(file.path("server", "computemean.R"), local = TRUE)$value #
  source(file.path("server", "grepcol.R"), local = TRUE)$value #
  source(file.path("server", "indexselected.R"), local = TRUE)$value #
  source(file.path("server", "selgroupandcont.R"), local = TRUE)$value #
  source(file.path("server", "backgroundcolor.R"), local = TRUE)$value #
  source(file.path("server", "groupcolor.R"), local = TRUE)$value #
  source(file.path("server", "renderertable.R"), local = TRUE)$value #
  source(file.path("server", "cutheatmap.R"), local = TRUE)$value #
#   
#   #########################################
#   ######## GO enrichissment               #
#   #########################################
#   
#   #obsC <- observe(quote({ print(hmobj$hm) }), quoted = TRUE)
#   
#   #gores <- reactiveValues()
#   
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


  observe({
    req(input$tabset25)
    if (grepl("hmpan", input$tabset25)) {
      updateTabsetPanel(session, "mainhmtabset",
                        selected = "hmmainpan")
    }
    else if (grepl("cutpan", input$tabset25)) {
      updateTabsetPanel(session, "mainhmtabset",
                        selected = "cuthmmainpan")
    }
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

    genlist <- hmobj$hm[!duplicated(hmobj$hm$GeneName), ] %>%
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

  davidwebservice <- eventReactive(input$GO, { #Warning: Error in .jcall: org.apache.axis2.AxisFault: Read timed out

    req(hmobj$hm)

    withProgress(message = 'Performing GO enrichment:',
                 value = 0, {
                   n <- NROW(50)
                   for (i in 1:n) {
                     incProgress(1 / n, detail = "Please wait...")
                   }
                   library(RDAVIDWebService)
                   mygodavid = probnamtoentrez(hmobj$hm, Species()) %>%
                     davidquery(input$Species)

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




  output$davidgo <- renderDataTable({
    davidwebservice()[[as.numeric(input$cutgo)]][,-c(4, 6)]
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


  output$savego = downloadHandler( 'go.xlsx',
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



  })
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


  
})


