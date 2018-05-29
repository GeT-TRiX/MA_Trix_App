###############################
######## Loading functions    #
###############################

source("function/heatmtruncated.R")
source("function/formating.R")
source("function/PCA.R")
source("function/decideTestTrix.R")
source("function/vennplot.R")
source("function/create_forked_task.R")
source("function/cutheat.R")
source("function/gosearch.R")
source("environnement/global.R")

###############################
######## creating graph log   #
###############################


# options(shiny.reactlog=TRUE)
# showReactLog(time = TRUE)


shinyServer(server <- function(input, output, session) {
  ###############################
  ######## Load the csv files   #
  ###############################
  
  
  source(file.path("server", "csvFile.R"), local = TRUE)$value #
  
  
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
  
  
  observeEvent(input$session, {
    output$SessionInfo <-
      renderText(paste(capture.output(sessionInfo()), collapse = "<br>"))
  })
  
  observeEvent(input$heatm, {
    print(colnames(adjusted()[[1]]))
  })
  
  file_name <- reactive({
    inFile <- input$file
    
    if (is.null(inFile))
      return(NULL) else return (tools::file_path_sans_ext(inFile$name))
  })     
  
   
  
  projectname <- reactive({
  
    req(file_name())
    projed <- strsplit(file_name(), "_")
    proj = grepl("^MA",projed[[1]])
    index = which(proj==T)
    return(projed[[1]][index])
    
  })
  
  # observe({
  #   req(file_name())
  #   print(file_name()[[1]])
  #   
  # })
  # 
  # output$myFileName <- renderText({ projectname()})
  
  
  ##################################
  ######## Hide and modify buttons #
  ##################################
  
  source(file.path("server", "changeheatmbut.R"), local = TRUE)$value #
  source(file.path("server", "hidevent.R"), local = TRUE)$value #
  
  #################################
  ######## Plot in the renderView #
  #################################
  
  source(file.path("server", "heatmapshiny.R"), local = TRUE)$value #
  
  ###############################
  ######## tracker              #
  ###############################
  
  source(file.path("server", "tracker.R"), local = TRUE)$value #
  
  ###############################
  ######## Plot&save heatm PCA  #
  ###############################
  
  source(file.path("server", "PCAshiny.R"), local = TRUE)$value #
  source(file.path("server", "plotandsave.R"), local = TRUE)$value #
  
  ###############################
  ######## Adding mean by group #
  ###############################
  
  source(file.path("server", "computemean.R"), local = TRUE)$value #
  
  #################################
  ######## Select the individuals #
  #################################
  
  source(file.path("server", "checkboxgrp.R"), local = TRUE)$value #
  
  #################################
  ######## Select the comparisons #
  #################################
  
  source(file.path("server", "checkboxcontrast.R"), local = TRUE)$value #
  
  #################################
  ######## Format the data frame  #
  #################################
  
  source(file.path("server", "grepcol.R"), local = TRUE)$value #
  source(file.path("server", "indexselected.R"), local = TRUE)$value #
  source(file.path("server", "datasummary.R"), local = TRUE)$value #
  
  #################################
  ### Selected group and contrast #
  #################################
  
  source(file.path("server", "selgroupandcont.R"), local = TRUE)$value #
  
  #########################################
  ######## Updating a colourInput         #
  #########################################
  
  source(file.path("server", "backgroundcolor.R"), local = TRUE)$value #
  
  #########################################
  ######## Colors for the  groups         #
  #########################################
  
  source(file.path("server", "groupcolor.R"), local = TRUE)$value #
  
  #########################################
  ######## Plot the data frame wiht input #
  #########################################
  
  source(file.path("server", "renderertable.R"), local = TRUE)$value #
  
  #########################################
  ######## PCA part                       #
  #########################################
  
  source(file.path("server", "PCAsandp.R"), local = TRUE)$value #
  source(file.path("server", "colforpca.R"), local = TRUE)$value #
  
  #########################################
  ######## Venn part                      #
  #########################################
  
  source(file.path("server", "Venn.R"), local = TRUE)$value #
  source(file.path("server", "Vennrender.R"), local = TRUE)$value #
  
  #########################################
  ######## cutheatmap part                #
  #########################################
  
  source(file.path("server", "cutheatmap.R"), local = TRUE)$value #
  
  
  #########################################
  ######## GO enrichissment               #
  #########################################
  
  
  #obsC <- observe(quote({ print(hmobj$hm) }), quoted = TRUE)
  
  #gores <- reactiveValues()
  
  
  gores <- reactiveValues()
  
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
    testad <- eventReactive(input$GO, {
      req(hmobj$hm)
      gores$obj <- NULL
      myl <- NULL
      
      withProgress(message = 'Performing GO enrichment:',
                   value = 0, {
                     n <- NROW(50)
                     for (i in 1:n) {
                       incProgress(1 / n, detail = "Please wait...")
                     }
                     
                     final = tryCatch({
                       gosearch(hmobj$hm, input$Species, "geneSymbol", myl)
                     },
                     error = function(e) {
                       warning("ERROR")
                     })
                     
                   })
      

      return(final)
    })
    
    
    # observe({
    #   req(gores$down)
    #   print(testad()[[1]])
    #   print(testad()[[2]])
    #   
    # })
    
    
    slidergoen <- reactive({
      req(testad(), input$cutgo)
      
      x <- input$cutgo
      
      sliderInput(
        "slidergo",
        label = "Select (GO) range of observations",
        min = 1,
        max = length(testad()[[as.integer(x)]][[1]]),
        value = c(1, 25)
      )
      
    })
    
    output$slidergo <- renderUI({
      slidergoen()
    })
    
    
    observe({
      req(input$heatmconf)
      heatidsw <- input$heatmconf
      if (grepl("cutpan", heatidsw)) {
        updateTabsetPanel(session, "mainhmtabset",
                          selected = "cuthmmainpan")
      }
      else if(grepl("hmpan", heatidsw)) {
        updateTabsetPanel(session, "mainhmtabset",
                          selected = "hmmainpan")
       }
      })
    
    observe({
      req(input$mainhmtabset)
      heatidswmain <- input$mainhmtabset
      if (grepl("cuthmmainpan", heatidswmain)) {
        updateTabsetPanel(session, "heatmconf",
                          selected = "cutpan")
      }
      else if(grepl("hmmainpan", heatidswmain)) { #|dfhmclu|maingo
        updateTabsetPanel(session, "heatmconf",
                          selected = "hmpan")
      }
    })
    

    
    # output$savego <- downloadHandler(
    #   
    #   filename = function() {
    #     paste(basename(file_path_sans_ext(input$filename)),
    #           'enrichment_clusters',
    #           '.txt',
    #           sep = '')
    #   },
    #   content = function() {
    #     write.csv(gores$down[[1]],file,row.names = F)
    #   }
    # )
    
    
    clustergrep <- reactive({
      
      req(hmobj$hm, input$cutgo)
      genlist <- hmobj$hm[!duplicated(hmobj$hm$GeneName), ]
      genlist <- genlist %>%
        dplyr::select(cluster, GeneName)   %>%
        filter(cluster == input$cutgo)
      mygensymb = genlist$cluster %>%
        length() %>%
        matrix(1, .) %>%
        as.double()
      names(mygensymb) = genlist$GeneName
      mygensymb = as.list(names(mygensymb))
      mygensymb <-mygensymb[lapply(mygensymb, function(x)
          length(grep("chr", x, value = FALSE))) == 0] # remove non-annotated genes
      
      
      return(mygensymb)
    })
    
    
  
    
    #davidurl <- reactive({
    
    observeEvent(input$DAVID, {
      davidurl <- eventReactive(input$DAVID, {
        req(clustergrep())
        #use_python("/usr/bin/python")
        # os <- import("os")
        # requests <- import("requests")
        # os$listdir(".")
        # request$listdir(".")
        source_python('./python/add.py')
        
        enrichmentdav(clustergrep())
        
      })
      
      davidurl()
      
    })
    
    
    output$clustgo <- renderPrint({
      validate(
        need(csvf(), 'You need to import data to visualize the data!') %next%
          need(input$cutgo, 'You need to click on the heatmap button! then on the run GO button')
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
    #gores$obj <- NULL
  })
  
  Species <- reactive({
    if (input$Genome == "hg19") {
      # human
      require("org.Hs.eg.db")
    }
    else if (input$Genome == "mm9") {
      # Mouse
      require("org.Mm.eg.db")
    }
    else if (input$Genome == "danRer6") {
      #Zebra fish
      require("org.Dr.eg.db")
    }
    else if (input$Genome == "galGal3") {
      # chicken
      require("org.Gg.eg.db")
    }
    else if (input$Genome == "equCab2") {
      # horse
      require("org.Gg.eg.db")
    }
    else if (input$Genome == "ce6") {
      # cC elegans
      require("org.Ce.eg.db")
    }
    else if (input$Genome == "rn4") {
      # Rat
      require("org.Rn.eg.db")
    }
    else if (input$Genome == "susScr3") {
      # Pig
      require("org.Ss.eg.db")
    }
    
  })
  
  
  #########################################
  ######## KEGG enrichissment             #
  #########################################
  
  
  
  #########################################
  ######## graph ???????????              #
  #########################################
  
  
})

#shinyApp(ui = ui , server = server)
