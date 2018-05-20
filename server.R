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
  
  gores <- reactiveValues()
  
  obsC <- observe({
    print(unique(hmobj$hm$cluster))
    
    
  })
  
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
  
  
  observeEvent(input$GO, {
    
    testad <- eventReactive(input$GO, {
      req(hmobj$hm)
      
      
      withProgress(message = 'Performing GO enrichment:',
                   value = 0, {
                     n <- NROW(50)
                     for (i in 1:n) {
                       incProgress(1 / n, detail = "Please wait...")
                     }
                     
                     final = tryCatch({
                       gosearch(hmobj$hm, input$Species, "geneSymbol")
                     },
                     error = function(e) {
                       warning("ERROR")
                     })
                     
                   })
      return(final)
    })
    
    gores$obj <- testad()
    
    
    
    slidergoen <- reactive({
      req(testad())
      req(input$cutgo)
      
      sliderInput("slidergo", label = h3("Slider Range"), min = 1, 
                  max = length(testad()[[input$cutgo]]$category), value = c(1, 25))
      
    })
    
    output$slidergo <- renderUI({
      slidergoen()
    })
    
    
    observe({
      req(testad())
      print(input$slidergo)
      print(input$slidergo[[1]])
    })
    
    output$savego <- downloadHandler( ## not working

      filename = function() {
         paste(basename(file_path_sans_ext(input$filename)),
               'enrichment_clusters',
               '.txt', sep = '')
       },
      content = function() {
        write.csv(testad()[[1]], file, row.names = FALSE)
        #wclust(testad(),"ok.txt", 15, 25)
      }
    )
    
    
    output$clustgo <- renderPrint({
      req(input$cutgo)
      req(input$slidergo)
      x <- input$cutgo
      print(input$slidergo)
      if (!is.null(testad()[[as.integer(x)]])) {
        for (go in input$slidergo[[1]]:input$slidergo[[2]]) {
          cat(paste("GOID:", (GOID(
            testad()[[as.integer(x)]][[1]][[go]]
          ))))
          cat("\n")
          cat(paste("Term:", (Term(
            testad()[[as.integer(x)]][[1]][[go]]
          ))))
          cat("\n")
          cat(paste("Definition:", (Definition(
            testad()[[as.integer(x)]][[1]][[go]]
          ))))
          cat("\n")
          
          cat("--------------------------------------\n")
        }
      }
      else
        print("Sorry, no enriched genes for this cluster")
      
    })
    
  })
  
  Species <- reactive({
    if (input$Genome == "hg19") {
      # human
      require("org.Hs.eg.db")
    }
    else if (input$Genome == "mm9") {
      # mouse
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
      require("org.Gg.eg.db")
    }
    else if (input$Genome == "rn4") {
      # Rat
      require("org.Gg.eg.db")
    }
    else if (input$Genome == "Pig") {
      # Rat
      require("org.Ss.e")
    }
    else if (input$Genome == "rn4") {
      # Rat
      require("org.Gg.eg.db")
    }
    else if (input$Genome == "rn4") {
      # Rat
      require("org.Gg.eg.db")
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
