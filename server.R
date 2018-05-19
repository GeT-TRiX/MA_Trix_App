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
  
  
  output$downloadData <- downloadHandler(
    filename <-function(){
      paste("sampleData",".zip", sep='')
    },
    content <- function(file) {
      file.copy("data/sampleData.zip", file)
    },
    contentType = "zip"
  )
  
  observeEvent(input$resetAll, {
    reset("form")
  })
  
  
  observeEvent(input$session,{
    output$SessionInfo <- renderText(paste(capture.output(sessionInfo()), collapse = "<br>"))
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
  
  
  observeEvent(input$GO,{
    
    testad <- reactive({
      req(hmobj$hm)
      final = gosearch(hmobj$hm,input$Species, "geneSymbol", 20,50 )
      return(final)
    })
    
    gores$obj <- testad()
    
    
  
    output$clustgo <- renderPrint({
      req(input$cutgo)
      
      mycut <- reactive({
        
        return(input$cutgo)
      })
      
      if(!length(testad()[[1]]) == 0) {
        for(go in 1:input$numberGenes) {
          
          cat(paste("GOID:",as.character(GOID(testad()[[mycut()]][[1]][[go]]))))
          cat("\n")
          cat(paste("Term:",as.character(Term(testad()[[mycut()]][[1]][[go]]))))
          cat("\n")
          cat(paste("Definition:",as.character(Definition(testad()[[mycut()]][[1]][[go]]))))
          cat("\n")

          cat("--------------------------------------\n")
        }
      }
      else
        print("Sorry, no enriched genes for this cluster")

   })
  
  })
  
  Species <- reactive({ 
    
    if(input$Genome == "hg19"){ # human
      require("org.Hs.eg.db")
    }
    else if (input$Genome == "mm9"){ # mouse
      require("org.Mm.eg.db")
    }
    else if(input$Genome == "danRer6"){ #Zebra fish
      require("org.Dr.eg.db")
    }
    else if(input$Genome == "galGal3"){ # chicken
      require("org.Gg.eg.db")  
    }
    else if(input$Genome == "equCab2"){ # horse
      require("org.Gg.eg.db")  
    }
    else if(input$Genome == "ce6"){ # cC elegans
      require("org.Gg.eg.db")  
    }
    else if(input$Genome == "rn4"){ # Rat
      require("org.Gg.eg.db")  
    }
    else if(input$Genome == "Pig"){ # Rat
      require("org.Ss.e")  
    }
    else if(input$Genome == "rn4"){ # Rat
      require("org.Gg.eg.db")  
    }
    else if(input$Genome == "rn4"){ # Rat
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
