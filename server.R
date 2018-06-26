shinyServer(function(input, output,session) {
  
  hide(id = "loading-content", anim = TRUE, animType = "fade",time=2)
  hide(id = "loading-content-bar", anim = TRUE, animType = "fade",time=2)
  
  #######################################################
  ##                                                   ##
  ##                    LOAD FILES                     ##
  ##                                                   ##
  #######################################################
  
  source(file.path("server", "csvFile.R"), local = TRUE)$value #
  
  ##########################################
  ######## Widget update and info         ##
  ##########################################
  
  source(file.path("server", "matrixwidg.R"), local = TRUE)$value #
  
  ##########################################
  ######## Datarender                     ##
  ##########################################
  
  source(file.path("server", "renderertable.R"), local = TRUE)$value #
  
  ##########################################
  ######## HOME page                      ##
  ##########################################

  source(file.path("server", "datasummary.R"), local = TRUE)$value #
  source(file.path("server", "renderertable.R"), local = TRUE)$value #
  source(file.path("server", "checkboxgrp.R"), local = TRUE)$value #
  
  ################################
  ######## PCA page             ##
  ################################
  
  source(file.path("server", "PCAshiny.R"), local = TRUE)$value #
  #source(file.path("server", "plotandsave.R"), local = TRUE)$value #
  source(file.path("server", "PCAsandp.R"), local = TRUE)$value #
  source(file.path("server", "colforpca.R"), local = TRUE)$value #
  
  ################################
  ######## Venn page            ##
  ################################
  
  source(file.path("server", "Venn.R"), local = TRUE)$value #
  source(file.path("server", "Vennrender.R"), local = TRUE)$value #
  source(file.path("server", "grepcol.R"), local = TRUE)$value # adjusted
  source(file.path("server", "Venninter.R"), local = TRUE)$value # adjusted
  source(file.path("server", "trackervenn.R"), local = TRUE)$value #
  
  ################################
  ######## Venn GO              ##
  ################################
  
  source(file.path("server", "vennquery.R"), local = TRUE)$value # adjusted
  
  ##########################################
  ######## Grep project name              ##
  ##########################################
  
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

  ################################
  ######## Heatmap page         ##
  ###############################"
  
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
  
  ##########################################
  ######## GO enrichissment               ##
  ##########################################
  
  source(file.path("server", "shinygo.R"), local = TRUE)$value #
  
  ################################
  ######## cutheatmap page      ##
  ################################
  
  source(file.path("server", "cutheatmap.R"), local = TRUE)$value #
  
})


