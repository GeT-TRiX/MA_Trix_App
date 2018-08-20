### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



shinyjscode <- "
shinyjs.init = function() {
  $(window).resize(shinyjs.calcHeight);
}
shinyjs.calcHeight = function() { 
  Shiny.onInputChange('plotHeight', $(window).height());
}
"



shinyServer(function(input, output,session) {
  
  hide(id = "loading-content", anim = TRUE, animType = "fade",time=2)
  hide(id = "loading-content-bar", anim = TRUE, animType = "fade",time=2)
  
  
  
  plotHeight <- reactive({ 
    ifelse(is.null(input$plotHeight), 0, (input$plotHeight/1.25))
    #print(input$plotHeight)
  })
  
  
  #######################################################
  ##                                                   ##
  ##                    LOAD FILES                     ##
  ##                                                   ##
  #######################################################
  
  #source(file.path("server", "csvFile.R"), local = TRUE)$value #
  csvf <- callModule(csvFile, "datafile",stringsAsFactors = FALSE)

  ##########################################
  ######## Widget update and info         ##
  ##########################################
  
  source(file.path("server", "matrixwidg.R"), local = TRUE)$value #
  
  ##########################################
  ######## HOME page                      ##
  ##########################################

  source(file.path("server", "datasummary.R"), local = TRUE)$value #
  source(file.path("server", "renderertable.R"), local = TRUE)$value #
  source(file.path("server", "checkboxgrp.R"), local = TRUE)$value #
  
  ##########################################
  ######## Volcano page                   ##
  ##########################################

  
  volcano <- reactive({
    req(csvf())
    
    EnhancedVolcano(csvf()[[3]], lab= csvf()[[3]]$GeneName , x = paste0("logFC_",input$volcacomp) , 
                    y = paste0(ifelse(input$method == "FDR", "adj.P.Val_","P.value_"),input$volcacomp), 
                    topgenes = input$topvolc, DrawConnectors = ifelse(is.na(input$topvolc),F,T),
                    pCutoff = input$volcpval ,FCcutoff = input$volcfc ,transcriptPointSize = 1,transcriptLabSize = 3.0,
                    title =  gsub("-"," versus " ,input$volcacomp),cutoffLineType = "twodash",
                    cutoffLineCol = "black",cutoffLineWidth = 1,legend=c("NS","Log (base 2) fold-change","P value",
                                                                         "P value & Log (base 2) fold-change"))
  })
  
  
  output$volcanoplot <- renderPlot({
   
  validate(need(csvf(), 'You need to import data to visualize this plot!'))
    
    req(input$volcacomp)
    volcano()

  },  height = plotHeight)
  
  output$compvolc <- renderUI({
    req(adjusted())
    selectInput("volcacomp", "Choose a comparison", choices = colnames(adjusted()[[1]][,-1]))
  })
  
  
  output$savevolcano <- downloadHandler(filename <- function() {
    paste0(basename(file_path_sans_ext(projectname())), '_volcano.', input$formvolc, sep =
             '')
  },
  content <- function(file) {
    if (input$formvolc == "pdf")
      
      pdf(file,
          width = 12,
          height = 12,
          pointsize = 12)
    
    
    else if (input$formvolc == "png")
      
      png(
        file,
        width = 2500,
        height = 2500,
        units = "px",
        pointsize = 12,
        res = 100
      )
    else
      ggsave(file,device=cairo_ps, fallback_resolution = 600)

    
    plot(volcano())
    dev.off()
  })
  
  
  ################################
  ######## PCA page             ##
  ################################
  
  source(file.path("server", "PCAshiny.R"), local = TRUE)$value #
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
  ######## Jvenn                ##
  ################################
  
  
  observe({
    req(vennlist(),user_cont())
    if(input$dispvenn == "genes")
      isolate(
      Rtojs <- toJvenn(vennlist()[[2]],user_cont()))
    else
      isolate(
      Rtojs <- toJvenn(vennlist()[[1]],user_cont()))
    
    #thisiswhy <-input$typejv
    Mymode <-  input$updamod # Mode
    Myfont <-  input$myfont # Font size
    Mystat <-  input$mystat # Stat
    session$sendCustomMessage(type="updatejvenn", Rtojs)

    
  })
  
  
  output$renderer <- renderPrint({
    req(input$together,input$selcontjv)
    #print(typeof(input$together))
    #print(class(input$together))
    cat("The intersection is", input$together,"With the following genes \n")
  })
  
  output$renderer2 <- renderText({
    req(input$testons)
    cat(input$testons)

  })
  
  
  observe({
    req(input$together)
    print(typeof(input$together))
    print(class(input$together))
    print(input$together)
  })
  
  ################################
  ######## Venn GO              ##
  ################################
  
  source(file.path("server", "vennquery.R"), local = TRUE)$value # adjusted
  
  ##########################################
  ######## Grep project name              ##
  ##########################################
  
  
  file_name <- reactive({
    req(csvf())
    inFile <- csvf()[[4]]
    print(inFile)
    if (is.null(csvf()))
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
  ################################
  
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
  source(file.path("server", "highchartshiny.R"), local = TRUE)$value #
  
  ################################
  ######## cutheatmap page      ##
  ################################
  
  source(file.path("server", "cutheatmap.R"), local = TRUE)$value #
  
  ##########################################
  ######## Contact chat                   ##
  ##########################################
  
  source(file.path("server", "shinychat.R"), local = TRUE)$value #
  
  
})


