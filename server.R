source("function/compat.R")
source("function/formating.R")
source("function/PCA.R")
source("environnement/global.R")
source("function/decideTestTrix.R")
source("function/vennplot.R")
source("function/create_forked_task.R")
source("function/cutheat.R")

shinyServer(server <- function(input, output, session) {
  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "advanced", anim = TRUE)) ## hide and show event
  
  # shinyjs::onclick("toggleAdvancedPCA",
  #                  shinyjs::toggle(id = "advancedPCA", anim = TRUE))
  
  shinyjs::onclick("toggleAdvancedcolors",
                   shinyjs::toggle(id = "advancedcol", anim = TRUE))
  
  
  
  source(file.path("server", "changeheatmbut.R"), local = TRUE)$value
  
  
  
  #################################
  ######## Plot in the renderView #
  #################################
  
  heatmapfinal <- function() {
    isolate({
      plotHeatmaps(
        data.matrix(new_data()),
        formated(),
        droplevels(new_group()$Grp),
        workingPath = wd_path,
        prefix,
        suffix,
        my_palette = colorRampPalette(c(
          choix_col1(), my_intermediate(), choix_col3()
        ))(n = 75),
        k = input$clusters,
        Rowdistfun = input$dist ,
        Coldistfun = input$dist,
        keysize = input$key,
        mycex = input$legsize ,
        cexrow = input$rowsize ,
        cexcol = input$colsize ,
        meanGrp = input$meangrp,
        labColu = input$colname ,
        labRowu = input$rowname,
        mypal =  unlist(colors()),
        showcol = input$colname,
        showrow = input$rowname,
        genename = csvf()[[3]]$GeneName
      )
      
    })
  }
  
  p <- reactive({
    heatmapfinal()
  })
  
  test <- reactive({
    
    mycsv = csvf()[[3]]
    row.names(mycsv) = mycsv$X
    return(test)
    
  })
  
  
  cutfinal <- function() {
    isolate({
      cutHeatmaps(
        p(),
        height = input$cutheight ,
        exprData = data.matrix(new_data()),
        groups = droplevels(new_group()$Grp),
        DEGres =  test()[,-1],
        plot.boxplot = F,
        plot.stripchart = T,
        hmp.plot = F,
        num = input$cutcluster,
        probes.boxplot = T
      )
    })
  }
  
  output$cutcluster <- renderUI({
    cut02=cut(p()$rowDendrogram,h=input$cutheight)
    selectInput(
      "cutcluster",
      "Choose your cluster",
      choices =  seq(1,length(cut02$lower),by=1)
    )
  })
  
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else 
      cat("Average expression Z-score over replicates; ",length(d$pointNumber)," probes")
  })
  

  observeEvent(input$cutheat, {
    output$cutheatmap <- renderPlotly({
      ggplotly(cutfinal(), height = 800, width=1200)
    })
    
  })
  
  ###############################
  ######## tracker              #
  ###############################
  
  
  source(file.path("server", "tracker.R"), local = TRUE)$value
  
  ###############################
  ######## Plot&save heatm PCA  #
  ###############################
  
  PCAplot <- function() {
    pcapal = brewer.pal(10, "Paired") %>%
      list(brewer.pal(8, "Dark2")) %>%
      unlist()
    
    empty <- reactive ({
      if (is.null(colorspca()[[1]])) {
        palpca = pcapal
      }
      else
        palpca = unlist(colorspca())
      return(palpca)
      
    })
    
    
    p <- fviz_mca_ind(
      PCAres(),
      label = labeled(),
      habillage = csvf()[[2]]$Grp,
      addEllipses = input$ellipse ,
      ellipse.level = 0.8,
      repel = input$jitter,
      axes = c(as.integer(input$dim1), as.integer(input$dim2)),
      labelsize = input$labelsiize,
      pointsize = input$pointsiize
    )
    
    return(p + scale_color_manual(values = empty()))
  }
  
  
  source(file.path("server", "plotandsave.R"), local = TRUE)$value
  
  
  ###############################
  ######## Load the csv files   #
  ###############################
  
  source(file.path("server", "csvFile.R"), local = TRUE)$value
  
  ###############################
  ######## Adding mean by group #
  ###############################
  
  source(file.path("server", "computemean.R"), local = TRUE)$value
  
  #################################
  ######## Select the individuals #
  #################################
  
  source(file.path("server", "checkboxgrp.R"), local = TRUE)$value
  
  #################################
  ######## Select the comparisons #
  #################################
  
  source(file.path("server", "checkboxcontrast.R"), local = TRUE)$value
  
  #################################
  ######## Format the data frame  #
  #################################
  
  source(file.path("server", "grepcol.R"), local = TRUE)$value
  
  source(file.path("server", "indexselected.R"), local = TRUE)$value
  
  source(file.path("server", "datasummary.R"), local = TRUE)$value
  
  
  #################################
  ### Selected group and contrast #
  #################################
  
  source(file.path("server", "selgroupandcont.R"), local = TRUE)$value
  
  
  #########################################
  ######## Updating a colourInput         #
  #########################################
  
  source(file.path("server", "backgroundcolor.R"), local = TRUE)$value
  
  #########################################
  ######## Colors for the  groups         #
  #########################################
  
  source(file.path("server", "groupcolor.R"), local = TRUE)$value
  
  #########################################
  ######## Plot the data frame wiht input #
  #########################################
  
  source(file.path("server", "renderertable.R"), local = TRUE)$value
  
  #########################################
  ######## PCA part                       #
  #########################################
  
  source(file.path("server", "PCAsandp.R"), local = TRUE)$value
  
  source(file.path("server", "colforpca.R"), local = TRUE)$value
  
  
  #########################################
  ######## Venn part                      #
  #########################################
  
  source(file.path("server", "Venn.R"), local = TRUE)$value
  
  
  observeEvent(input$vennd, {
    output$myVenn <- renderPlot({
      Vennplot()
    }, width = 1400 , height = 1400, res = 100)
    
    
    output$savevenn <- downloadHandler(filename <- function() {
      paste0(basename(file_path_sans_ext("myfile")),
             '_venn_diagram.',
             input$formven,
             sep = '')
    },
    content <- function(file) {
      print(input$formven)
      if (input$formven == "emf")
        
        emf(
          file,
          width = 7,
          height = 7,
          pointsize = 12,
          coordDPI = 300
        )
      
      else if (input$formven == "png")
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
            width = 7,
            height = 7)
      
      
      plot(Vennplot())
      dev.off()
    })
    
  })
  
  
  
  #########################################
  ######## cutheatmap part                #
  #########################################
  
  
  
  
})

#shinyApp(ui = ui , server = server)
