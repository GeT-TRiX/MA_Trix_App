##################################
##################################
##                              ##
## Shiny app/server part        ##
##################################
##                              ##
## Author: Franck Soubès        ##
##################################
##################################


###############################
######## creating graph log   #
###############################

# options(shiny.reactlog=TRUE)
# showReactLog(time = TRUE)

######################################
######## Define Server Functionality #
######################################

shinyServer(server <- function(input, output, session) {
  
  ###############################
  ######## Loading js           #
  ###############################
  
  hide(id = "loading-content", anim = TRUE, animType = "fade",time=1.5)
  hide(id = "loading-content-bar", anim = TRUE, animType = "fade",time=1.5)
  
  ###############################
  ######## Load the csv files   #
  ###############################
  
  source(file.path("server", "csvFile.R"), local = TRUE)$value #
  
  #########################################
  ######## Example files                  #
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
  
  #########################################
  ######## citation packages              #
  #########################################
  

  
  mypacklist <- reactive({

    dfpack <- names(sessionInfo()$otherPkgs) %>%
      lapply(function(x) return(paste(mysess$otherPkgs[[x]]$Package, mysess$otherPkgs[[x]]$Version)))%>%
      unlist()%>%
      cbind(.,unlist(lapply(names(mysess$otherPkgs), function(x)return(paste(mysess$otherPkgs[[x]]$Title)))))%>%
      as.data.frame()%>%
      setNames( c('Version', "Title"))
    
    return(dfpack)
  })
  
  
  observeEvent(input$session, {
    req(mypacklist())
    output$sessinfo <- renderDataTable(mypacklist())
  })
  
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
      return(NULL) else return (tools::file_path_sans_ext(inFile$name))
  })     
  
   
  
  projectname <- reactive({
  
    req(file_name())
    projed <- strsplit(file_name(), "_")
    proj = grepl("^MA",projed[[1]])
    index = which(proj==T)
    myproj = list(projed[[1]][index],proj)
    
    return(myproj)
    
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
  
  
  vennchoice <- reactive({if (is.null (input$intscol)) return(NULL) else return(input$intscol)})
  
  output$myselvenn <- renderUI({
    req(user_cont())
    #intscol <- names(user_cont())#names(adjusted()[[1]][,-1])
    selectInput('intscol', 'Specify your interaction(s):', choices = names(user_cont()), multiple = TRUE)
  }) 
  
  venninter <- reactive({
    req(vennlist(), user_cont())
    myelist <- setvglobalvenn(vennlist(),user_cont())
    return(myelist)
  })
  
  
  vennfinal <- reactive({
    req(vennchoice())
    if(is.null(vennchoice))
      return(NULL)
    
    reordchoice <- vennchoice() %>%
      factor(levels = names(adjusted()[[1]][,-1] )) %>%
      sort() %>%
      paste(collapse="")
    
    resfinal= csvf()[[3]] %>% 
      filter(ProbeName %in% venninter()[[reordchoice]]) %>% 
      select(ProbeName,GeneName, paste0("logFC_", vennchoice())) %>%
      mutate_if(is.numeric, funs(formatC(., format = "f")))
    return(resfinal)
  })


  output$topgenesvenn <- renderUI({
  req(vennfinal(),vennchoice())
  
  numericInput('topgenes', 'Top genes', 50, 
                 min = 1, max = length(vennfinal()$ProbeName))
  })
  
  
  venntopgenes <- reactive({if (is.null (input$topgenes)) return(NULL) else return(input$topgenes)})

  output$vennresinter <- DT::renderDataTable(DT::datatable(vennfinal(),list(
    lengthMenu =  c('5', '15', '50'))), server=F) 
  
  
  output$downloadvennset = downloadHandler('venns-filtered.csv', content = function(file) {
    s = input$vennresinter_rows_all
    write.csv2(vennfinal()[s, , drop = FALSE], file)
  })
  
  
  plottopgenes <- eventReactive(input$topdegenes,{
    req(vennfinal(),vennchoice(),venntopgenes())
    mycont = paste0("logFC_", vennchoice())
    myplot <- topngenes(vennfinal()[input$vennresinter_rows_all, , drop = FALSE],mycont,venntopgenes(), input$meandup)
    return(myplot)
    
  })
  
  observeEvent(input$topdegenes, {
    isolate(output$barplotvenn <- renderPlot({
      req(plottopgenes())
      plotOutput(plottopgenes())
      
    }, width = 1100 , height = 600, res = 100))
    
  })
  
  
  observe({
    
    validate(
      need(csvf(), 'You need to import data to visualize this plot!'))
    
    output$savebarplot <- downloadHandler(filename <- function() {
      paste0(basename(tools::file_path_sans_ext(projectname())),
             '_venn_barplot.',
             input$formven,
             sep = '')
    },
    content <- function(file) {
      if (input$formven == "pdf")
        
        pdf(
          file,
          width = 19,
          height = 7,
          pointsize = 12
        )
      
      else if (input$formven == "png")
        png(
          file,
          width = 1600,
          height = 700,
          units = "px",
          pointsize = 12,
          res = 100
        )
      else
        cairo_ps(filename=file, width=16, height=7,pointsize = 12)
      
      print(plottopgenes())
      
      dev.off()
    })
    
  })
  
  source(file.path("server", "trackervenn.R"), local = TRUE)$value #
  
  #########################################
  ######## cutheatmap part                #
  #########################################
  
  source(file.path("server", "cutheatmap.R"), local = TRUE)$value #
  
  
  #########################################
  ######## GO enrichissment               #
  #########################################
  
  
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
    req(input$heatmconf)
    if (grepl("cutpan", input$heatmconf)) {
      updateTabsetPanel(session, "mainhmtabset",
                        selected = "cuthmmainpan")
    }
    else if (grepl("hmpan", input$heatmconf)) {
      updateTabsetPanel(session, "mainhmtabset",
                        selected = "hmmainpan")
    }
  })
  
  
  observe({
    req(input$mainhmtabset)
    if (grepl("cuthmmainpan",  input$mainhmtabset)) {
      updateTabsetPanel(session, "heatmconf",
                        selected = "cutpan")
    }
    else if (grepl("hmmainpan",  input$mainhmtabset)) {  #|dfhmclu|maingo
      updateTabsetPanel(session, "heatmconf",
                        selected = "hmpan")
    }
  })
  
  
  
  observe({
    
    # testad <- eventReactive(input$GO, {
    #   req(hmobj$hm)
    #   gores$obj <- NULL
    #   myl <- NULL
    #   
      # withProgress(message = 'Performing GO enrichment:',
      #              value = 0, {
      #                n <- NROW(50)
      #                for (i in 1:n) {
      #                  incProgress(1 / n, detail = "Please wait...")
      #                }
    #                  
    #                  final = tryCatch({
    #                    gosearch(hmobj$hm, input$Species, "geneSymbol", myl)
    #                  },
    #                  error = function(e) {
    #                    warning("ERROR")
    #                  })
    #                  
    #                })
    #   return(final)
    # })
    
  
    
    # slidergoen <- reactive({
    #   req(testad(), input$cutgo)
    #   
    #   x <- input$cutgo
    #   
    #   sliderInput(
    #     "slidergo",
    #     label = "Select (GO) range of observations",
    #     min = 1,
    #     max = length(testad()[[as.integer(x)]][[1]]),
    #     value = c(1, 25)
    #   )
    #   
    # })
    # 
    # output$slidergo <- renderUI({
    #   slidergoen()
    # })
    
    
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
    
    davidwebservice <- eventReactive(input$GO, {
      req(hmobj$hm)
      library(RDAVIDWebService)
      
      withProgress(message = 'Performing GO enrichment:',
                   value = 0, {
                     n <- NROW(50)
                     for (i in 1:n) {
                       incProgress(1 / n, detail = "Please wait...")
                     }
                     
        mygodavid = probnamtoentrez(hmobj$hm,Species()) %>%
          davidquery( input$Species) 
        
      })
      
      final = lapply(1:NROW(mygodavid),function(x)
        return(format(mygodavid[[x]], digits = 3)))
      
      return(final)
    })
    
    
    
    davidurl <- eventReactive( input$DAVID, {
      req(clustergrep())

      source_python('./python/add.py')
      mydavurl = enrichmentdav(clustergrep())
      mygloburl <- paste(`mydavurl`,",", "'_blank')")

      return(mygloburl)
    })
    
    
    observe({
      req(davidurl())
      url$myurl = davidurl()
    })

    # observe({
    #   req(csvf())
    #   print(input$Species)
    #   Species()
    # 
    # })

    
    # observeEvent(input$DAVID, {
    #   davidurl <- eventReactive(input$DAVID, {
    #     req(clustergrep())
    # 
    #     source_python('./python/add.py')
    #     enrichmentdav(clustergrep())
    #     
    #   })
    #   
    #   davidurl()
    # })
    
    
    # mydavidshow <- reactive({
    #   x<- input$cutgo
    #   print(x)
    #   mydf = davidwebservice()[[as.numeric(x)]]
    #   return(mydf)
    # })
    
    
    output$davidgo <- renderDataTable({
      davidwebservice()[[as.numeric(input$cutgo)]][, -c(4,6)] 
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
    
    
    output$savego = downloadHandler('go.xlsx', content = function(file) {
      
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
      
      
      # write.csv2(vennfinal()[s, , drop = FALSE], file)
      #davidxlsx()
    })
    
    
    
  })
    #gores$obj <- NULL
    
    #Species <- eventReactive(input$DAVID,{
  Species <- reactive({
    if (input$Species == "Homo sapiens") {# human
      library("org.Hs.eg.db")
      mypack = org.Hs.egALIAS2EG
      return(mypack)
    }
    else if (input$Species == "Mus musculus") {# Mouse
      library("org.Mm.eg.db")
      mypack = org.Mm.egALIAS2EG
      return(mypack)
    }
    else if (input$Species == "Danio rerio") {#Zebra fish
      library("org.Dr.eg.db")
      mypack = org.Dr.egALIAS2EG
      return(mypack)
    }
    else if (input$Species == "Gallus gallus") {# chicken
      library("org.Gg.eg.db")
      mypack = org.Gg.egALIAS2EG
      return(mypack)
    }
    else if (input$Species == "equCab2") {# horse
      library("org.Gg.eg.db")
      mypack = org.Mm.egALIAS2EG
      return(mypack)
    }
    else if (input$Species == "Caenorhabditis elegans") {# cC elegans
      library("org.Ce.eg.db")
      mypack = org.Ce.egALIAS2EG
      return(mypack)
    }
    else if (input$Species == "Rattus norvegicus") {# Rat
      library("org.Rn.eg.db")
      mypack = org.Rn.egALIAS2EG
      return(mypack)
    }
    else if (input$Species == "Sus scrofa") {# Pig
      library("org.Ss.eg.db")
      mypack = org.Ss.egALIAS2EG
      return(mypack)
    }
    
  })
  
    

  #})
  
  
  
  
  #########################################
  ######## KEGG enrichissment             #
  #########################################
  
  
  
  #########################################
  ######## graph ???????????              #
  #########################################
  
  

  
})

#shinyApp(ui = ui , server = server)
