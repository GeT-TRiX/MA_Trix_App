### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


###############################
########heatmap function & co #
###############################

boolhm <- F


output$heatmbool <- reactive({
  boolhm
})

observe({
  print(boolhm)
  
})

outputOptions(output, "heatmbool", suspendWhenHidden = F)

observe({
  req(csvf(),length(selected_test()) >0,input$reactheat == T| global$clicked)
  
  observe({boolhm <<-T}) # modify and lock the bool value to false
  
  output$heatmbool <- reactive({
    boolhm
  })
  
})


#' rowname is a reactive function which aim is to hide or show the rownames
#'
#' @param input$rowname  a boolean radio button input
#'
#' @return rowname a reactive boolean value
#'
#' @export

rowname <- reactive({
  rowname <- switch(input$rowname,
                    hide = F,
                    show = T,
                    F)
  return(rowname)
})

#' colname is a reactive function which aim is to show or hide the colnames
#'
#' @param input$colname  a boolean radio button input
#'
#' @return colname a reactive  reactive boolean value
#'
#' @export

colname <- reactive({
  colname <- switch(input$colname,
                    hide = T,
                    show = F,
                    F)
  return(colname)
})


heatmapobj <- NULL # declare outside the observeEvent
formatidus <- NULL
hmbis <- reactiveValues()
hmboth <- reactiveValues()
hmobj <- reactiveValues()
hmsize <- reactiveValues()


observe({
  
  #' heatmapfinal is an isolate function that only react to a user's click on the heatmap button 
  #' 
  #' @param hmbis[[1]] a data frame with all the individuals selected
  #' @param formated  a data frame with the indexes corresponding to the sigificant genes
  #' @param new_group  a data frame with the corresponding groups 
  #' @param workingPath the current user's repository 
  #' @param my_palette a vector of colors 
  #' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
  #' @param Rowdistfun a character value set by the user to defined the method to calculate the dendogram matrix distance for the genes input$dist
  #' @param Coldistfun a character value set by the user to defined the method to calculate the dendogram matrix distance for the contrasts input$dist
  #' @param mycex a numeric value which aim is to change the size of the legend in the heatmap defined by the user input$legsize
  #' @param cexrow  a numeric value to change the size of the police legend for the rows input$rowsize
  #' @param cexcol a numeric value to change the size of the police legend for the columns input$colsize
  #' @param meanGrp a boolean value to compute or not the mean of each contrasts in the heatmap input$meangrp
  #' @param mypal a list of values 
  #' @param showcol a boolean value used to hide or show the colnames input$colname
  #' @param showrow a boolean value used to hide or show the rownames input$rowname
  #' @param genename a data frame 
  #' @param notplot a boolean value for applying dev.off or not on the heatmap
  #' @param rowv  dendogram object
  #' @param ColOrd  positive numbers, used as cex.axis in for the row or column axis labeling
  #' @param gpcol  matrix with colors associated to each groups 
  #' @param gpcolr  matrix with gray color depending on the clusters
  #' @param distfunTRIX function that computes whether euclidian or pearson for Hierarchical Clustering
  #' @param height a numeric object corresponding to the selected cluster to display
  #' @param rastering a graphical boolean
  #' @param geneSet 
  #'
  #' @return  a data frame with the cluster and the corresponding genes 
  #' 
  #' @export
  #' 
  
  heatmapfinal <- function(isplot  = F, israstering = T) {
    if (is.null(my_intermediate()))
      mypal = (colorRampPalette(c("green", "black", "red"))(n = 75))
    else
      mypal = (colorRampPalette(c(
        col_choice1(), my_intermediate(), col_choice3()
      ))(n = 75))
    
    plotHeatmaps(
      
      isolate(hmbis()[[1]]),
      geneSet =  isolate(hmbis()[[7]]),
      droplevels(new_group()$Grp),
      workingPath = wd_path,
      my_palette = (colorRampPalette(
        c(col_choice1(), my_intermediate(), col_choice3())
      )(n = 75)),
      mycex = input$legsize ,
      cexrow = input$rowsize ,
      cexcol = input$colsize ,
      mypal =  unlist(colors()),
      showcol = colname(),
      showrow = rowname(),
      genename =  csvf()[[3]],
      notplot = isplot,
      rowv = hmbis()[[4]],
      ColvOrd = hmbis()[[3]],
      gpcol = hmbis()[[5]],
      gpcolr = hmbis()[[6]],
      distfunTRIX = isolate(hmbis()[[2]]),
      height = hmbis()[[8]], 
      rastering = israstering
    )
    
  }
  
  
  output$warningsheat <- renderPrint({
    validate(need(
      csvf(),
      'You need to import data to visualize to plot the Heatmap' ) %next% 
      need(length(selected_test()) >0, 'You need to select a contrast(s)') %next% 
      need(input$heatm , 'You need to click on the heatmap button down below the heatmap settings')
    )
  })
  
  
  heatid <- input$side
  if (grepl("Heatmap", heatid)) {
    if (input$reactheat == T)
      source(file.path("server", "Plotreact.R"), local = TRUE)$value #
    else
      source(file.path("server", "Plotreact2.R"), local = TRUE)$value #
    
  }
  
  
  output$savehm <- downloadHandler(filename <- function() {
    paste0(basename(file_path_sans_ext(projectname())),
           '_heatmap.',
           input$formhm,
           sep = '')
  },
  content <- function(file) {
    myras = ifelse(input$formhm == "emf", F, T)
    
    if (input$formhm == "emf")
      
      emf(
        file,
        width = 9,
        height = 12,
        pointsize = 12,
        coordDPI = 300
      )
    
    else if (input$formhm == "png")
      png(
        file,
        width = 900,
        height = 1200,
        units = "px",
        pointsize = 12,
        res = 100
      )
    else
      eps(file,
          width = 7,
          height = 9)
    
    if (!is.null(formated()[[1]]))
      withProgress(message = 'Saving heatmap:',
                   value = 0, {
                     n <- NROW(formated()[[1]])
                     for (i in 1:n) {
                       incProgress(1 / n, detail = "Please wait...")
                     }
                     heatmapfinal(isplot = F,israstering =myras)
                   })
    dev.off()
    
  })
  
  
  output$downloadcut <- downloadHandler(
    filename = function() {
      paste(basename(file_path_sans_ext(projectname())),
            '_clustered_hm',
            '.csv',
            sep = '')
    },
    content = function(file) {
      write.csv(ordered(), file, row.names = FALSE)
    }
  )
  
  ordered <- reactive({
    
    req(hmobj$hm)
    
    if (input$method2 == "FDR")
      met = "adj.P.Val_"
    else
      met = "P.value_"
    
    mycont = paste0(met, selected_test())
    ordered = csvf()[[3]] %>% filter(ProbeName %in% hmobj$hm$ProbeName)  %>%
      select(ProbeName,  mycont) %>%
      full_join(hmobj$hm[,-1], ., by = "ProbeName") %>%
      select(ProbeName, GeneName, mycont, cluster) %>%
      mutate_if(is.numeric, funs(format(., digits = 3)))

    rightor = sort(as.integer(rownames(ordered)), decreasing = T)
    ordered = ordered[match(rightor, rownames(ordered)), ]
    
    return(ordered)
  })
 
  grouplength <- reactive({
    req(ordered())
    
    mydfhmgen = (subset( hmobj$hm, !duplicated(subset( hmobj$hm, select=GeneName)))) 
    lengthofmyclust = sapply(1:NROW(unique( hmobj$hm$cluster)),function(x)
      return(length(which(hmobj$hm$cluster ==x)))) %>%  
      cbind(.,sapply(1:NROW(unique( hmobj$hm$cluster)),function(x)
        return(length(which(mydfhmgen$cluster ==x))))) %>% as.data.frame() %>%
      setNames(.,c("total number of probes","total number of genes")) 
    rownames(lengthofmyclust) <- sapply(1:NROW(unique(hmobj$hm$cluster)), function(x)
      return(paste("cluster", x)))
    
    lengthofmyclust <- rbind(lengthofmyclust,c(sum(unlist(lengthofmyclust$`total number of probes`)),sum(unlist(lengthofmyclust$`total number of genes`))))
    rownames(lengthofmyclust)[length(rownames(lengthofmyclust))]<- "total"
    
    return(lengthofmyclust)
    
  })
  
  output$totalgenbyc <- DT::renderDataTable(DT::datatable(grouplength() )) #
  
  
  output$clustering <- DT::renderDataTable(DT::datatable(ordered() ,  options = list(scrollX = TRUE) ))

  
  
})

