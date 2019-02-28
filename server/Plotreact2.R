### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


###############################
######## not Reactive side    #
###############################


shinyjs::enable("heatm")



#' hmbis is an event reactive function that pre-computed hierarchical clustering on microarray data
#'
#' @param subsetwset a data frame with all the individuals selected
#' @param subsetDEG  a data frame with the indexes corresponding to the sigificant genes
#' @param subsetgroup_hm  a data frame with the corresponding groups
#' @param workingPath the current user's repository
#' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
#' @param Rowdistfun a function used to compute the distance for the rows
#' @param Coldistfun a function used to compute the distance for the columns
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#' @param genename a data frame
#'
#' @return  a list of objects which aim is to being passed as argument in the plotHeatmaps function
#'
#' @export
#'

hmbis <- reactive({
  withProgress(message = 'Performing the hierarchical clustering, be patient!',{
                 for (i in 1:15) {
                   incProgress(1 / 15, detail = "Please wait...")
                   } 
                  truncatedhat(
                   data.matrix(subsetwset()),
                   subsetDEG()[[1]],
                   droplevels(subsetgroup_hm()$Grp),
                   workingPath = wd_path,
                   k = input$clusters,
                   mypal = unlist(colors()),
                   Rowdistfun = input$dist ,
                   Coldistfun = input$dist,
                   meanGrp = input$meangrp,
                   genename =  csvf()[[3]],
                   algo = input$algomet
                 )
                 
               })
})


observeEvent(input$heatm, {
  
  
  if (is.null(my_intermediate())) {
    pdf(NULL) 
    heatmapfinal(isplot = F)
    shinyjs::alert("The colors defined for the heatmap are not fit to be together!!")
    return(NULL)
    
  }
  
  else
    output$distPlot <- renderPlot({
      isolate({  

        hmbis()
        hmsize$cut <- hmbis()[[8]]
        hmobj$obj$rows <- hmbis()[[6]]
        hmobj$obj$cols <-  hmbis()[[5]]
        hmobj$obj$colgroup <- unlist(colors())
        hmobj$obj$groups <-  droplevels(subsetgroup_hm()$Grp)
        hmobj$obj$rownames <- hmbis()[[7]]
        
        observe({
          boolhm <<- T
        })
        
        output$heatmbool <- reactive({
          boolhm
        })
        
        withProgress(message = 'Plotting heatmap:',  {
                       for (i in 1:15) {
                         incProgress(1 / 15, detail = "Please wait...")
                       }
                       hmboth$tot <- heatmapfinal(isplot = F)
                       hmobj$hm <- hmboth$tot[[1]]
                       hmobj$obj$hm <- hmboth$tot[[2]]
                       
                     })
      })
      
    })
})
  


