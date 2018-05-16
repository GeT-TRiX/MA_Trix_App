#' heatmapfinal is an isolate function that only react to a user's click on the heatmap button 
#' 
#' @param heatmapobj[[1]] a data frame with all the individuals selected
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
#'
#' @return  a data frame with the cluster and the corresponding genes 
#' 
#' @export
#' 


heatmapfinal <- function(isplot  = T) {
  plotHeatmaps(
    hmbis()[[1]],
    formated(),
    droplevels(new_group()$Grp),
    workingPath = wd_path,
    my_palette = colorRampPalette(c(
      choix_col1(), my_intermediate(), choix_col3()
    ))(n = 75),
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
    distfunTRIX = hmbis()[[2]]
  )
}

#' hmbis is an event reactive function that pre-computed hierarchical clustering on microarray data 
#'
#' @param new_data a data frame with all the individuals selected
#' @param formated  a data frame with the indexes corresponding to the sigificant genes
#' @param new_group  a data frame with the corresponding groups 
#' @param workingPath the current user's repository 
#' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
#' @param Rowdistfun a function used to compute the distance for the rows
#' @param Coldistfun a function used to compute the distance for the columns
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#'
#' @return  a list of objects which aim is to being passed as argument in the plotHeatmaps function
#' 
#' @export
#' 


hmbis <- reactive( {
  #hmbis <- eventReactive(input$heatm, {
  
  isolate( # isolate in order to avoid that reactive values update the heatmap 
    truncatedhat(
      data.matrix(new_data()),
      formated(),
      droplevels(new_group()$Grp),
      workingPath = wd_path,
      k = input$clusters,
      mypal = unlist(colors()),
      Rowdistfun = input$dist ,
      Coldistfun = input$dist,
      meanGrp = input$meangrp
    )
  )
})

#heatmapobj <<- hmbis() # a static variable 



output$distPlot <- renderPlot({
  isolate({
    if (!is.null(formated()))
      withProgress(message = 'Plotting heatmap:', # Add sliderbar when loading heatmap
                   value = 0,
                   {
                     n <- NROW(formated()) #number of row in the formated dataframe
                     
                     for (i in 1:n) {
                       incProgress(1 / n, detail = "Please wait...")
                     }
                     hmbis()
                     heatmapfinal(isplot = F)
                   })
  })
}, width = 900 , height = 1200, res = 100)


output$save <- downloadHandler(filename <- function() {
  paste0(basename(file_path_sans_ext("myfile")),
         '_heatmap.',
         input$form,
         sep = '')
},
content <- function(file) {
  if (input$form == "emf")
    
    emf(
      file,
      width = 7,
      height = 7,
      pointsize = 12,
      coordDPI = 300
    )
  
  else if (input$form == "png")
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
        height = 7)
  
  if (!is.null(formated()))
    withProgress(message = 'Saving heatmap:',
                 value = 0,
                 {
                   n <- NROW(formated())
                   
                   for (i in 1:n) {
                     incProgress(1 / n, detail = "Please wait...")
                   }
                   
                   heatmapfinal(isplot = F)
                   
                 })
  
  dev.off()
  
  
})




