###############################
########heatmap function & co #
###############################


#' heatmapfinal is an isolate function that only react to a user's click on the heatmap button 
#' 
#'@param new_data a data frame with all the individuals selected
#'@param formated  a data frame with the indexes corresponding to the sigificant genes
#'@param new_group  a data frame with the corresponding groups 
#'@param workingPath the current user's repository 
#'@param my_palette a vector of colors 
#'@param k a numeric value which aim is to defined the number of clusters wanted for the rows defined by the user input input$clusters
#'@param Rowdistfun a character value set by the user to defined the method to calculate the dendogram matrix distance for the genes input$dist
#'@param Coldistfun a character value set by the user to defined the method to calculate the dendogram matrix distance for the contrasts input$dist
#'@param keysize 
#'@param mycex a numeric value which aim is to change the size of the legend in the heatmap defined by the user input$legsize
#'@param cexrow 
#'@param cexcol 
#'@param meanGrp a boolean value to compute or not the mean of each contrasts in the heatmap input$meangrp
#'@param labColu a numeric value to change the size of the police legend for the columns input$colsize
#'@param labRowu a numeric value to change the size of the police legend for the rows input$rowsize
#'@param mypal a list of values 
#'@param showcol a boolean value used to hide or show the colnames input$colname
#'@param showrow a boolean value used to hide or show the rownames input$rowname
#'@param genename a data frame list corresponding to the gene names 
#'
#' @return \heatmapfinal an heatmap object 
#' 


heatmapfinal <- function() {
  
  isolate({
    plotHeatmaps(
      data.matrix(new_data()),
      formated(),
      droplevels(new_group()$Grp),
      workingPath = wd_path,
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
      showcol = colname(),
      showrow = rowname(),
      genename = csvf()[[3]]$GeneName
    )
    
  })
}

#' rowname is a reactive function which aim is to hide or show the rownames
#'
#' @param input$rowname  a boolean radio button input
#'
#' @return \rowname a reactive boolean value
#'

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
#' @return \colname a reactive  reactive boolean value
#'

colname <- reactive({
  colname <- switch(input$colname,
                    hide = T,
                    show = F,
                    F)
  return(colname)
})