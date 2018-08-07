### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



#########################################
######## Colors for the  PCA groups     #
#########################################

#' colspca is a reactive function which aim is to dynamically create widgets depending on the number of groups
#'
#' @param brewer.pal a local list defined in the RcolorBrewer package
#' @param mycolgrppca a dataframe representing the selected groups
#'
#' @return colspca a reactive number of widget-s
#'
#' @export
#' 

colspca <- reactive({
    
    pcapal = brewer.pal(8,"Dark2") %>%
    list(brewer.pal(10,"Paired")) %>%
    unlist()
  
  lapply(seq_along(unique(mycolgrppca())), function(x) {
    colourInput(
      paste("colpca", x, sep = "_"),
      levels(mycolgrppca())[x],
      pcapal[x],
      allowedCols =  pcapal,
      palette = "limited",
      returnName = T)
  })
})



#' colorfluidpca is a reactive function wich aim is to group colors side by side
#' depending of the number of groups odd or even for  the gui.
#' 
#'
#' @param colspca a reactive number of widget-s
#'
#' @return html code interpreted by shiny
#' 
#' @export
#'


colorfluidpca <- reactive({
  
  lapply(1:length(colspca()), function(i){
    
    j = length(colspca())
    if(length(colspca()) %%2==0){
      if (i %% 2 == 0) {
        fluidRow(column(6, colspca()[[i - 1]]), column(6, colspca()[[i]]))
      }
    }
    else{
      if (i %% 2 ==0 && j!=i) {
        fluidRow(column(6, colspca()[[i - 1]]), column(6, colspca()[[i]]))
      }
      else if (j == i){
        fluidRow(column(6, colspca()[[i]]))
      }
    }
    
  })
  
})



output$myPanelpca <- renderUI({ # display the colourInput in the UI
  colorfluidpca()
})



#' colorspca is a reactive function which aim is to create as many variables as groups
#'
#' @param mycolgrppca  a reactive data frame
#'
#' @return colorspca a reactive  list containing the different variable names
#'
#' @export
#' 


colorspca <- reactive({
  lapply(seq_along(unique(mycolgrppca())), function(i) {
    input[[paste("colpca", i, sep = "_")]]
  })
})

#' mycolgrppca is a reactive function which aim is to display the total number of groups
#'
#' @param csvf a dataframe
#' @param new_grouppca a reactive factor with the corresponding groups selected
#'
#' @return mycolgrppca a reactive reorder dataframe
#'
#' @export

mycolgrppca <- reactive  ({
  req(csvf())
  mygrpcol <- new_grouppca()$Grp %>%
    sort() %>%
    unique() %>%
    droplevels()
  
  return(mygrpcol)
})
