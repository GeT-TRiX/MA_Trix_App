#' Do some stuff
#' @param a A nice description
#' @param b A great description
#' @return Final stuff
do_stuff <- function(a, b) {
  # Important stuff done
  some_object
}


reactive({
  do_stuff(input$param1, input$param2)
})


#' colspca is a reactive function which aim is to dynamically create widgets in function of the number of groups
#'
#' @param brewer.pal a local list defined in the RcolorBrewer package
#' @param mycolgrppca a dataframe representing the selected groups
#'
#' @return \colspca a reactive number of widget-s
#'
colspca <- function(brewer.pal , mycolgrppca){
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
  
}

