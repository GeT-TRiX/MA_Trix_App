###############################
######## Summarise data       #
###############################

#' reactive function that return the indexes for the signficant genes
#'
#' @param csvf data frame
#' @param intput$fc numeric pvalue

#' @return \datasummary a reactive data frame with the indexes corresponding to the sigificant genes for 5 Fold change 1.2,2,4,6,10
#'

data_summary <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  myfinalfc(csvf()[[3]], input$pval1, input$method)
})