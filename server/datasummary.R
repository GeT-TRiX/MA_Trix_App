### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


###############################
######## Summarise data       #
###############################

#' data_summary is a reactive function that return the indexes for the signficant genes
#'
#' @param csvf data frame
#' @param pval1 a numeric input corresponding to the cutoff pvalue
#' @param method a charactger input for the statistical method selected BH or raw
#'
#' @return \datasummary a reactive data frame with the indexes corresponding to the sigificant genes for 5 Fold change 1.2,2,4,6,10
#'
#' @export

data_summary <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  myfinalfc(csvf()[[3]], input$pval1, input$method)
})