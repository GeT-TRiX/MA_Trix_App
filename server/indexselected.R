### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



#' formated is a reactive function that return the indexes for the signficant genes
#'
#' @param user_group a list of three data frame with rows selected according to the contrasts selected
#' @param intput$fc a numeric FC selected
#' @param input$method2 a character method, default = BH
#' @param input$pval a numeric pvalue
#' @param input$maxgen a numeric maxgen, default = NULL
#'
#' @return formated a reactive data frame with the indexes corresponding to the sigificant genes
#'
#' @export
#' 


formated <- reactive({
  
  req(user_group())

  df <- csvf()
  if (is.null(df))
    return(NULL)
  
  else
    treated = decTestTRiX(
      user_group()[[1]],
      user_group()[[2]],
      user_group()[[3]],
      DEGcutoff = input$pval,
      FC = input$fc,
      cutoff_meth = input$method2,
      maxDE = input$maxgen
      
    )
  
  return(treated)
  
})




