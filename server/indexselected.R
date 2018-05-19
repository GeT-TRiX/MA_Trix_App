#' a reactive function that return the indexes for the signficant genes
#'
#' @param user_group list of three data frame with rows selected according to the contrasts selected
#' @param intput$fc numeric FC selected
#' @param input$method2 character method, default = BH
#' @param input$pval numeric pvalue
#' @param input$maxgen numeric maxgen, default = NULL
#'
#' @return \formated a reactive data frame with the indexes corresponding to the sigificant genes
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


