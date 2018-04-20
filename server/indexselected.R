#' Reactive function that return a data frame with significant genes for a defined p-value
#'
#' @param csv  Data frame corresponding to the WorkingSet
#'
#' @return \treated a data frame with the id for significant genes
#'


formated <- reactive({
  
  #treated = formating(new_test(), csvf()[[1]], input$pval)
  treated = decTestTRiX(
    user_group()[[1]],
    user_group()[[2]],
    user_group()[[3]],
    DEGcutoff = input$pval,
    #DEGcutoff = mypval(),
    FC = input$fc,
    cutoff_meth = input$method2
    
    ,maxDE = input$maxgen
    
  )
  return(treated)
})