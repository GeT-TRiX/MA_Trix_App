### Summarise data
###

data_summary <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  myfinalfc(csvf()[[3]], input$pval1)
})