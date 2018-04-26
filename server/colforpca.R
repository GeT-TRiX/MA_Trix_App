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


output$myPanelpca <- renderUI({
  colspca()
})

colorspca <- reactive({
  lapply(seq_along(unique(mycolgrppca())), function(i) {
    input[[paste("colpca", i, sep = "_")]]
  })
})


mycolgrppca <- reactive  ({
  mygrpcol <- csvf()[[2]]$Grp %>%
    sort() %>%
    unique() 
  
  return(mygrpcol)
})