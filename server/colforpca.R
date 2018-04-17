colspca <- reactive({
  
  lapply(seq_along(unique(csvf()[[2]]$Grp)), function(i) {
    colourInput(
      paste("col", i, sep = "_"),
      levels(csvf()[[2]]$Grp)[i],
      brewer.pal(8, "Dark2")[i],
      allowedCols =  brewer.pal(8, "Dark2"),
      palette = "limited",
      returnName = T)
  })
})


output$myPanelpca <- renderUI({
  colspca()
})

colorspca <- reactive({
  lapply(seq_along(unique(csvf()[[2]]$Grp)), function(i) {
    input[[paste("col", i, sep = "_")]]
  })
})
