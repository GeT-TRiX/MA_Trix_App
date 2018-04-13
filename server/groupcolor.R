#########################################
######## Colors for the  groups         #
#########################################

mycolgrp <- reactive  ({
  mygrpcol <- new_group()$Grp %>%
    sort() %>%
    unique() %>%
    droplevels()
  
  return(mygrpcol)
})


cols <- reactive({

  if (is.null(mypal())){
    lapply(seq_along(mycolgrp()), function(i) {
      colourInput(
        paste("col", i, sep = "_"),
        levels(mycolgrp())[i],
        palette[i],
        allowedCols =  palette,
        palette = "limited",
        returnName = T)
    })
  }
  else {
  lapply(seq_along(mycolgrp()), function(i) {
    colourInput(
      paste("col", i, sep = "_"),
      levels(mycolgrp())[i],
      mypaletA()[i],
      allowedCols =  palette,
      palette = "limited",
      returnName = T
    )})
  }
})

mypaletA <- reactive  ({
  if (is.null(mypal))
    return(NULL)
  else
    mypal = (colors())
  return(mypal)
})

mypal <- reactive({
  unlist(colors())
})


output$myPanel <- renderUI({
  cols()
})


colors <- reactive({
  lapply(seq_along(mycolgrp()), function(i) {
    input[[paste("col", i, sep = "_")]]
  })
})