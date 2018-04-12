#########################################
######## Updating a colourInput         #
#########################################


colourpicker::updateColourInput(
  session,
  "col1",
  label = "downregulated genes:",
  value = firstcol,
  showColour = NULL,
  allowTransparent = FALSE,
  allowedCols = c("green", "orange", "blue"),
  returnName = T
)


colourpicker::updateColourInput(
  session,
  "col3",
  label = "upregulated genes:",
  value = lastcol ,
  showColour = NULL,
  allowTransparent = FALSE,
  allowedCols = c("red", "yellow"),
  returnName = T
)



choix_col1 <- reactive({
  return(input$col1)
})

choix_col3 <- reactive({
  return(input$col3)
})


my_intermediate <- reactive({
  if (choix_col1() == "green" & choix_col3() == "red")
    inter = "black"
  
  else if (choix_col1() == "orange" & choix_col3() == "red")
    inter = "yellow"
  
  else if (choix_col1() == "blue" & choix_col3() == "red")
    inter = "white"
  
  else if (choix_col1() == "blue" & choix_col3() == "yellow")
    inter = "green"
  
  
  return(inter)
  
})