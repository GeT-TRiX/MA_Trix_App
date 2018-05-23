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

#' Reactive function that return a character color
#'
#' @param choix_col1 character color for the lowest values
#'
#' @return \choix_col1 a reactive value
#'

choix_col1 <- reactive({
  return(input$col1)
})

#' Reactive function that return a character color
#'
#' @param choix_col3 character color for the lowest values
#'
#' @return \choix_col3 a reactive value   
#' 

choix_col3 <- reactive({
  return(input$col3)
})


#' Reactive function that return a character color
#'
#' @param choix_col1 character color for the lowest values 
#' @param choix_col2 character color for the highest values
#'
#' @return \my_intermediata a reactive character intermediate color between the lowest and the highest values
#'


my_intermediate <- reactive({
  
  if (choix_col1() == "green" & choix_col3() == "red")
    inter = "black"
  
  else if (choix_col1() == "orange" & choix_col3() == "red")
    inter = "yellow"
  
  else if (choix_col1() == "blue" & choix_col3() == "red")
    inter = "white"
  
  else if (choix_col1() == "blue" & choix_col3() == "yellow")
    inter = "black"
  
  else
    inter= NULL
  
  return(inter)
  
})