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

#' col_choice1 is a reactive function that return a character color
#'
#' @param col1 input character color for the highest values
#'
#' @return col_choice1  a reactive value
#'
#' @export
#'

col_choice1 <- reactive({
  return(input$col1)
})

#' col_choice3 is a reactive function that return a character color
#'
#' @param col3 input character color for the lowest values
#'
#' @return  col_choice3 reactive value
#'
#' @export
#'

col_choice3 <- reactive({
  return(input$col3)
})


#' my_intermediate is a reactive function that return a character color
#'
#' @param col_choice1 character color for the lowest values
#' @param col_choice3 character color for the highest values
#'
#' @return inter a reactive character intermediate color between the lowest and the highest values
#'
#' @export
#'


my_intermediate <- reactive({
  
  if (col_choice1() == "green" & col_choice3() == "red")
    inter = "black"
  
  else if (col_choice1() == "orange" & col_choice3() == "red")
    inter = "yellow"
  
  else if (col_choice1() == "blue" & col_choice3() == "red"){#
    user_choice <- eventReactive(input$submit, input$text)
    inter <- user_choice()
  }
  
  else if (col_choice1() == "blue" & col_choice3() == "yellow")
    inter = "black"
  
  else
    inter= NULL
  
  return(inter)
  
})