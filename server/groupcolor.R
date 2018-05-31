#########################################
######## Colors for the  groups         #
#########################################

#' mycolgrp is a reactive function which aim is to display the number of groups selected 
#'
#' @param new_group a derivate data frame of the pData
#'
#' @return \mycolgrp a reactive data frame 
#'

mycolgrp <- reactive  ({
  mygrpcol <- new_group()$Grp %>%
    sort() %>%
    unique() %>%
    droplevels()
  
  
  return(mygrpcol)
})

#' cols is a reactive function which aim is to dynamically create widgets in function of the number of groups
#'
#' @param palette a local list defined in the environment
#' @param mycolgrp a dataframe representing the selected groups
#' @mypaletA a list which contaings the colors values corresponding to the different groups
#'
#' @return \cols a reactive number of widget-s
#'

cols <- reactive({

  if (is.null(mypal()) )
    lapply(seq_along(mycolgrp()), function(i) {
      colourInput(
        paste("col", i, sep = "_"),
        levels(mycolgrp())[i],
        palette[i],
        allowedCols =  palette,
        palette = "limited",
        returnName = T)
    })
  
  else 
  lapply(seq_along(mycolgrp()), function(i) {
    colourInput(
      paste("col", i, sep = "_"),
      levels(mycolgrp())[i],
      mypaletA()[i],
      allowedCols =  palette,
      palette = "limited",
      returnName = T
    )})
})

#' mypaletA is a reactive function which aim is to set colors if the advanced graphical settings are not displays
#'
#' @param colors a list of input for the different user's choice
#'
#' @return \mypaletA a reactive list of colors attributed by ranking order to the different groups
#'

mypaletA <- reactive  ({
  if (is.null(mypal))
    return(NULL)
  else
    mypal = (colors())
  return(mypal)
})

#' mypal is a reactive function which aim is to unlist the choice of colors
#'
#' @param colors a list of input for the different user's choice
#'
#' @return \mypal a reactive  that unlist the colors attributed to the different groups
#'

mypal <- reactive({
  unlist(colors())
})



output$myPanel <- renderUI({
  fluidRow(
  cols())
})

# output$myPanel1 <- renderUI({ # display the colourInput in the UI
#   cols()[1:2]
# })
# 
# output$myPanel2 <- renderUI({ # display the colourInput in the UI
#   cols()[3:4]
# })

#' colors is a reactive function which aim is to create as many variables as groups
#'
#' @param mycolgrp  a reactive data frame 
#'
#' @return \colors a reactive  list containing the different variable names
#'

colors <- reactive({
  lapply(seq_along(mycolgrp()), function(i) {
    input[[paste("col", i, sep = "_")]]
  })
})