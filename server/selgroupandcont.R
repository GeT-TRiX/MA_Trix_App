#' Reactive function that return a comparison data frame with the specific user's selection
#'
#' @param csv Data frame corresponding to the Alltoptable
#'
#' @return \new_data a  data frame with all the individuals selected
#'

#user_group <- eventReactive(input$heatm, {
user_group <- reactive({ 
 inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  
  myfinal = list()
  for (i in 1:3)
    myfinal[[i]] = (subset(adjusted()[[i]],
                           select = choix_test()))
  
  return(myfinal)
})
#, ignoreNULL = F)


#' Reactive function that select specific groups in the data frame
#'
#' @param csv Data frame corresponding to the pData table
#'
#' @return \new_group a new factor with the corresponding individuals from the checkbox with the good levels
#'


#new_group <-reactive(csvf()[[2]][csvf()[[2]]$X %in% choix_individus(),])


#new_group <- eventReactive(input$heatm, {
new_group <- reactive({  
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(),]
#}
#, ignoreNULL = F)
})