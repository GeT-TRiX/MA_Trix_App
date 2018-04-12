#' Reactive function that return a comparison data frame with the specific user's selection
#'
#' @param csv Data frame corresponding to the Alltoptable
#'
#' @return \new_data a  data frame with all the individuals selected
#'

user_group <- eventReactive(input$heatm, {
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  
  myfinal = list()
  for (i in 1:3)
    myfinal[[i]] = (subset(adjusted()[[i]],
                           select = choix_test()))
  
  return(myfinal)
}, ignoreNULL = F)