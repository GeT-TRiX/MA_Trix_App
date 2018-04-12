# new_group <- reactive( csvf()[[2]] %>%
#                          filter( X ==  list_ind()))


#' Reactive function that return a comparison data frame with the specific user's selection
#'
#' @param csv Data frame corresponding to the Alltoptable
#'
#' @return \new_data a  data frame with all the individuals selected
#'

# data_sign <- reactive({
#   inFile <- input$file
#   if (is.null(inFile))
#     return(NULL)
#   createdfsign(adjusted())
# })


data_sign <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  ptv <- c(.01, .05)
  cbind.data.frame("FDR<1%" = colSums(adjusted()[, -1] < ptv[1]),
                   "FDR<5%" = colSums(adjusted()[, -1] < ptv[2]))
  
})