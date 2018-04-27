#' Reactive function that return a list of data frame depending on the comparisons
#'
#' @param adjusted list of three data frame corresponding to the grep of respectively Adj.pval, P.val and logFC columns
#' @param choix_test character corresponding to the defined contrast set by the user
#'
#' @return \myfinal list containing three data frame for each contrast selected
#'

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



#' Reactive function that select specific groups in the data frame
#'
#' @param csvf Data frame of the pData
#' @param choix_grp() character corresponding to the defined groups set by the user
#' 
#' @return \new_group a new factor with the corresponding groups 
#'


new_group <- reactive({  
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(),]
})


#new_group <-reactive(csvf()[[2]][csvf()[[2]]$X %in% choix_individus(),])