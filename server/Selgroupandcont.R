### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



# user_group is a reactive function that summarise the significant genes depending on the pvalue with FC set to (1.2,2,4,6,10)#' Reactive function that return a list of data frame depending on the comparisons
#'
#' @param adjusted list of three data frame corresponding to the grep of respectively Adj.pval, P.val and logFC columns
#' @param selected_test character corresponding to the defined contrast set by the user
#'
#' @return usergroup a reactive list containing three data frame for each contrast selected
#'
#' @export

user_group <- reactive({ 
  
  req(selected_test(),adjusted())
  
  
  myfinal = list()
  for (i in 1:3)
    myfinal[[i]] = (subset(adjusted()[[i]],
                           select = selected_test()))
  
  return(myfinal)
})

