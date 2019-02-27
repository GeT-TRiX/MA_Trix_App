#' ### Author: Franck Soubès
#' ### Bioinformatics Master Degree - University of Bordeaux, France
#' ### Link: https://github.com/GeT-TRiX/MA_Trix_App/
#' ### Where: GET-TRiX's facility
#' ### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
#' ### Licence: GPL-3.0
#' 
#' 
#' #############################################################################
#' ######## align group name in the pannel if there is more than 6 groups      #
#' #############################################################################

selected_test <- callModule(boxChooser, "selcomphm", label = "Choose your comparison", data = subsetstat , group = csvf, case = 2 )
subsetgroup_hm <- callModule(boxChooser, "selgrouphm", label = "Choose your group to visualize", data = csvf , group = csvf, case = 1 )


#' subsetwset is a reactive function that aim is to select specific individuals in the data frame
#'
#' @param csvf Data frame corresponding to the Workingset
#' @param subsetgroup_hm a reactive factor with the corresponding groups selected
#'
#' @return subsetwset a reactive data frame with specific columns depending on the user's choices
#'
#' @export


subsetwset <- reactive({
  req(csvf())
  select(csvf()[[1]], as.character(factor(subsetgroup_hm()$X)))
})
