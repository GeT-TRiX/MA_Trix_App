### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0

#' myreorderwk is a reactive function that sort the workingset by names
#'
#' @param csvf a list of reactive dataframes
#'
#' @return A reactive sorted dataframe
#'
#' @export


myreorderwk <- reactive({
  req(csvf())
  wkingsetclean <- csvf()[[1]]
  samplesgroup <- factor(csvf()[[2]]$Grp)
#  samplesnum <- parse_number(as.character(csvf()[[2]]$X))
  samplesnum <- as.character(csvf()[[2]]$X)
message("samplesnum: ",samplesnum)
#if(any(duplicated(samplesnum)))	samplesnum <- as.character(csvf()[[2]]$X)

 colnames(wkingsetclean)[-1] <- paste(samplesgroup, samplesnum , sep = ".")

  wkingsetclean$GeneName <- csvf()[[3]]$GeneName
  return(wkingsetclean)
})

#' filenamestrip is a reactive function that return a character
#'
#' @param projectname a character vector (project MA0xxx or date)
#'
#' @return A reactive character
#'
#' @export

filenamestrip <- reactive({
  req(projectname())

  return( paste0(
    basename(tools::file_path_sans_ext(projectname())),
    '_strip_chart',
    sep = ''
  ))

})

#' filterwkingset is a reactive function that return a character
#'
#' @param projectname a character vector (project MA0xxx or date)
#'
#' @return A reactive character
#'
#' @export

filterwkingset <- reactive({
  req(myreorderwk())
  myreorderwk() %>% select(dataid() ,GeneName, sort(names(.[-1]))) %>% slice(., getDegenestrip()[[1]]) %>% mutate_if(is.numeric, funs(format(., digits = 3)))

})

selectedrow <- callModule(stylishTables, "orderedwk", data = filterwkingset , 
	lengthpage=  c('5', '10', '15'), pageLength=10, scrollX = T, scrollY=T, dom = 'Bfrtip', filter = c("none"), extensions = 'Buttons', ordering= TRUE, server = TRUE, 
	list(
             list(extend = 'csv',
                  filename =  filenamestrip()[1]),
             list(extend = 'pdf',
                  filename = filenamestrip()[1],
                  title = "My Title",
                  header = FALSE)
           ),
	selection = 'single', case = 2 )

##selectedrow <- callModule(stylishTables, "orderedwk", data = filterwkingset ,
##           scrollX = TRUE,
##           pageLength = 150,
##           scrollY=550,
##           stateSave = T,
##           dom = 'Bfrtip',
##           server = F ,
##           buttons = list(
##             list(extend = 'csv',
##                  filename =  filenamestrip()[1]),
##             list(extend = 'pdf',
##                  filename = filenamestrip()[1],
##                  title = "My Title",
##                  header = FALSE)
##           ),
##           selection = 'single', case = 2 )



getDegenestrip <- callModule(getDegenes, "degstrip", data = subsetstat , meth = reactive(input$decidemethodstrip), case = 1 , maxDe = reactive(NULL) )

#' callstripgenes is a reactive function that plot distribution of a specific gene for all samples
#'
#' @param filterwkingset A reactive character 
#' @param selectedrow A character vector
#'
#' @return a ggplot object
#'
#' @export

callstripgenes <- reactive({

  validate(need(selectedrow(), 'Search your gene and select the corresponding row'))

  req(filterwkingset())
  message("colnames(filterwkingset()[-(1:2)]): ",colnames(filterwkingset()[-(1:2)]))
#  grps <- gsub("[.][0-9]*","",colnames(filterwkingset()[-(1:2)]), perl=T)
  grps <- gsub("[.].*","",colnames(filterwkingset()[-(1:2)]))
  message("grps: ",grps)
  ggp=ggstrip_groups(grps=grps , wSet= filterwkingset() , probesID= selectedrow() )

})


output$renderstripgenes <- renderPlot({
  req(callstripgenes())
  plotOutput(callstripgenes())
})


callModule(downoutputfiles, "savestrip", projectname = projectname , suffix=paste0( '_', selectedstripgene(), "_strip_chart.", sep='' ), data = callstripgenes , w =16, h = 7  )

#' selectedstripgene is a reactive function that return the genename selected
#'
#' @param filterwkingset A reactive character 
#' @param selectedrow A character vector
#'
#' @return A reactive character value
#'
#' @export

selectedstripgene <- reactive({
  req(selectedrow())
  return(filterwkingset()[selectedrow(),"GeneName"] )
})


output$selected_stripgene <- renderText({
  paste("You have selected", selectedstripgene(), "gene.")
})
