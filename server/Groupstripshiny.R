### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


myreorderwk <- reactive({
  req(csvf())
  wkingsetclean <- csvf()[[1]]
  samplesgroup <- factor(csvf()[[2]]$Grp)
  samplesnum <- parse_number(as.character(csvf()[[2]]$X))

if(any(duplicated(samplesnum))){
  samplesnum <- str_extract(csvf()[[2]]$X, "[0-9]$")
  colnames(wkingsetclean)[-1] <- paste(samplesgroup, samplesnum , sep = ".")
}else
  colnames(wkingsetclean)[-1] <- paste(samplesgroup, samplesnum , sep = ".")

  wkingsetclean$GeneName <- csvf()[[3]]$GeneName
  return(wkingsetclean)
})



filenamestrip <- reactive({
  req(csvf(),projectname())

  return( paste0(
    basename(tools::file_path_sans_ext(projectname())),
    '_strip_chart',
    sep = ''
  ))

})



filterwkingset <- reactive({

  req(myreorderwk())
  myreorderwk() %>% select(dataid() ,GeneName, sort(names(.[-1]))) %>% slice(., getDegenes()[[1]]) %>% mutate_if(is.numeric, funs(format(., digits = 3)))

})


selectedrow <- callModule(stylishTables, "orderedwk", data = filterwkingset ,
           scrollX = TRUE,
           pageLength = 150,
           scrollY=550,
           stateSave = T,
           dom = 'Bfrtip',
           server = T ,
           buttons = list(
             list(extend = 'csv',
                  filename =  filenamestrip()[1]),
             list(extend = 'pdf',
                  filename = filenamestrip()[1],
                  title = "My Title",
                  header = FALSE)
           ),
           selection = 'single', case = 2 )


# output$orderedwk <- DT::renderDataTable(DT::datatable(filterwkingset(),
# options = list(scrollX = TRUE,
#                   pageLength = 150,
#                   scrollY=550,
#                   stateSave = T,
#                   dom = 'Bfrtip',
#                 buttons = list(
#                  list(extend = 'csv',
#                       filename =  filenamestrip()[1]),
#                  list(extend = 'pdf',
#                       filename = filenamestrip()[1],
#                       title = "My Title",
#                       header = FALSE)
#                 )),
# selection = 'single',
#  extensions=c("Buttons",'Scroller'),
#  filter =c("none"),
# rownames= FALSE ))



getDegenes <- reactive({

  req(subsetstat(), csvf(), input$pvalstrip)

  indexDEG = decTestTRiX(
    subsetstat()[[1]],
    subsetstat()[[2]],
    subsetstat()[[3]],
    DEGcutoff = input$pvalstrip,
    FC = input$fcstrip,
    cutoff_meth = input$decidemethodstrip, maxDE=NULL )
  return(indexDEG)

})




callstripgenes <- reactive({

  validate(
  need(selectedrow(), 'Search your gene and select the corresponding row'))

  req(getDegenes(), filterwkingset())
  grps <- gsub("[.][0-9]*","",colnames(filterwkingset()[-(1:2)]), perl=T)
  print(grps)
  ggp=ggstrip_groups(grps=grps , wSet= filterwkingset() , probesID= selectedrow() )

})


output$renderstripgenes <- renderPlot({
  req(callstripgenes())
  plotOutput(callstripgenes())
})


callModule(downoutputfiles, "savestrip", projectname = projectname , suffix=paste0( '_', selectedstripgene(), "_strip_chart.", sep='' ), data = callstripgenes , w =16, h = 7  )



selectedstripgene <- reactive({
  req(selectedrow())
  return(filterwkingset()[selectedrow(),"GeneName"] )
})


output$selected_stripgene <- renderText({
  paste("You have selected", selectedstripgene(), "gene.")
})
