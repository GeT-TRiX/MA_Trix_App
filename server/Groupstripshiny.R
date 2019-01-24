#' ### Author: Franck Soubès
#' ### Bioinformatics Master Degree - University of Bordeaux, France
#' ### Link: https://github.com/fsoubes/MA_Trix_App
#' ### Where: GET-TRiX's facility
#' ### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
#' ### Licence: GPL-3.0


myreorderwk <- reactive({ ## add GeneName
  
  req(csvf())
  wkingsetclean <- csvf()[[1]]
  samplesgroup <- factor(csvf()[[2]]$Grp)
  samplesnum <- parse_number(as.character(csvf()[[2]]$X))
  colnames(wkingsetclean)[-1] <- paste(samplesgroup, samplesnum , sep = ".") 
  wkingsetclean$GeneName <- csvf()[[3]]$GeneName
  return(wkingsetclean)
  
})



filenamestrip <- reactive({
  req(csvf(),projectname())
  
  return( paste0(
    basename(tools::file_path_sans_ext(projectname())), # Add cutoff
    '_strip_chart',
    sep = ''
  ))
  
})



filterwkingset <- reactive({   
  
  req(myreorderwk())
  myreorderwk() %>% select(ProbeName,GeneName, sort(names(.[-1]))) %>% slice(., getDegenes()[[1]]) %>% mutate_if(is.numeric, funs(format(., digits = 3)))
  
})




output$orderedwk <- DT::renderDataTable(DT::datatable(filterwkingset(), 
options = list(scrollX = TRUE, 
                  pageLength = 150, 
                  scrollY=550,  
                  stateSave = T,  
                  dom = 'Bfrtip',
                buttons = list(
                 list(extend = 'csv',
                      filename =  filenamestrip()[1]),
                 list(extend = 'pdf',
                      filename = filenamestrip()[1],
                      title = "My Title",
                      header = FALSE)
                )),
selection = 'single', 
 extensions=c("Buttons",'Scroller'),
 filter =c("none"),
rownames= FALSE ))





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
    need(input$orderedwk_row_last_clicked, 'Search your gene and select the corresponding row'))
  
  req(getDegenes(), filterwkingset(), req(input$orderedwk_row_last_clicked))
  grps <- gsub("[.][0-9]*","",colnames(filterwkingset()[-(1:2)]), perl=T)
  ggp=ggstrip_groups(grps=grps , wSet= filterwkingset() , probesID= input$orderedwk_row_last_clicked)
  
})


output$renderstripgenes <- renderPlot({
  req(callstripgenes())
  plotOutput(callstripgenes())
})


output$savestriplot <- downloadHandler(filename <- function() {
  paste0(
    basename(tools::file_path_sans_ext(projectname())), # add  gene name
    '_',
    selectedstripgene(), 
    '_strip_chart.',
    input$formstrip,
    sep = ''
  )
},
content <- function(file) {
  if (input$formstrip == "pdf")
    
    pdf(file,
        width = 16,
        height = 7,
        pointsize = 12)
  
  else if (input$formstrip == "png")
    png(
      file,
      width = 1600,
      height = 700,
      units = "px",
      pointsize = 12,
      res = 100
    )
  else
    eps(file,
        width = 16,
        height = 7,
        pointsize = 12)
  
  print(callstripgenes())
  
  dev.off()
})


selectedstripgene <- reactive({
  req(input$orderedwk_row_last_clicked)
  
  return(filterwkingset()[input$orderedwk_row_last_clicked,"GeneName"] )
  
})


output$selected_stripgene <- renderText({ 
  paste("You have selected", selectedstripgene(), "gene.")
})



