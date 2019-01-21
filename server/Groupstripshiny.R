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
  #samplesnum <- parse_number(as.character(csvf()[[2]]$X))
  colnames(wkingsetclean)[-1] <- paste(samplesgroup, as.character(csvf()[[2]]$X) , sep = ".") 
  wkingsetclean$GeneName <- csvf()[[3]]$GeneName
  return(wkingsetclean)
  
})


# myreorderwk <- reactive({ ## add GeneName
#   
#   TODO return a sorted table with the colnames = Groups & Samples
#   
# })



filenamestrip <- reactive({
  req(csvf(),projectname())
  
  return( paste0(
    basename(tools::file_path_sans_ext(projectname())), # Add cutoff
    '_strip_chart',
    sep = ''
  ))
  
})

# filenamestrip <- reactive({
#
#   TODO return past vector of the project name
#
# })


#final filter based on slice() select rownames

output$orderedwk <- DT::renderDataTable(DT::datatable(myreorderwk() %>% select(ProbeName,GeneName, sort(names(.[-1]))) %>% slice(., getDegenes()[[1]]) %>% mutate_if(is.numeric, funs(format(., digits = 3))), 
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
 extensions=c("Buttons",'Scroller'),
 filter =c("none"),
rownames= FALSE ))


# output$orderedwk <- DT::renderDataTable(DT::datatable(
#
#   TODO render the filtered table 
#
# ))



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

observe({
  req(getDegenes(), csvf())
  print(input$decidemethodstrip)
  print(getDegenes()[[1]])
})


# getDegenes <- reactive({
#   
#   TODO return a list of index with the decTestTRiX function
#   
# })



callstripgenes <- reactive({
  req(getDegenes())
  # ggstrip_groups call 
  
  return(NULL)
  
})

# callstripgenes <- reactive({
# 
#   TODO call the function to compute the plot
#   
# })



output$renderstripgenes <- renderPlot({
  
  return(NULL)
})


# output$renderstripgenes <- renderPlot({
#   
#   TODO render plot strip gene
#
# })


output$savestriplot <- downloadHandler(filename <- function() {
  paste0(
    basename(tools::file_path_sans_ext(projectname())), # add  gene name
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




# output$savestriplot <- downloadHandler(filename <- function() {
#   
#   TODO add donwload button 
#   
# })

