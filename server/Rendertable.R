### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


##########################################
######## Plot the data frame wiht input ##
##########################################


callModule(stylishTables, "renderpdata", data = reactive(csvf()[[2]]) , lengthpage=  c('5', '10', '15','50','100'), pageLength=10 , searching =F)
callModule(stylishTables, "rendersummary", data = data_summary , lengthpage=  c('5', '10', '15','20'), pageLength=15 )


observe({
  
  req(input$dispvenn, vennfinal())

  if(any(grepl("probes|transcripts", input$dispvenn)) &&  (is.null(input$filteredcompjv) || input$filteredcompjv == "" ))
    #callModule(stylishTables, "renderjvenntab", data = reactive(vennfinal()[[1]]) ,pageLength=150,extensions=c("Buttons",'Scroller'), scrollX= T,  scrollY=530,dom = 'Bfrtip',  buttons = c( 'csv',  'pdf'),  stateSave = T)
    output$vennresinter <- DT::renderDataTable(DT::datatable(vennfinal()[[1]], list(lengthMenu =  c('5', '10', '15')),extensions=c("Buttons",'Scroller'),  options = list(scrollX = TRUE,  pageLength = 150, scrollY=530,  stateSave = T,  dom = 'Bfrtip',
                                                                                                                                      buttons = c( 'csv',  'pdf' )) ), server = F)
  else if (input$dispvenn == "genes"  &&  (is.null(input$filteredcompjv) || input$filteredcompjv == "" ))
    #callModule(stylishTables, "renderjvenntab", data = reactive(vennfinal()[[2]]) ,pageLength=150,extensions=c("Buttons",'Scroller'), scrollX= T,  scrollY=530,dom = 'Bfrtip',  buttons = c( 'csv',  'pdf'), dupgenes= reactive(jvenndup$duplicated$GeneName), case =1 , stateSave = T)
    output$vennresinter <- DT::renderDataTable(DT::datatable(vennfinal()[[2]], list(lengthMenu =  c('5', '10', '15')),extensions=c("Buttons",'Scroller'), options = list(scrollX = TRUE ,pageLength = 150, scrollY=530,  stateSave = T,dom = 'Bfrtip',
                                                                                                                                      buttons = c( 'csv',  'pdf' ))) %>% formatStyle('GeneName', color = styleEqual(unique(jvenndup$duplicated$GeneName), rep('orange', length(unique(jvenndup$duplicated$GeneName))))), server = F)
  else
    #callModule(stylishTables, "renderjvenntab", data = topngenesDT , pageLength=150,extensions=c("Buttons",'Scroller'), scrollX= T,  scrollY=530, dom = 'Bfrtip',  buttons = c( 'csv',  'pdf'  ), stateSave = T)
    output$vennresinter <- DT::renderDataTable(DT::datatable(topngenesDT(), list(lengthMenu =  c('5', '10', '15')),extensions=c("Buttons",'Scroller'),  options = list(scrollX = TRUE ,pageLength = 150, scrollY=530,  stateSave = T,dom = 'Bfrtip',
                                                                                                                                   buttons = c( 'csv',  'pdf' ))), server = F)

})

rounddavidtable <- reactive({
  req(davidwebservice())
  return(lapply(1:NROW(davidwebservice()$mygodavid), function(x)
  return(format(davidwebservice()$mygodavid[[x]], digits = 3))))
})

#callModule(stylishTables, "davidgo", data = reactive(rounddavidtable()[[as.numeric(input$cutgo)]][, -9]) , scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis'), extensions = 'Buttons')

output$davidgo <- DT::renderDataTable(DT::datatable(rounddavidtable()[[as.numeric(input$cutgo)]][, -9] , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons'),server=F)

#' myrenderedtop is a reactive function which aim is to display or not the labels in the PCA render plot
#'
#' @param csvf a data frame
#'
#' @return  a reactive data frame
#'
#' @export
#' 


myrenderedtop <- reactive({
  req(csvf())
  csvf()[[3]] %>%
    select_if(.,grepl("^Probe|^Tran|^Gene|^logFC|^log2FoldChange|^P.value|^pvalue|^PValue|^adj.P|^padj|^FDR", colnames(.))) %>%
    mutate_if(is.numeric, funs(format(., digits = 3)))
})


callModule(stylishTables, "renderestab", data = myrenderedtop , lengthpage=  c('5', '10', '15','20'), pageLength=15, scrollX = T, dom = 'Bfrtip', filter = c("none"), buttons = I('colvis'), extensions = 'Buttons', ordering= F, server = T )

callModule(stylishTables, "renderestab", data = myrenderedtop , lengthpage=  c('5', '10', '15','20'), pageLength=15, scrollX = T, dom = 'Bfrtip', filter = c("none"), buttons = I('colvis'), extensions = 'Buttons', ordering= F, server = T )

callModule(stylishTables, "clustdavid", data = reactive({summary(Venncluster()$mygodavid) %>% as.data.frame() %>% mutate_if(is.numeric, funs(format(., digits = 3)))}) , lengthpage=  c('5', '10', '15','20','30'), pageLength=10 )


