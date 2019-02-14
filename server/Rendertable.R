### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


##########################################
######## Plot the data frame wiht input ##
##########################################

output$designtab <- renderDataTable(csvf()[[2]]) # Data frame corresponding to the pData


output$data_summary <- renderDataTable(data_summary()) # Summary of the significant genes depending on the pvalue with FC set to (1.2,2,4,6,10)


observe({

  req(input$dispvenn, vennfinal())

  if(any(grepl("probes|transcripts", input$dispvenn)) &&  (is.null(input$filteredcompjv) || input$filteredcompjv == "" ))
    output$vennresinter <- DT::renderDataTable(DT::datatable(vennfinal()[[1]], list(lengthMenu =  c('5', '10', '15')),extensions=c("Buttons",'Scroller'),  options = list(scrollX = TRUE,  pageLength = 150, scrollY=530,  stateSave = T,  dom = 'Bfrtip',
                                                                                                                                      buttons = c( 'csv',  'pdf' )) ), server = F)
  else if (input$dispvenn == "genes"  &&  (is.null(input$filteredcompjv) || input$filteredcompjv == "" ))
    output$vennresinter <- DT::renderDataTable(DT::datatable(vennfinal()[[2]], list(lengthMenu =  c('5', '10', '15')),extensions=c("Buttons",'Scroller'), options = list(scrollX = TRUE ,pageLength = 150, scrollY=530,  stateSave = T,dom = 'Bfrtip',
                                                                                                                                      buttons = c( 'csv',  'pdf' ))) %>% formatStyle('GeneName', color = styleEqual(unique(jvenndup$duplicated$GeneName), rep('orange', length(unique(jvenndup$duplicated$GeneName))))), server = F)
  else
    output$vennresinter <- DT::renderDataTable(DT::datatable(topngenesDT(), list(lengthMenu =  c('5', '10', '15')),extensions=c("Buttons",'Scroller'),  options = list(scrollX = TRUE ,pageLength = 150, scrollY=530,  stateSave = T,dom = 'Bfrtip',
                                                                                                                                   buttons = c( 'csv',  'pdf' ))), server = F)

})

rounddavidtable <- reactive({
  req(davidwebservice)
  return(lapply(1:NROW(davidwebservice()), function(x)
  return(format(davidwebservice()[[x]], digits = 3))))
})

output$davidgo <- DT::renderDataTable(DT::datatable(rounddavidtable()[[as.numeric(input$cutgo)]][, -9] , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons'),server=F)

#' myrenderedtop is a reactive function which aim is to display or not the labels in the PCA render plot
#'
#' @param csvf a data frame
#'
#' @return  a reactive data frame
#'
#' @export


myrenderedtop <- reactive({
  req(csvf())
  csvf()[[3]] %>%
    select_if(.,grepl("^Probe|^Tran|^Gene|^logFC|^log2FoldChange|^P.value|^pvalue|^PValue|^adj.P|^padj|^FDR", colnames(.))) %>%
    mutate_if(is.numeric, funs(format(., digits = 3)))
})

output$subsetgroup_hm <- DT::renderDataTable(DT::datatable(myrenderedtop() , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons',filter =c("none")))


observe({
  req(!is.null(length(myresdavitab())))
output$cat_MF <- DT::renderDataTable({
  if(is.null(myresdavitab()[[1]]))
    return(NULL)
  else DT::datatable(myresdavitab()[[1]] , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons') })
})

observe({
  req(length(myresdavitab())>1)
output$cat_BP <- DT::renderDataTable({
DT::datatable(myresdavitab()[[2]] ,options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons') })})

observe({
req(length(myresdavitab())>2)

output$cat_CC <- DT::renderDataTable({
 DT::datatable(myresdavitab()[[3]] , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons' ) })
})


output$debug <- DT::renderDataTable({
  req(Venncluster())
  summary(Venncluster()) %>% as.data.frame() %>% mutate_if(is.numeric, funs(format(., digits = 3)))
})

observe({
  req(length(myresdavitab())>3)

  output$cat_KEGG <- DT::renderDataTable({
  DT::datatable(myresdavitab()[[4]] , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons') })
})
