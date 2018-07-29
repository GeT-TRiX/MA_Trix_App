##########################################
######## Plot the data frame wiht input ##
##########################################

output$new_test <- renderDataTable(csvf()[[2]]) # Data frame corresponding to the pData

#output$new_data <- renderDataTable(head(csvf()[[1]][2:6])) # Head of the WorkingSet data 

#output$new_group <- renderDataTable(new_group()) # a data frame corresponding to the selected groups

output$data_summary <- renderDataTable(data_summary()) # Summary of the significant genes depending on the pvalue with FC set to (1.2,2,4,6,10)


observe({
  
  req(input$dispvenn)
  if(input$dispvenn == "probes")
    output$vennresinter <- DT::renderDataTable(DT::datatable(vennfinal()[[1]], list(lengthMenu =  c('15', '30', '50','100')), options = list(scrollX = TRUE,  pageLength = 15)), server = F)
  else
    output$vennresinter <- DT::renderDataTable(DT::datatable(vennfinal()[[2]], list(lengthMenu =  c('15', '30', '50','100')), options = list(scrollX = TRUE ,pageLength = 15)), server = F)
})
    
observe({
  if(input$dispvenn == "genes")
    output$vennresintergen <- DT::renderDataTable(DT::datatable(vennfinal()[[1]], list(lengthMenu =  c('15', '30', '50','100')),options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis'), pageLength = 15), extensions = 'Buttons'), server = F)
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
  select( csvf()[[3]], ProbeName:SystematicName, everything() ) %>%
    #mutate_if(is.numeric, funs(format(., digits = 3)))
    mutate_if(is.numeric, funs(round(., digits = 3)))
})

output$new_group <- DT::renderDataTable(DT::datatable(myrenderedtop()[,-c(4:9)] , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons',filter =c("none")) )


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


observe({
  req(length(myresdavitab())>3)

  output$cat_KEGG <- DT::renderDataTable({
  DT::datatable(myresdavitab()[[4]] , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons') })
})
