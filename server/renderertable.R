#########################################
######## Plot the data frame wiht input #
#########################################

output$new_test <- renderDataTable(csvf()[[2]]) # Data frame corresponding to the pData

#output$new_data <- renderDataTable(head(csvf()[[1]][2:6])) # Head of the WorkingSet data 

#output$new_group <- renderDataTable(new_group()) # a data frame corresponding to the selected groups

output$data_summary <- renderDataTable(data_summary()) # Summary of the significant genes depending on the pvalue with FC set to (1.2,2,4,6,10)

output$vennresinter <- DT::renderDataTable(DT::datatable(vennfinal(), list(lengthMenu =  c('5', '15', '50'))), server = F)

output$davidgo <- DT::renderDataTable(DT::datatable(davidwebservice()[[as.numeric(input$cutgo)]][, -9] , options = list(scrollX = TRUE) ) )

#output$totalgenbyc <- renderDataTable(grouplength())

#opuput$resumetopgoandkegg <- 

myrenderedtop <- reactive({
  req(csvf())

  select( csvf()[[3]], ProbeName:SystematicName, everything() ) %>%
    mutate_if(is.numeric, funs(format(., digits = 3)))


})

output$new_group <- DT::renderDataTable(DT::datatable(myrenderedtop()[,-c(4:9)] , options = list(scrollX = TRUE) ) )

