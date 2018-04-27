#########################################
######## Plot the data frame wiht input #
#########################################

output$new_test <- renderDataTable(csvf()[[2]]) # Data frame corresponding to the pData

output$new_data <- renderDataTable(head(csvf()[[1]][2:6])) # Head of the WorkingSet data 

output$new_group <- renderDataTable(new_group()) # a data frame corresponding to the selected groups

output$data_summary <- renderDataTable(data_summary()) # Summary of the significant genes depending on the pvalue with FC set to (1.2,2,4,6,10)