#########################################
######## Plot the data frame wiht input #
#########################################

output$new_test <- renderDataTable(csvf()[[2]])

output$new_data <- renderDataTable(head(csvf()[[1]][2:6]))

output$new_group <- renderDataTable(new_group())

#output$data_sign <- renderDataTable(data_sign())

output$data_summary <- renderDataTable(data_summary())