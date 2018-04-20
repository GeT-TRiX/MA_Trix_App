output$indiv <-  renderText({
  my_final <<- paste(choix_grp(),as.character(),  sep=",") 
})

output$test <- renderText({
  my_final <<- paste(choix_test(),as.character(),  sep=",") 
})

mypval <- reactive({
  return(output$myPVAL)
})

output$myPVAL <- renderText({
  input$pval
})


output$myFC <- renderText({
  input$fc
})

output$myMET <- renderText({
  input$method2
})

output$myCLUST <- renderText({
  input$clusters
})

output$myMAT <- renderText({
  input$dist
})

# output$myCOL <- renderText({
#   input$
# })



