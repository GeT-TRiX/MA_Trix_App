output$myNUM <- renderPrint({
  if(is.null(formated()))
    return("X")
  else
    cat(length(formated()))
})


output$indiv <-  renderText({
  my_final <<- paste(choix_grp(),as.character(),  sep=",") 
})

output$indivcol <-  renderText({
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

output$myPAL <- renderText({
  if(is.null(mypal()))
    palette[1:length(choix_grp())]
  else
    paste(mypal(),as.character(),  sep=",")
})

output$myLEG <- renderText({
  input$legsize
})

output$myROW <- renderText({
  input$rowsize
})
output$myCOL <- renderText({
  input$colsize
})


