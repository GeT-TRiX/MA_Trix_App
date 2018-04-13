###############################
######## Adding mean by group #
###############################

output$value <- renderText({
  input$meangrp
})

mean_grp <- reactive({
  return(output$value)
})


