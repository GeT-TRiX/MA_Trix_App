#########################################
######## Venn part                      #
#########################################



observeEvent(input$vennd, {
  output$myVenn <- renderPlot({
    Vennplot()
  }, width = 1200 , height = 800, res = 100)
  
  
  output$savevenn <- downloadHandler(filename <- function() {
    paste0(basename(file_path_sans_ext("myfile")),
           '_venn_diagram.',
           input$formven,
           sep = '')
  },
  content <- function(file) {
    print(input$formven)
    if (input$formven == "emf")
      
      emf(
        file,
        width = 12,
        height = 12,
        pointsize = 12,
        coordDPI = 300
      )
    
    else if (input$formven == "png")
      png(
        file,
        width = 1200,
        height = 1200,
        units = "px",
        pointsize = 12,
        res = 100
      )
    else
      eps(file,
          width = 12,
          height = 12)
    
    
    plot(Vennplot())
    #Vennplot()
    dev.off()
  })
  
})