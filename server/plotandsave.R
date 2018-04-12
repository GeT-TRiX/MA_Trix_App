observeEvent(input$heatm, {
  
  updateActionButton(session,
                     "heatm",
                     label = "Update Heatmap",
                     icon = icon("repeat"))
  
  output$distPlot <- renderPlot({
    
    heatmapfinal()
    
  }, width = 900 , height = 1200, res = 100)
  
  
  output$save <- downloadHandler(
    
    filename <- function() {
      paste0(basename(file_path_sans_ext("myfile")), '_heatmap.', input$form, sep='')
    },
    content <- function(file) {
      
      if(input$form == "emf")  
        
        emf(file,
            width = 7,
            height = 7,
            pointsize = 12,
            coordDPI = 300
        )
      
      else if(input$form == "png")
        png(file,
            width =800,
            height = 1200,
            units = "px",
            pointsize= 12
        )
      else 
        eps(
          file,
          width = 7,
          height = 7
        )
      
      heatmapfinal()
      dev.off()
    }
  )
  
})