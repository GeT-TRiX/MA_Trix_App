#########################################
######## Venn part                      #
#########################################



#observeEvent(input$vennd, {
  output$myVenn <- renderPlot({
    validate(
      need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(length(user_cont()) >0, 'You need to  select some groups and your p-value!'))
    
    req(Vennplot())
    
    Vennplot()
  }, width = 1200 , height = 800, res = 100)
  
 
observe({
  
  validate(
    need(csvf(), 'You need to import data to visualize this plot!'))
   
  output$savevenn <- downloadHandler(filename <- function() {
    paste0(basename(tools::file_path_sans_ext(projectname())),
           '_venn_diagram.',
           input$formven,
           sep = '')
  },
  content <- function(file) {
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

    dev.off()
  })
  
})