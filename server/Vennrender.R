#########################################
######## Venn part                      #
#########################################



#observeEvent(input$vennd, {
  output$myVenn <- renderPlot({
    validate(
      need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(length(user_cont()) >0,  'You need to  select your p-value and then some groups!'))
    
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
    if (input$formven == "pdf")
      
      pdf(
        file,
        width = 12,
        height = 12,
        pointsize = 12
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
      #ggsave(file, device=cairo_ps, fallback_resolution = 600)
      # eps(file, paper="special", family="Helvetica", fonts=c("serif","Helvetica"),
      #     width = 12,
      #     height = 12)
      cairo_ps(filename=file, width=11, height=11,pointsize = 12)
    
    
    plot(Vennplot())

    dev.off()
  })
  
})