#########################################
######## PCA part                       #
#########################################

PCAres <- reactive({
  
  if (is.null(csvf()[[1]]))
    return(NULL)
  
  mypca = res.pca(csvf()[[1]][,-1], scale =F)
  return(mypca)
})

Scree_plot <- reactive({
  mybar = eboulis(PCAres())
  return(mybar)
})


output$eigpca <- renderPlot({
  plot(Scree_plot())
  
}, width = 1200 , height = 600, res = 100)

labeled <- reactive({
  
  if(input$label == T)
    showlab = "all"
  else 
    showlab = "none"
  
  return (showlab)
})


output$PCA <- renderPlot({
  
  plot(PCAplot())
  
}, width = 1200 , height = 800, res = 100)


output$savepca <- downloadHandler(
  
  filename <- function() {
    paste0(basename(file_path_sans_ext("myfile")), '_pca.png', sep='')    
  },
  content <- function(file) {
    
    png(file,
        width =1400,
        height = 1400,
        units = "px",
        pointsize= 12,
        res=100
    )
    
    plot(PCAplot())
    dev.off()
  })