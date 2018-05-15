library(shiny) # http://rstudio.github.com/shiny/tutorial/#hello-shiny
library(Rgraphviz) # http://www.bioconductor.org/packages/2.11/bioc/html/Rgraphviz.html

server <- function(input, output) {
  output$graphPlot <- reactivePlot(function() {
    g <- randomGraph(1:input$V, 1:input$M, input$p)
    plot(g)
  })
}


ui = pageWithSidebar(
  
  headerPanel("Shiny Random Graph"),
  
  sidebarPanel(
    
    sliderInput("V", 
                "Number of nodes:", 
                min = 1, 
                max = 100, 
                value = 10),
    
    sliderInput("M", 
                "M:", 
                min = 1, 
                max = 10, 
                value = 4),
    
    sliderInput("p", 
                "p:", 
                min = 0, 
                max = 1, 
                value = 0.5),
    
    helpText("Code available at https://gist.github.com/4045750")
  ),
  
  mainPanel(
    plotOutput("graphPlot")
  )
)

shinyApp(ui = ui, server=server)
