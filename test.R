library(shiny)
library(ggplot2)

ui <- fluidPage(
    
    selectInput("species", "choose your species",
                choices = c("setosa", "virginica", "versicolor")),
    downloadButton(
      "plotsave",
      "Save your plot" ),
  
  mainPanel(plotOutput("tgPlot"))
)

server <- function(input, output, session) {
  
  
  tgPlot <- function(inputSpecies){
    plot1 <-ggplot(iris[iris$Species==inputSpecies,])+geom_point(aes(Sepal.Length ,Sepal.Width))
    print(plot1)
  }
  
  output$tgPlot <- renderPlot({
    tgPlot(input$species)
  }) 
  
  output$plotsave <- downloadHandler(
    filename = 'plot.pdf',
    content = function(file){
      pdf(file = file, width=12, height=4)
      tgPlot(input$species)
      dev.off()
    }
  )
  
}

shinyApp(ui = ui, server = server)
