library(shiny)
library(shinyjs)

groupss <- read.csv2("data/TOXA_HEGU_MA0191 _AllChip_pData.csv", sep= ";" , dec = ",",header= T)
test<-unique(sort(groupss$Grp))
print(test)
length(test)
levels(test)[1]
palette= c( "#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442", "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray", "burlywood1","darkkhaki", "#CC0000" )
mypal= palette
class(mypal)
palette[1]


dat <- data.frame(matrix(rnorm(120, 2, 3), ncol=length(test)))


runApp(shinyApp(
  ui = fluidPage(
    uiOutput('myPanel'),
    plotOutput("plot")
  ),
  server = function(input, output, session) {
    
    cols <- reactive({
      lapply(seq_along(test), function(i) {
        colourpicker::colourInput(paste("col", i, sep="_"), levels(test)[i], palette[i])        
      })
    })
    
    output$myPanel <- renderUI({cols()})
    
    # Put all the input in a vector
    colors <- reactive({
      lapply(seq_along(test), function(i) {
        input[[paste("col", i, sep="_")]]
      })
    })
    
    output$plot <- renderPlot({
      print(cols())
      print(colors())
      if (is.null(input$col_1)) {
        cols <- rep("#000000", ncol(dat))
      } else {
        cols <- unlist(colors())
      }
      print(cols)
      plot(dat[,1], col = cols[1])
      for(i in 2:ncol(dat)) lines(dat[,i], col = cols[i])
    })
  }
))