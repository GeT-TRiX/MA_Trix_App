source("./module/csvmodules.R")

##################################################################################
# Here starts main program. Lines above can be sourced: source("path-to-module.R")
##################################################################################
options(shiny.maxRequestSize=2000000000)
library(shiny)
library(data.table)
library(shinyBS)

ui <- shinyUI(navbarPage(
  "My Application",
  tabPanel("File upload",
           bsAlert("alert"),
           dataTabUI(
             "tab1",
             csvFileInput("datafile", "User data (.csv format)"),
             "table"
           )),
  tabPanel("Plot", plotTabUI(
    "tab2", basicPlotUI("plot1"), "plotOutput"
  ))
  
))


server <- function(input, output, session) {
  datafile <- callModule(csvFile, "datafile",
                         stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    req(datafile())
    datafile()
    View(datafile()[[1]])
  })
  
  plotData <- callModule(basicPlot, "plot1", datafile())
  
  output$plotOutput <- renderPlot({
    plot(plotData())
  })
}


shinyApp(ui, server)
runApp()
#retest with https://stackoverflow.com/questions/27080089/how-to-organize-large-shiny-apps/27122115