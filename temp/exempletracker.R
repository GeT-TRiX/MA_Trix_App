require(shiny)
runApp(list(ui = pageWithSidebar(
  headerPanel("censusVis"),
  sidebarPanel(
    helpText("Create demographic maps with 
             information from the 2010 US Census."),
    selectInput("var", 
                label = "Choose a variable to display",
                choices = c("Percent White", "Percent Black",
                            "Percent Hispanic", "Percent Asian"),
                selected = "Percent White"),
    sliderInput("range", 
                label = "Range of interest:",
                min = 0, max = 100, value = c(0, 100))
    ),
  mainPanel(textOutput("text1"),
            textOutput("text2"),
            htmlOutput("text")
  )
),
server = function(input, output) {
  output$text1 <- renderText({paste("You have selected", input$var)})
  output$text2 <- renderText({paste("You have chosen a range that goes from",
                                    input$range[1], "to", input$range[2])})
  output$text <- renderUI({
    str1 <- paste("You have selected", input$var)
    str2 <- paste("You have chosen a range that goes from",
                  input$range[1], "to", input$range[2])
    HTML(paste(str1, str2, sep = '<br/>'))
    
  })
}
)
)