if (interactive()) {
  shiny::shinyApp(
    ui = shiny::fluidPage(
      useShinyjs(),  # Set up shinyjs
      shiny::actionButton("btn", "Click me"),
      p(id = "element", "Click me to change my text"),
      verbatimTextOutput("element")
    ),
    server = function(input, output) {
      # Change the following lines for more examples
      onclick("btn", info(date()))
      onclick("element", output$element <- renderPrint({runif(n = 1,0,1000)}))
    }
  )
}