tabPanel(
  p(icon("line-chart"), "PCA"),
  
  sidebarPanel(
    style = " font-size:100%; font-family:Arial;
    border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
    width = 3 ,
    
    numericInput("normCount", "Count", 100),
    numericInput("normMean", "Mean", 0),
    numericInput("normSd", "Std Dev", 1)
    
  ),
  
  mainPanel()
)