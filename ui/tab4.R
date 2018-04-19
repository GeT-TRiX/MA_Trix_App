tabPanel(
  "Venn Diagram",
  
  wellPanel(
    uiOutput("contout")
    ,
    actionButton(
      inputId = "allCont",
      label = "Select all",
      icon = icon("check-square-o"),
      style =
        "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    )
    ,
    actionButton(
      inputId = "noCont",
      label = "Clear selection",
      icon = icon("square-o"),
      style =
        "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    )
    #,
    # shiny::actionButton("vennd", "Print Venn diagram", style =
    #                       "color: #fff; background-color: #337ab7; border-color: #2e6da4")
    
  ),
  
  
  mainPanel(bsAlert("alert"),
            plotOutput(outputId = "myVenn"))
)