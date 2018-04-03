tabPanel(
  p(icon("upload"),
    "Data loading"),
  
  sidebarPanel(
    style = " font-size:100%; font-family:Arial;
    border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
    width = 3,
    
    br(),
    
    fileInput(
      "file1",
      "Choose your csv file",
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv")
      ,
      multiple = T
    )
  ),
  mainPanel(
    bsAlert("alert"),
    
    column(
      12,
      
      h3("Show the actual data frame with the columns selected"),
      helpText(
        "Warning according to the number of NA for a given parameter, the analysis should be strongly biased"
      )
      ,
      dataTableOutput("new_data")
    ),
    column(
      12,
      
      h3("Show the actual data frame with the columns selected"),
      helpText(
        "Warning according to the number of NA for a given parameter, the analysis should be strongly biased"
      )
      ,
      dataTableOutput("new_group")
    ),
    column(
      12,
      
      h3("Show the actual data frame with the columns selected"),
      helpText(
        "Warning according to the number of NA for a given parameter, the analysis should be strongly biased"
      )
      ,
      dataTableOutput("new_test")
    )
  )
)