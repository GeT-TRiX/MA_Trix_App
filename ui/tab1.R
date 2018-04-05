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
    ),
    sliderInput(
      "pval1",
      "P-value:",
      min = 0.01,
      max = 0.05,
      value = 0.05,
      step = 0.01
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
    ),
    column(
      12,
      
      h3(
        "This table represent the significant genes for different condition"
      ),
      helpText(
        "Warning according to the number of NA for a given parameter, the analysis should be strongly biased"
      )
      ,
      dataTableOutput("data_summary")
    )
    
  )
)