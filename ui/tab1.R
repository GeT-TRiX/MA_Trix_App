source("ui/uiport.R")

tabPanel(
  p(icon("upload"),
    "Data loading"),
  
  sidebarPanel(
    style = " font-size:100%; font-family:Arial;
    border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
    width = 3,
    
    br(),
    
    
    #csvFileInput("file", "Choose your csv files"),
    fileInput(
      "file",
      "Choose your csv file",
      accept = c("text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv"),
      multiple = T
    ),
    
    br(),
    
  #   sliderInput(
  #     "pval1",
  #     "P-value treshold",
  #     min = 0.01,
  #     max = 0.05,
  #     value = 0.05,
  #     step = 0.01
  #   )
  # ,
  
  selectInput(
    "method",
    "Choose your matrix distance",
    choices = c("FDR", "none")
  )
  #colourInput("col", "Select colour"),
  #colourpicker::colourInput("col", "Select colour")
  
  ),
  
  mainPanel(
    bsAlert("alert"),
    
    column(
      12,
      
      h3(
        "This table summarize the significant genes depending on the p-value treshold choosen through the slider bar"
      ),
      helpText(
        "Choose your p-value treshold to modify the following data table"
      )
      ,
      sliderInput(
        "pval1",
        "",
        min = 0.01,
        max = 0.05,
        value = 0.05,
        step = 0.01
        ,width = "500")
      #)
      ,
      dataTableOutput("data_summary")
    ),


    column(
      12,
      
      h3("This table shows the samples with the accoording groups"),
      helpText(
        "Warning according to the number of NA for a given parameter, the analysis should be strongly biased"
      )
      ,
      dataTableOutput("new_test")
    ),
    
    column(
      12,
      
      h3("This table shows the head of the working set"),
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
    )
    
  )
)