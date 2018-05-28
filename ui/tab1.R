source("ui/uiport.R")

tabPanel( 
  p(icon("upload"), #icon of the panel used with boostrap icons
    # https://www.w3schools.com/icons/bootstrap_icons_glyphicons.asp
    "Data loading"),# name of the Navbar
  titlePanel("Upload Data"), 
  
  sidebarPanel( #Create a rectangular region containing the different widgets
    style = " font-size:100%; font-family:Arial;
    border-color: #2e6da4; background-color: #337ab7, width: 28px; ", #CSS attributes for the sidebarPanel
    width = 3,
    
    downloadLink("downloadData", label = "download sample data"),
    br(),br(),
    #csvFileInput("file", "Choose your csv files"),
    fileInput( # browse button (UI)
      "file",
      "Choose your csv files",
      accept = c("text/csv", #accept only csv and text files
                 "text/comma-separated-values,text/plain",
                 ".csv"),
      multiple = T # Attribute to load multiple data at once
    ),
    
    br(), # white space (<br> </br> basic HTML)
    
    # selectInput("method2", "Choose your matrix distance:", selected = "FDR", 
    #             c("adj.p.val(FDR)" = "FDR", "p.value(raw)" = "None" )),  
    
    
    selectInput("method", #  Create a select list that can be used to choose a single or multiple items from a list of values.
                "Choose your statistical method",
                choices = c("adj.p.val(FDR)" = "FDR", "p.value(raw)" = "None"))

  ),
  
  mainPanel( #Render right screen
    bsAlert("alert"),#Alert message depending of the user's input
    conditionalPanel(condition = 'output.boolmark', #Hide or Show event depending on the loading data success or failure
    includeMarkdown("markdown/help.md"),
    br(),br(),br(),br(),br(),br(),br(),br(),
    hr()
    ),
    
    # conditionalPanel(condition = 'output.boolmark',
    #                  br(),br(),br(),br(),br(),br(),br(),br()
    #                  ,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
    #                  br(),br(),br(),br(),br(),br(),br(),br()
    #                  ,br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
    #                  br(),br(),br(),br(),br(),br(),br(),br()
    #                  ,br(),br(),br(),br(),br(),br(),br(),br(),br(),br()),
    
    #conditionalPanel(condition = '!output.boolmark',bsAlert("alert"),
    column( #Create a column for use within a fluidRow or fixedRow
      12,
      
      h3(
        "This table summarize the significant genes depending on the p-value treshold choosen with the slider bar"
      ),
      helpText("Choose your p-value treshold to modify the following data table")
      ,
      sliderInput( #Constructs a slider widget to select a numeric value from a range.
        "pval1",
        "",
        min = 0.01,
        max = 0.05,
        value = 0.05,
        step = 0.01
        ,
        width = "500"
      )
      
      ,
      dataTableOutput("data_summary") # render a renderTable or renderDataTable within an application page
    ),
    
    
    column(
      12,
      
      h3("This table shows the samples with the corresponding groups"),
      dataTableOutput("new_test")
    ),
    
    column(
      12,
      
      h3("This table shows the head of the working set"),
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