##################################
##################################
##                              ##
## Shiny application            ##
##################################
##                              ##
## Author: Franck Soubès        ##
##################################
##################################


#source("plotHeatmaps.r")
source("compat.R")
source("formating.R")
source("global.R")
#source("PCA.R")


options(shiny.maxRequestSize = 40 * 1024 ^ 2) # defined the maximum size in Mb that R can load for one file


ui <- fluidPage(theme = shinytheme("united"),
                
                navbarPage(
                  'Test App',
                  
                  headerPanel("Import data files"),
                  #sidebarLayout(
                  
                  sidebarPanel(
                    width = 3,
                    
                    tabsetPanel(
                      id = "tabset",
                      tabPanel(
                        "Heatmap",
                        
                        fileInput(
                          "file1",
                          "Choose your csv file",
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")
                          ,
                          multiple = T
                        ),
                        style = " font-size:100%; font-family:Arial;
                        border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
                        #tags$style("#myNumericInput {font-size:10px;height:10px;}"),
                        
                        downloadButton("save", "Save your plot"),
                        hr(),
                        
                        numericInput('clusters', 'Cluster count', 3,
                                     min = 1, max = 15),
                        hr(),
                        
                        numericInput('key', 'Cluster count', 1,
                                     min = 0, max = 2),
                        
                        
                        sliderInput(
                          "pval",
                          "P-value:",
                          min = 0.01,
                          max = 0.05,
                          value = 0.05,
                          step = 0.01
                        ),
                        
                        hr(),
                        
                        selectInput(
                          "dist",
                          "Choose your matrix distance",
                          choices = c("cor", "euclidian")
                        ),
                        hr(),
                        
                        selectInput("form", "Choose your file format",
                                    choices = c("png", "eps")),
                        hr(),
                        
                        actionButton("first", "Print Heatmap", style =
                                       "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                        
                        wellPanel(uiOutput("individusel"))
                      ),
                      tabPanel(
                        "PCA",
                        
                        numericInput("normCount", "Count", 100),
                        numericInput("normMean", "Mean", 0),
                        numericInput("normSd", "Std Dev", 1)
                        
                      ),
                      
                      
                      tabPanel(
                        "Display data",
                        
                        DT::dataTableOutput("mytable3"),
                        
                        fileInput(
                          "file2",
                          "Choose your csv file",
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")
                          ,
                          multiple = T
                        ),
                        style = " font-size:100%; font-family:Arial;
                        border-color: #2e6da4; background-color: #337ab7, width: 28px; "
                        
                        ,
                        actionButton("second", "Print Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                      )
                      
                    )
                  ),
                  
                  # conditionalPanel(
                  # 'input.dataset === "atom"',
                  # helpText("Click the column header to sort a column."))),
                  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    tags$style(
                      type = "text/css",
                      ".shiny-output-error { visibility: hidden; }",
                      ".shiny-output-error:before { visibility: hidden; }"
                    ),
                    ### no more error messages
                    
                    bsAlert("alert"),
                    #tableOutput(ouputId= "mytable3"),
                    #tabPanel("seq", DT::dataTableOutput("mytable3")),
                    # Output: Plot
                    
                    plotOutput(outputId = "distPlot")
                    
                  )
                  
                  # ),
                  # navbarPage('Testing App',
                  #
                  #            headerPanel("Import data files"),
                  #            #sidebarLayout(
                  #
                  #            sidebarPanel(width = 3,
                  #
                  #                         tabsetPanel(id = "tabset",
                  #                                     tabPanel("Heatmap",
                  #
                  #                                              fileInput("file1","Choose your csv file",accept = c(
                  #                                                "text/csv",
                  #                                                "text/comma-separated-values,text/plain",
                  #                                                ".csv")
                  #                                                , multiple = T), style=" font-size:100%; font-family:Arial;
                  #                                              border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
                  #                                              #tags$style("#myNumericInput {font-size:10px;height:10px;}"),
                  #
                  #                                              downloadButton("save", "save your plot"),
                  #                                              hr(),
                  #
                  #                                              numericInput('clusters', 'Cluster count', 3,
                  #                                                           min = 1, max = 15),
                  #                                              hr(),
                  #
                  #                                              sliderInput("pval", "P-value:",
                  #                                                          min = 0.01, max = 0.05,
                  #                                                          value = 0.05, step = 0.01),
                  #                                              hr(),
                  #
                  #                                              selectInput("dist", "Choix de la distance",
                  #                                                          choices = c("cor","euclidian")),
                  #                                              hr(),
                  #
                  #                                              actionButton("first", "Print Heatmap",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  #
                  #                                              wellPanel(uiOutput("individusel")
                  #
                  #
                  #                                              )),
                  #                                     tabPanel("PCA",
                  #
                  #                                              numericInput("normCount", "Count", 100),
                  #                                              numericInput("normMean", "Mean", 0),
                  #                                              numericInput("normSd", "Std Dev", 1)
                  #
                  #                                     ),
                  #
                  #
                  #                                     tabPanel("Display data",
                  #
                  #                                              DT::dataTableOutput("mytable3"),
                  #
                  #                                              fileInput("file2","Choose your csv file",accept = c(
                  #                                                "text/csv",
                  #                                                "text/comma-separated-values,text/plain",
                  #                                                ".csv")
                  #                                                , multiple = T), style=" font-size:100%; font-family:Arial;
                  #   border-color: #2e6da4; background-color: #337ab7, width: 28px; "
                  #
                  #                                              , actionButton("second", "Print Data",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                  #
                  #                         )),
                  #
                  #            # conditionalPanel(
                  #            # 'input.dataset === "atom"',
                  #            # helpText("Click the column header to sort a column."))),
                  #
                  #            # Main panel for displaying outputs ----
                  #            mainPanel(
                  #
                  #              tags$style(type="text/css",
                  #                         ".shiny-output-error { visibility: hidden; }",
                  #                         ".shiny-output-error:before { visibility: hidden; }"), ### no more error messages
                  #
                  #              bsAlert("alert"),
                  #              #tableOutput(ouputId= "mytable3"),
                  #              #tabPanel("seq", DT::dataTableOutput("mytable3")),
                  #              # Output: Plot
                  #
                  #              plotOutput(outputId = "distPlot")
                  #
                  #            )
                  #
                  # )
                ))