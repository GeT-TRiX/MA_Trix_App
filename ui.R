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


shinyUI(
  ui <- bootstrapPage(
    navbarPage(
      "MaTrix_App",
      # MA for microarray and Trix for the name of the team
      
      #useShinyjs(),
      theme = shinytheme("united"),
      
      # multi-page user-interface that includes a navigation bar.
      
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
      ),
      
      
      tabPanel(
        p(icon("line-chart"),
          "Heatmap "),
        
        
        sidebarPanel(
          width = 3,
          style = " font-size:100%; font-family:Arial;
          border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
          #tags$style("#myNumericInput {font-size:10px;height:10px;}"),
          
          br(),
          
          wellPanel(
            uiOutput("individusel")
            ,
            actionButton(
              inputId = "allIndividus",
              label = "Select all",
              icon = icon("check-square-o"),
              style =
                "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
            ,
            actionButton(
              inputId = "noIndividus",
              label = "Clear selection",
              icon = icon("square-o"),
              style =
                "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
            ,
            
            p("You've selectionned the following individuals : "),
            hr(),
            verbatimTextOutput("indiv")
            
          ),
          wellPanel(
            uiOutput("testout")
            ,
            actionButton(
              inputId = "allTests",
              label = "Select all",
              icon = icon("check-square-o"),
              style =
                "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
            ,
            actionButton(
              inputId = "noTests",
              label = "Clear selection",
              icon = icon("square-o"),
              style =
                "color: #fff; background-color: #337ab7; border-color: #2e6da4"
            )
            # ,
            # actionButton(
            #   inputId = "refresh",
            #   label = "Update selection",
            #   icon = icon("repeat")
            # )
            ,
            p("You've selectionned the following test : "),
            hr(),
            verbatimTextOutput("test")
            
          ),
          
          sliderInput(
            "pval",
            "P-value:",
            min = 0.01,
            max = 0.05,
            value = 0.05,
            step = 0.01
          ),
          
          br(),
          
          shiny::actionButton(
            "toggleAdvanced",
            "Show Advanced Options",
            href = "#",
            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
          ),
          
          br(),
          
          shinyjs::hidden(div(
            id = "advanced",
            wellPanel(
              numericInput('clusters', 'Cluster count', 3,
                           min = 1, max = 15),
              
              br(),
              
              
              # numericInput('key', 'Cluster count', 1,
              #              min = 0, max = 2),
              
              
              selectInput(
                "dist",
                "Choose your matrix distance",
                choices = c("cor", "euclidian")
              ),
              
              checkboxInput("meangrp", "Add Mean for the different", FALSE),
              verbatimTextOutput("value")
            )
            
          )),
          
          br(),
          
          selectInput("form", "Choose your file format",
                      choices = c("png", "eps")),
          br(),
          downloadButton("save", "Save your plot" , style =
                           "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          
          br(),
          br(),
          
          #actionButton("heatm", "Print Heatmap", style =
          #               "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          
          shiny::actionButton("heatm", "Print Heatmap", style =
                                "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          uiOutput('Button'),
          
          numericInput('num', '', 0),
          verbatimTextOutput("valuedd")
        ),
        
        mainPanel(tabsetPanel(
          tabPanel(
            p(icon("line-chart"), "Visualize the Heatmap"),
            tags$style(
              type = "text/css",
              ".shiny-output-error { visibility: hidden; }",
              ".shiny-output-error:before { visibility: hidden; }"
            ),
            
            useShinyjs(),
            ### no more error messages
            bsAlert("alert"),
            plotOutput(outputId = "distPlot")
            
          ),
          tabPanel
          (
            p(icon("table"), "Dataset"),
            column(
              12,
              
              h3("This table represent the significant genes for different condition"),
              helpText(
                "Warning according to the number of NA for a given parameter, the analysis should be strongly biased"
              )
              ,
              dataTableOutput("data_sign")
            )
            
          )
        ))
      ),
      
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
        
        mainPanel(
          
          
          
          
          
          
          
          
          
          
          
          
        )),
      tabPanel(p(icon("question-circle"),
                 "How to use?"),
               mainPanel(includeMarkdown("markdown/help.md")))
      
      ,
      tabPanel(p(icon("info-circle"),
                 "About"),
               mainPanel(includeMarkdown("markdown/about.md")))
      
    )
  )
)