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
      ),
      
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
      "P-value treshold",
      min = 0.01,
      max = 0.05,
      value = 0.05,
      step = 0.01
    ),
    
    br(),
    
    
    sliderInput(
      "fc",
      "FC treshold",
      min = 1,
      max = 10,
      value = 1,
      step = 1
    ),
    
    br(),
    
    selectInput(
      "method2",
      "Choose your matrix distance",
      choices = c("FDR", "None")
    ),
    
    br(),
    
    shiny::actionButton(
      "toggleAdvanced",
      "Advanced Computational Options",
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
      
        selectInput(
          "dist",
          "Choose your matrix distance",
          choices = c("correlation", "euclidian")
        ),
        
        checkboxInput("meangrp",
                      "Compute the mean for the different groups",
                      FALSE),
        verbatimTextOutput("value")
      )
      
    )),
    br(),
    
    shiny::actionButton(
      "toggleAdvancedcolors",
      "Advanced Color Settings",
      href = "#",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
    ),
    
    br(),
    
    shinyjs::hidden(
      div(id = "advancedcol",
          wellPanel(fluidRow(
            column(5,
              colourpicker::colourInput(
                "col1",
                "Select colour for downregulated genes",
                firstcol,
                palette = "limited"
              )
            ),
            column(5,
              colourpicker::colourInput("col3", "Select colour for upregulated genes", lastcol,
                                        palette = "limited")
            )
          ), 
          fluidRow(
            column(5,
              numericInput('rowsize','Row size',0.6,
                min = 0.2,
                max = 1.5,
                step = 0.1
              )
            ),
            column(5,
              numericInput('colsize','Col size',0.6,
                min = 0.2,
                max = 1.5,
                step = 0.1
              )
            )
          ),

          numericInput('legsize','Legende size',0.6,
            min = 0.2,
            max = 1.5,
            step = 0.1
          ),
          
          fluidRow(column(
            5,
            radioButtons(
              "rowname",
              "show/hide rowname",
              choiceNames = list("hide",
                                 "show"),
              choiceValues = list(F, T),
              selected= "hide"
            )
            
          ),
          column(
            5,
            radioButtons(
              "colname",
              "show/hide colnames",
              choiceNames = list("hide",
                                 "show"),
              choiceValues = list(T, F),
              selected =  "show"
            )
          )
          
          ),
          
          uiOutput('myPanel'),
          
          br()
          ))), 
      
  
      br(),
      
      selectInput("form", "Choose your file format",
                  choices = c("png", "eps","emf")),
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
        p(icon("table"), "cutheatmap"),
        column(
          12,
          
          h3(
            "This table represent the significant genes for different condition"
          ),
          helpText(
            "Warning according to the number of NA for a given parameter, the analysis should be strongly biased"
          )
          ,
          dataTableOutput("data_sign")
        )
        
      )
    ))
)