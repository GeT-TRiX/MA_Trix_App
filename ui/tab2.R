tabPanel(
  p(icon("line-chart"),
    "Heatmap "),
  titlePanel("Heatmap settings"),
  sidebarPanel(
    tabsetPanel(
      tabPanel("Heatmap", uiOutput("aToZPlayerList"),

    width = 3,
    style = " font-size:100%; font-family:Arial;
    border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
    tags$style("#myNumericInput {font-size:10px;height:10px;}"),
    br(),
    wellPanel(
      uiOutput("individusel"),
      actionButton(
        inputId = "allIndividus",
        label = "Select all",
        icon = icon("check-square-o"),
        style =
          "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      actionButton(
        inputId = "noIndividus",
        label = "Clear selection",
        icon = icon("square-o"),
        style =
          "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    ),
    wellPanel(
      uiOutput("testout"),
      actionButton(
        inputId = "allTests",
        label = "Select all",
        icon = icon("check-square-o"),
        style =
          "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      actionButton(
        inputId = "noTests",
        label = "Clear selection",
        icon = icon("square-o"),
        style =
          "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
      #,
      # 
      # p("You've selectionned the following test : "),
      # hr(),
      # verbatimTextOutput("test")
      
    ),
    br(),
    numericInput(
      'maxgen',
      'Maximal number of genes by groups',
      NULL,
      min = 100,
      max = 1500
    ),
    br(),
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
    
    shinyjs::hidden(div(
      id = "advancedcol",
      wellPanel(
        fluidRow(
          column(
            5,
            colourpicker::colourInput(
              "col1",
              "Select colour for downregulated genes",
              firstcol,
              palette = "limited"
            )
          ),
          column(
            5,
            colourpicker::colourInput("col3", "Select colour for upregulated genes", lastcol,
                                      palette = "limited")
          )
        ),
        fluidRow(column(
          5,
          numericInput(
            'rowsize',
            'Row size',
            0.9,
            min = 0.2,
            max = 1.5,
            step = 0.1
          )
        ),
        column(
          5,
          numericInput(
            'colsize',
            'Col size',
            0.9,
            min = 0.2,
            max = 1.5,
            step = 0.1
          )
        )),
        
        numericInput(
          'legsize',
          'Legend size',
          0.8,
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
            selected = "hide"
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
        )),
        
        uiOutput('myPanel'),
        
        
        br()
      )
    )),
    
    br(),
    
    selectInput("form", "Choose your file format",
                choices = c("png", "eps", "emf")),
    br(),
    downloadButton("save", "Save your plot" , style =
                     "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    
    br(),
    br(),
    
    
    # shiny::actionButton("heatm", "Print Heatmap", style =
    #                       "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    
    uiOutput("button"), 
    shinyjs::disabled(actionButton("stop", "Stop"))),
    tabPanel("clustering of heatmap", 
             wellPanel(
               
               sliderInput(
                 "cutheight",
                 "Choose where you cut the heatmap",
                 min = 1,
                 max = 15,
                 value = 2,
                 step = 0.5
               ),
               
               uiOutput("cutcluster"),
              
               selectizeInput('cutinfo', 'Choose your types of plots', choices = cutheatmlist),
               
               
               br(),
               selectInput("formcut", "Choose your file format",
                           choices = c("png", "eps", "emf")),
               
               shiny::actionButton("cutheat", "Plot cluster", style =
                                     "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               downloadButton('downloadcut',"Download the data", 
                              style =
                                "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
               downloadButton("savecut", "Save your plot" , style =
                                "color: #fff; background-color: #337ab7; border-color: #2e6da4")
               
             )
  #),
      ))),
  mainPanel(
    tabsetPanel(
      tabPanel(
        p(icon("line-chart"), "Visualize the Heatmap"),
        tags$style(
          type = "text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"
        ),
        ### from the library ShinyJS
        useShinyjs(),
        ### no more error messages
        bsAlert("alert"),
        #plotOutput(outputId = "distPlot"),
        plotOutput("distPlot"),
      ### Adding white spaces between the heatmap plot and the tracker
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),
      h1("Here's a tracker for your different selections:"),
      #br(),
      wellPanel(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),
        
        tags$head(tags$style("
                             #container * {
                             display: inline;
                             }")),
        div(
          id = "container",
          p("There are"),
          htmlOutput("myNUM"),
          p("significant genes"),
          p("with the following contrasts"),htmlOutput("test")
        ),
      #br(),
      div(
        id = "container",
        p('The selected rows for your heatmap are based on the '),
        textOutput("myMET"),
        p("method, with a P-value and FC treshold respectively set to "),
        textOutput("myPVAL"),
        p('and'),
        textOutput("myFC")
      ),
      #br(),
      div(
        id = "container",
        p('The'),
        textOutput("myMAT"),
        p(
          "method was used to compute the matrix distance with a number of clusters for the significant genes equal to",
          textOutput("myCLUST")
        )
      ),
      div(
        id = "container",
        p('The advanced color settings choosen for the following groups :'),
        textOutput("indivcol"),
        p("are respectively correlated to the successive colors"),
        htmlOutput("myPAL")
      ),
      div(
        id = "container",
        p(
          'The legend size, row size, col size are respectively equals to ',
          textOutput("myLEG"),
          p(','),
          textOutput("myROW"),
          p(','),
          textOutput("myCOL")
        )
      )
        )
    ),
    tabPanel
    (p(icon("table"), "cutheatmap"),
      bsAlert("alert"),
      plotlyOutput(outputId = "cutheatmap"),

      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),
      
      verbatimTextOutput("event")
      )
    ))
)