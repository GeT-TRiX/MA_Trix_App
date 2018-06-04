#navbarMenu("menu",
tabPanel(
  p(icon("line-chart"),
    "Heatmap"),
  titlePanel("Heatmap settings"),
  sidebarPanel(tabsetPanel(id = "heatmconf",
    tabPanel(
      "Heatmap",
      value="hmpan",
      width = 3,
      style = " font-size:100%; font-family:Arial;
      border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
      tags$style("#myNumericInput {font-size:10px;height:10px;}"),
      br(),
      actionLink("resetAll",  label = ("reset all"), style="float:right"),
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
      div(
        id = "form",
      
      wellPanel(
        # Creates a panel with a slightly inset border and grey background
        uiOutput("testout"),
        actionButton(
          # Action button that automatically react when triggered
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
      fluidRow( column(5,
      numericInput(
        # Create an input control for entry of numeric values
        'maxgen',
        'Maximal number of genes by comparison(s)',
        NULL,
        min = 100,
        max = 1500
      )),column(5,
      selectInput(
        "method2",
        "Choose your statistical method",
        choices = c("adj.p.val (FDR)"= "FDR", "p.value (raw)" = "None")
      ))),
      
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
      
      # sliderInput(
      #   "cutheatm",
      #   "Choose where you cut the heatmap",
      #   min = 1,
      #   max = 15,
      #   value = 2,
      #   step = 0.5
      # ),
      
      br(),
      
      # selectInput("method2", "Choose your matrix distance:", selected = "FDR", 
      #             c("adj.p.val(FDR)" = "FDR", "p.value(raw)" = "None" )),
      
      
      shiny::actionButton(
        "toggleAdvanced",
        "Advanced Computational Options",
        href = "#",
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      br(),
      shinyjs::hidden(div(
        # Hide some widgets between the tags
        id = "advanced",
        wellPanel(
          fluidRow(
            column(5,
          numericInput('clusters', 'Cluster count', 3,
                       min = 1, max = 15)),
          #br(),
          column(5,
          selectInput(
            "dist",
            "Choose your matrix distance",
            choices = c("correlation", "euclidian"))
          )),
          fluidRow(
            column(5,
          checkboxInput("meangrp",
                        "Compute the mean for the different groups",
                        FALSE)),
          #verbatimTextOutput("value"),
          column(5,
          checkboxInput("scalcol",
                        "Apply scaling to columns",
                        FALSE))
          
        )
      ))),
      br(),
      
      
      shiny::actionButton(
        "toggleAdvancedcolors",
        "Advanced graphical Settings",
        href = "#",
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      
      br(),
    
      shinyjs::hidden(div(
        id = "advancedcol",
        wellPanel(
          fluidRow(
            column(5,
              colourpicker::colourInput(
                # Creation of a color button
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
            radioButtons("rowname",
                         "show/hide rowname",
                         c("hide", "show"))
            
          ),
          column(
            5,
            radioButtons("colname",
                         "show/hide colnames",
                         c("show", "hide"))
          )),
          
          # fluidRow(column(
          #   5,
          # uiOutput('myPanel1')),
          # column(
          #   5,
          #   uiOutput('myPanel2'))
          # 
          # ),
          uiOutput('myPanel',inline=T),

          br()
        )
      ))), #end of the div "form"
      
      br(),
      
      
      # shiny::actionButton(
      #   "toggleAdvancedgo",
      #   "Advanced enrichment Settings",
      #   href = "#",
      #   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      # ),
      
      shinyjs::hidden(div(
        # Hide some widgets between the tags
        id = "advancedgo",
        wellPanel(
          
        )
      )),
      
      
      br(),
      
      
      fluidRow(column(
        5, uiOutput("button")
       
      ),
      column(
        5,
        checkboxInput("reactheat",
                      "Add reactivity",
                      FALSE))
      ),
    helpText("Note: It is highly advised to check this box if you're working with a set of genes close to 1000."),
      
      # fluidRow(
      #   column(4,
      # downloadButton(
      #   "save",
      #   "Save your plot" ,
      #   style = ## allowed to download an image
      #     "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      # )),
      # 
      # downloadButton('downloadcut', "Download the data",
      #                style ="color: #fff; background-color: #337ab7; border-color: #2e6da4")
      # 
      # ),
    
    conditionalPanel(condition = 'output.heatmbool',

                     fluidRow(column(
                       5,
                       selectInput("Species", "Choose your Species:", selected = "mm9", 
                                   c("Mouse" = "mm9", "Human" = "hg19", "Rat" = "rn4", "C. elegans" = "ce6",
                                     "Zebrafish" = "danRer6",  "Pig" = "susScr3", 
                                      "Chicken" = "galGal3", "Chimpanzee" = "panTro2" ))),
                     column(
                       5,
                       uiOutput("cutgo"))
                     ),
      uiOutput("slidergo"),
      helpText("GO enrichment are ranked from highest to the lowest, with 1 corresponding to the highest"),
      fluidRow(column(3, selectInput("onto", "Category", 
                                     selected ="BP", choices = c("BP", "MF", "CC"))
      ),
      
      column(3,br(),
      actionButton("GO", "Run GO",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
      column(3,br(),
      actionButton("DAVID", "Open DAVID",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
      )),
      
      #actionButton("resetAll", "Reset all"),
      
      br(),
      br()
      
      
      # shiny::actionButton("heatm", "Print Heatmap", style =
      #                       "color: #fff; background-color: #337ab7; border-color: #2e6da4"),

      # Render input from server.R
      #shinyjs::disabled(actionButton("stop", "Stop"))
      
    ),
    
    
    
    tabPanel(
      "Heatmap clustering",
      value="cutpan",
      wellPanel(
        
        # sliderInput(
        #   "cutheight",
        #   "Choose where you cut the heatmap",
        #   min = 1,
        #   max = 15,
        #   value = 2,
        #   step = 0.5
        # ),
        
        uiOutput("cutcluster"),
        
        selectizeInput('cutinfo', 'Choose your types of plots',
                       choices = cutheatmlist),
        # cutheatmlist is a variable defined in the global environment
        
        br(),
        
        selectInput(
          "formcut",
          "Choose your file format",
          choices = c("png", "eps", "emf")
        ),
        
        verbatimTextOutput("event"),
        
        
        shiny::actionButton("updateheatm", "Update the clusters", style =
                              "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        
        downloadButton("savecut", "Save your plot" , style =
                         "color: #fff; background-color: #337ab7; border-color: #2e6da4")
        
      )
    )
  )),
  
  mainPanel(tabsetPanel(id = "mainhmtabset",
    # Tabsets are useful for dividing output into multiple independently viewable sections.
    tabPanel(
      p(icon("line-chart"), "Visualize the Heatmap"),
      value = "hmmainpan",
      tags$style(
        type = "text/css"#,
        #".shiny-output-error { visibility: hidden; }",
        #".shiny-output-error:before { visibility: hidden; }"
      ),
      ### from the library ShinyJS
      useShinyjs(),
      ### no more error messages
      bsAlert("alert"),
      #plotOutput(outputId = "distPlot"),
      #fluidRow(
       # column(1,
      #div(style="display:inline-block",
       fluidRow( column(1 , br(),
               downloadButton(
                 "savehm",
                 "Save your plot" ,
                 style =  "color: #fff; background-color: #337ab7; border-color: #2e6da4"
               )),
               column(1),
               column( 1, 
          selectInput("formhm", "",
                      choices = c("png", "eps", "emf")))
          
          
        
      ),
      
      br(),br(),br(),
      conditionalPanel(condition = '!output.heatmbool',  verbatimTextOutput("warningsheat")
                       ),
      #plotOutput("warningsheat")
      conditionalPanel(condition = 'output.heatmbool',
                       plotOutput("distPlot"),
     
      ### Adding white spaces between the heatmap plot and the tracker
      
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br(),br(),br(),br(),br(),
      br(),br(),br(),br(),br()),
      
      h1("Here's a tracker for your different selections:"),
      #br(),
      wellPanel(
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # add style.css in order to add better police
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
          p("for the following comparison(s)"),
          htmlOutput("test")
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
          p("are respectively correlated to the following colors"),
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
  #),
  
  tabPanel(
    "Heatmap clusters",
    value = "dfhmclu",
    downloadButton('downloadcut', "Download the data",
                   style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    column(
      12,

      h3("Table summarizing the heatmap"),
      helpText(
        "Heatmap's cluster are upside down in order to match the genes with the heatmap from top to bottom"
      )
      ,
      dataTableOutput("clustering")
    )
  ),
  
  tabPanel(
    "(GO) enrichment-based cluster analysis",value="maingo",
    downloadButton("savego", "Save your enrichment" , style =
                     "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
    verbatimTextOutput("clustgo")
  ),

  tabPanel
  (
    p(icon("table"), "Cut heatmap"),
    bsAlert("alert"),value = "cuthmmainpan",
    plotlyOutput(outputId = "cutheatmap"),
    
    
    br(),br(),br(),br(),br(),br(),br(),br(),br(),
    br(),br(),br(),br(),br(),br(),br(),br(),br(),
    br(),br()
    
    #verbatimTextOutput("event")
  )
  ))
)
