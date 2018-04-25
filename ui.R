##################################
##################################
##                              ##
## Shiny application            ##
##################################
##                              ##
## Author: Franck Soubès        ##
##################################
##################################

source("function/compat.R")
source("function/formating.R")
source("environnement/global.R")
source("function/PCA.R")



options(shiny.maxRequestSize = 70 * 1024 ^ 2) # defined the maximum size in Mb that R can load for one file
shinyUI(ui <- bootstrapPage(
  navbarPage(
    "MaTrix App",
    # MA for microarray and Trix for the name of the team
    
    #useShinyjs(),
    theme = shinytheme("united"),
    
    # multi-page user-interface that includes a navigation bar.
    
    source(file.path("ui", "tab1.R"), local = TRUE)$value,
    source(file.path("ui", "tab4.R"), local = TRUE)$value,
    source(file.path("ui", "tab3.R"), local = TRUE)$value,
    source(file.path("ui", "tab2.R"), local = TRUE)$value,
    
    
    tabPanel(
      "Cut heatmap",
      titlePanel("Cutheatmap settings"),
      sidebarPanel(
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

          br(),
          selectInput("formcut", "Choose your file format",
                      choices = c("png", "eps", "emf")),
          
          shiny::actionButton("cutheat", "Print Venn diagram", style =
                                "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          downloadButton('downloadcut',"Download the data", 
                         style =
                           "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
          downloadButton("savecut", "Save your plot" , style =
                           "color: #fff; background-color: #337ab7; border-color: #2e6da4")
          
        )
      ),
      
      mainPanel(
        bsAlert("alert"),
        plotlyOutput(outputId = "cutheatmap"),
        br(),br(),br(),br(),br(),br(),br(),br(),br(),
        br(),br(),br(),br(),br(),br(),br(),br(),br(),
        br(),br(),
        verbatimTextOutput("event")
        
      )
      
    ),
    
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    #source(file.path("ui", "markdown.R"), local = TRUE)$value
    
    tabPanel(p(icon("question-circle"),
               "How to use?"),
             mainPanel(includeMarkdown("markdown/help.md"))),
    tabPanel(p(icon("info-circle"),
               "About"),
             mainPanel(includeMarkdown(
               "markdown/about.md"
             )))
    
  )
))