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
    "MaTrix App",# MA for microarray and Trix for the name of the team
    
    #useShinyjs(),
    theme = shinytheme("united"),

    # multi-page user-interface that includes a navigation bar.
    #navbarMenu("menu",
    source(file.path("ui", "tab1.R"), local = TRUE)$value,
    source(file.path("ui", "tab4.R"), local = TRUE)$value,
    source(file.path("ui", "tab3.R"), local = TRUE)$value,
    source(file.path("ui", "tab2.R"), local = TRUE)$value,
    #),
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