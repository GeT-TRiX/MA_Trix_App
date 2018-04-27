##################################
##################################
##                              ##
## Shiny application/UI part    ##
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
shinyUI(ui <- bootstrapPage( # Create a Shiny UI page that loads the CSS and JavaScript for Bootstrap
  navbarPage( # divided between the differnt tabPanel
    "MaTrix App",# MA for microarray and Trix for the name of the team
    
    #useShinyjs(),
    theme = shinytheme("united"),

    source(file.path("ui", "tab1.R"), local = TRUE)$value, # loading data
    source(file.path("ui", "tab4.R"), local = TRUE)$value, # ploting Venn
    source(file.path("ui", "tab3.R"), local = TRUE)$value, # ploting PCA
    source(file.path("ui", "tab2.R"), local = TRUE)$value, # ploting Heatmap and cutheatmap
    
    tabPanel(p(icon("info-circle"),
               "About"),
             mainPanel(includeMarkdown(
               "markdown/about.md"
             )))
    
  )
))