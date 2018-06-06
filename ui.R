##################################
##################################
##                              ##
## Shiny app/UI part            ##
##################################
##                              ##
## Author: Franck Soubès        ##
##################################
##################################

#jsCode <- "$('div.dataTables_length select').append( '<option value='500'>500</option>' );"

options(shiny.maxRequestSize = 70 * 1024 ^ 2) # defined the maximum size in Mb that R can load for one file

######################################
######## Shiny UI handler            #
######################################

shinyUI(
  ui <-bootstrapPage(
    #extendShinyjs(text = jsCode),
    inlineCSS(appCSS),
    div(id = "loading-content-bar",
        p()),
    div(
      id = "loading-content",
      br(),
      br(),
      br(),
      h2("Please wait MATRiX app is loading...")),
    
      # Create a Shiny UI page that loads the CSS and JavaScript for Bootstrap
      navbarPage(
        # divided between the differnt tabPanel
        "MATRiX App",
        # MA for microarray and Trix for the name of the team
        id = "matrixapp",
        #position = "fixed-top",
        theme = shinytheme("united"),
        
        ###############################
        ######## GUI for loading data #
        ###############################
        
        source(file.path("ui", "tab1.R"), local = TRUE)$value, # loading data
        
        ################################
        ######## GUI for plotting Venn #
        ################################
        
        source(file.path("ui", "tab4.R"), local = TRUE)$value, # ploting Venn
        
        ###############################
        ######## GUI for plotting PCA #
        ###############################
        
        source(file.path("ui", "tab3.R"), local = TRUE)$value, # ploting PCA
        
        ###############################
        ######## GUI for plotting hm  #
        ###############################
        
        source(file.path("ui", "tab2.R"), local = TRUE)$value, # ploting Heatmap and cutheatmap
        
        
        tabPanel(p(icon("info-circle"),
                   "About"),
                 mainPanel(includeMarkdown(
                   "markdown/about.md"
                 )), 
             br(), br(), br(),
             br(), br(), br(),
             br(), br(), br(),
             br(), br(), 
             actionLink("session",
                        "Print version information about R, the OS and attached or loaded packages."),
             br(), br(), br(),
             DT::dataTableOutput("sessinfo")
             #htmlOutput("SessionInfo")
    )
    
  )
))
