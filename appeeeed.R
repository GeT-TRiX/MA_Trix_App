  #################################
  #################################
  ##                              #
  ## Shiny application            #
  #################################
  ##                              #
  ## Author: Franck Soubès        #
  #################################
  #################################
    
  
  #source("plotHeatmaps.r")
  source("compat.R")
  source("formating.R")
  source("global.R")
  #source("PCA.R")
  
  
  options(shiny.maxRequestSize=40*1024^2) # defined the maximum size in Mb that R can load for one file
  
  
  ui <- fluidPage(theme = shinytheme("united"),
                  
                  headerPanel("Import data files"),
                  sidebarLayout(
                    
                    sidebarPanel(width = 3, 
                                 
                    tabsetPanel(id = "tabset",
                               tabPanel("Heatmap",
                    
                    fileInput("file1","Choose your csv file",accept = c(
                      "text/csv",
                      "text/comma-separated-values,text/plain",
                      ".csv")
                    , multiple = T), style=" font-size:100%; font-family:Arial; 
                    border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
                    #tags$style("#myNumericInput {font-size:10px;height:10px;}"),
                    
                    downloadButton("save", "save your plot"),
                    hr(),
                      
                    numericInput('clusters', 'Cluster count', 3,
                                   min = 1, max = 15),
                    hr(),
                    
                    sliderInput("pval", "P-value:",
                                min = 0.01, max = 0.05,
                                value = 0.05, step = 0.01),
                    hr(),
                    
                    selectInput("dist", "Choix de la distance", 
                    choices = c("cor","euclidian")),
                    hr(),
                              
                    actionButton("first", "Print Heatmap",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    
                    wellPanel(uiOutput("individusel")
                            
                    
                    )),
                    tabPanel("PCA",
                             
                             numericInput("normCount", "Count", 100),
                             numericInput("normMean", "Mean", 0),
                             numericInput("normSd", "Std Dev", 1)
                    
                    ),

                   
                    tabPanel("Display data", 
                             
                             DT::dataTableOutput("mytable3"),
                             
                             fileInput("file2","Choose your csv file",accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")
                               , multiple = T), style=" font-size:100%; font-family:Arial;
                    border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
                             
                            actionButton("second", "Print Data",style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                  )),
                  
                      # conditionalPanel(
                      # 'input.dataset === "atom"',
                      # helpText("Click the column header to sort a column."))),
  
                  # Main panel for displaying outputs ----
                  mainPanel(
                    
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"), ### no more error messages
                    
                    bsAlert("alert"),
                    
                    # Output: Plot
                    #fluidRow(
                    plotOutput(outputId = "distPlot")
                    
                    #tabPanel("seq", DT::dataTableOutput("mytable3"))
                    #br(),br(),br(),br(),br()
                    #   tabsetPanel(
                    #     id = 'dataset',
                    
                      )
                  )
                    
                  
  )
  
  server <- function(input, output,session) {
    
    observeEvent(input$first, {
        
  
        csvf <- reactive({
          
          inFile <- input$file1
          name <- inFile$name
          
          if(is.null(inFile)){
            
            createAlert(session, "alert", "exampleAlert", title = "Oops Error",
                       content = "You need to import 3 files in the browser widget", append = FALSE)
            return(NULL)
          }
          
          data <- as.list(inFile$datapath)
          csvtest = list()
          
          if( length(data) > 3)
           {
            createAlert(session, "alert", "exampleAlert", title = "Oops Error",
                         content = "Are you sure it's the good number of files? you  have imported less than 3 files, 
                        you need to import 3 files
                        Tips: Use ctrl+left click then choose your files with the good order", append = FALSE)
            
            return (NULL)
          }
          
          else if (length(data) < 3) {
            createAlert(session, "alert", "exampleAlert", title = "Oops Error",
                        content = "Are you sure it's the good number of files? you have imported less than
                        3 files, you need to import 3 files
                        Tips: Use ctrl+left click then choose your files with the good order", append = FALSE)
          
          return (NULL) }
          
          else{
            
          for (i in 1:length(data)){
            
            for (elem in input$file1[[i, 'datapath']]){
              
              cat("loading file number" ,i, "\n")
            }
            csvtest[i] = elem
          }
          csv <- lapply(csvtest, read.csv2)
  
          output$individusel <- renderUI({
            selectInput("variable1", "Choose Option:", colnames(csv[[1]])) 
          })
          }
          return (csv)
     
        }) 
        
      formated <- reactive({
      df <- csvf()
      if(is.null(df)) return(NULL)
      adj = csvf()[[3]][,grep("^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|X|adj.P.Val_LKO_CTRL.LWT_CTRL", names(csvf()[[3]]), value=TRUE)]
      treated = formating(adj,csvf()[[1]],input$pval)
      return(treated)
      })
    
      myData <- reactive({
        df <- csvf()
        if(is.null(df)) return(NULL)
        return(df[[1]])
        
      })  
      
      
      output$distPlot <- renderPlot({
        
        plotHeatmaps(formated()[[2]],formated()[[1]],csvf()[[2]]$Grp,workingPath=wd_path,prefix,suffix,
                     k=input$clusters, Rowdistfun=input$dist ,Coldistfun= input$dist)
        
      },width = 1200 , height = 800, res = 100)
      
      p = reactive( plotHeatmaps(formated()[[2]],formated()[[1]],csvf()[[2]]$Grp,workingPath=wd_path,prefix,suffix,k=input$clusters))
      
        output$save <- downloadHandler(
        filename = "save.png" ,
          content = function(file) {
        ggsave(p(), filename = file)
            
       })
        
    })
    
    observeEvent(input$second, {
      
      csvf1 <- reactive({

        inFile <- input$file2
        name <- inFile$name
        for (i in 1:length(data)){

          for (elem in input$file2[[i, 'datapath']]){

            cat("loading file number" ,i, "\n")
          }
          csvtest[i] = elem
        }
        csv <- lapply(csvtest, read.csv2)

      return (csv)
      })
      
      
    output$mytable3 <- DT::renderDataTable({
      
      DT::datatable(csvf1()[[3]], options = list(orderClasses = TRUE))
      
      })
    
    })
  }
  

  shinyApp(ui = ui , server = server)
  
  
