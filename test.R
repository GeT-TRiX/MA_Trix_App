###################################
##################################
##                              ##
##  Shiny application            ##
##################################
##                              ##
##  Author: Franck Soubès        ##
##################################<
###################################


#source("plotHeatmaps.r")
source("compat.R")
source("formating.R")
source("global.R")
#source("PCA.R")


# library(shiny)
# library(BH)
# library(rCharts)
# require(markdown)
# require(data.table)
# library(dplyr)
# library(DT)

#shinyUI(

options(shiny.maxRequestSize = 40 * 1024 ^ 2) # Defined the maximum size in Mb that R can load for one file

ui <- navbarPage(
  
  "MA_Trix_App",
  
  theme = shinytheme("united"),
  
  # multi-page user-interface that includes a navigation bar.
  
  tabPanel(
    "Explore the Data",
    sidebarPanel(
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
      ),
      style = " font-size:100%; font-family:Arial;
      border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
      #tags$style("#myNumericInput {font-size:10px;height:10px;}"),
      
      downloadButton("save", "Save your plot"),
      br(),
      
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 15),
      br(),
      
      # numericInput('key', 'Cluster count', 1,
      #              min = 0, max = 2),
      
      
      sliderInput(
        "pval",
        "P-value:",
        min = 0.01,
        max = 0.05,
        value = 0.05,
        step = 0.01
      ),
      
      br(),
      
      selectInput(
        "dist",
        "Choose your matrix distance",
        choices = c("cor", "euclidian")
      ),
      
      checkboxInput("somevalue", "Add Mean for the different", FALSE),
      verbatimTextOutput("value"),
      br(),
      
      selectInput("form", "Choose your file format",
                  choices = c("png", "eps")),
      br(),
      
      actionButton("first", "Print Heatmap", style =
                     "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      
      wellPanel(
        uiOutput("individusel")
        ,
        actionButton(
          inputId = "allIndividus",
          label = "Select all",
          icon = icon("check-square-o")
        )
        ,
        actionButton(
          inputId = "noIndividus",
          label = "Clear selection",
          icon = icon("square-o")
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
          icon = icon("check-square-o")
        )
        ,
        actionButton(
          inputId = "noTests",
          label = "Clear selection",
          icon = icon("square-o")
        )
        ,
        
        p("You've selectionned the following test : "),
        hr(),
        verbatimTextOutput("test")
        
      )
    ),
    
    mainPanel(tabsetPanel(
      tabPanel(
        p(icon("line-chart"), "Visualize the Heatmap"),
        tags$style(
          type = "text/css",
          ".shiny-output-error { visibility: hidden; }",
          ".shiny-output-error:before { visibility: hidden; }"
        ),
   
        
        bsAlert("alert"), ### no more error messages
        
        
        plotOutput(outputId = "distPlot")
        
      ),
      tabPanel(
        p(icon("table"), "Dataset"),
        
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
    ))
  ),

  tabPanel(
    p(icon("search"), "PCA"),
    
    sidebarPanel(
      
      style = " font-size:100%; font-family:Arial;
      border-color: #2e6da4; background-color: #337ab7, width: 28px; ",
      width = 3 ,

      numericInput("normCount", "Count", 100),
      numericInput("normMean", "Mean", 0),
      numericInput("normSd", "Std Dev", 1)
      
    ),
    
    mainPanel(
      
      # h4("The page popped-up is the LEGO set database on Brickset.com."),
      # h4("Step 1. Please type the Set ID below and press the 'Go!' button:"),
      # textInput(inputId = "setid", label = "Input Set ID"),
      # #p('Output Set ID:'),
      # #textOutput('setid'),
      # actionButton("goButtonAdd", "Go!"),
      # h5('Output Address:'),
      # textOutput("address"),
      # p(""),
      # h4(
      #   "Step 2. Please click the button below.
      #   The link to the Set's page is being generated."
      # ),
      # p(""),
      # actionButton("goButtonDirect", "Generate Link Below!"),
      # p(""),
      # htmlOutput("inc"),
      # p(
      #   "I was supposed to show you in an iframe below. However, it only
      #   worked on localhost and has security issue after deployed to the cloud. Ooops..."
      # )
      
      )
    )
  
  , tabPanel("How to use?",
             mainPanel(
               
               includeMarkdown("help.md")
             )
  )
  
  , tabPanel("About",
           mainPanel(
             includeMarkdown("about.md")
           )
  ) 
  
  )

server <- function(input, output, session) {
  
  observeEvent(input$first, {
    
    csvf <- reactive({
      inFile <- input$file1
      
      if (is.null(inFile)) {
        createAlert(
          session,
          "alert",
          "exampleAlert",
          title = "First Step",
          content = "You need to import 3 csv files in the browser widget",
          append = FALSE
          
        )
        
        return(NULL)
      }
      
      data <- as.list(inFile$datapath)
      csvtest = list()
      name = inFile$datapath
      iscsv = grep(pattern = '.csv$', name, value = T)
      
      if (length(iscsv) == 0) {
        createAlert(
          session,
          "alert",
          "exampleAlert",
          title = "Oops Error",
          content = "Are you sure you're importing csv files ?",
          append = FALSE
        )
        return(NULL)
      }
      
      else{
        if (length(data) > 3)
        {
          createAlert(
            session,
            "alert",
            "exampleAlert",
            title = "Oops Error",
            content = "Are you sure it's the good number of files? you  have imported more than 3 files,
            you need to import 3 csv files
            Tips: Use ctrl+left click then choose your files ",
            append = FALSE
          )
          
          return (NULL)
        }
        
        else if (length(data) < 3) {
          createAlert(
            session,
            "alert",
            "exampleAlert",
            title = "Oops Error",
            content = "Are you sure it's the good number of files? you have imported less than
            3 files, you need to import 3 csv files
            Tips: Use ctrl+left click then choose your files ",
            append = FALSE
            
          )
          
          return (NULL)
        }
        
        else{
          
          for (i in 1:length(data)) {
            for (elem in input$file1[[i, 'datapath']]) {
              cat("loading file number" , i, "\n")
            }
            csvtest[i] = elem
          }
        }
        
        csv <- lapply(csvtest, read.csv2)
        csvord = list()
        
        for (i in 1:length(csv)) {
          if (colnames(csv[[i]][2]) == "Grp")
          {
            csvord[[2]] = csv[[i]]
          }
          else if (colnames(csv[[i]][10]) == "Amean")
          {
            csvord[[3]] = csv[[i]]
            
          }
          else{
            csvord[[1]] = csv[[i]]
          }
        }
        
        row.names(csvord[[1]]) = csvord[[1]]$X
      }
      
      return (csvord)
      
    })
    
    
    output$individusel <- renderUI(
      checkboxGroupInput(
        inputId = "indiv" ,
        label =  "Choose your samples:",
        choices =  colnames(csvf()[[1]][, -1]),
        selected = colnames(csvf()[[1]][, -1])
        
      )
    )
    
    output$value <- renderText({
      input$somevalue
    })
    
    mean_grp <- reactive({
      return(output$value)
    })
    
    output$testout <- renderUI(
      checkboxGroupInput(
        inputId = "test" ,
        label =  "Choose Option:",
        choices =  colnames(adjusted()[,-1]),
        selected = colnames(adjusted()[, -1])

      )
    )
    
    # observerEvent(input$mean ,{
    #
    #   checkboxInput("mean", "Do you want to mean your genes ?", value = FALSE, width = NULL)
    # })
    
    observeEvent(input$allIndividus, {
      updateCheckboxGroupInput(
        session,
        "indiv",
        label = "Choix des individus",
        choices = colnames(csvf()[[1]][, -1]),
        selected = colnames(csvf()[[1]][, -1])
      )
    })
    
    observeEvent(input$noIndividus, {
      updateCheckboxGroupInput(session,
                               "indiv",
                               label = "Choix des individus",
                               choices = colnames(csvf()[[1]][, -1]))
    })
    
    choix_individus <- reactive({
      return(input$indiv)
    })
    
    list_ind <- reactive({
      return(list(input$indiv))
    })
    
    output$indiv <-  renderText({
      choix_individus()
    })
    
    choix_test <- reactive({
      return(input$test)
    })
    
    output$test <- renderText({
      choix_test()
    })
    
    adjusted <- reactive({
      
      df <- csvf()
      if (is.null(df))
        return(NULL)
      adj = csvf()[[3]][, grep(
        #"^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|X|adj.P.Val_LKO_CTRL.LWT_CTRL",
        "X|adj.P.Val",
        names(csvf()[[3]]),
        value = TRUE
      )]
      return(adj)
      
    })
    
    
    observeEvent(input$allTests, {
      updateCheckboxGroupInput(
        session,
        "test",
        label = "Choix des individus",
        choices = colnames(adjusted()[,-1]),
        selected = colnames(adjusted()[,-1])
      )
    })
    
    observeEvent(input$noTests, {
      updateCheckboxGroupInput(session,
                               "test",
                               label = "Choix des individus",
                               choices = colnames(adjusted()[, -1]))
    })
    
    
    formated <- reactive({
      df <- csvf()
      if (is.null(df))
        return(NULL)
      adj = csvf()[[3]][, grep(
        "^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|X|adj.P.Val_LKO_CTRL.LWT_CTRL",
        #"X|adj.P.Val",
        names(csvf()[[3]]),
        value = TRUE
      )]

      treated = formating(adjusted(), csvf()[[1]], input$pval)
      return(treated)
      
    })
    
    #observeEvent(input$first, {
    myData <- reactive({
      df <- csvf()
      if (is.null(df))
        return(NULL)
      return(df[[1]])
      
    })
    
    
    output$distPlot <- renderPlot({
      plotHeatmaps(
        data.matrix(new_data()),
        formated()[[1]],
        new_group()$Grp,
        workingPath = wd_path,
        prefix,
        suffix,
        k = input$clusters,
        Rowdistfun = input$dist ,
        Coldistfun = input$dist,
        keysize = input$key,
        meanGrp = input$somevalue
        
      )
      
    }, width = 900 , height = 1200, res = 100)
    
    new_data <- reactive(subset(csvf()[[1]],
                                select = choix_individus()))
    
    new_test <- reactive(subset(adjusted(),
                                select = choix_test()))
    
    # new_group <- reactive( csvf()[[2]] %>%
    #                          filter( X ==  list_ind()))
    
    #selection = list("LWT_Ctrl2","LWT_MCD5")
    
    selection = reactive({
      test = list(choix_individus())
      return (as.character(test))
    })
    
    
    new_group <- reactive(csvf()[[2]][csvf()[[2]]$X %in% choix_individus(), ])
    
    output$new_test <- renderDataTable(new_test())
    
    output$new_data <- renderDataTable(new_data())
    
    output$new_group <- renderDataTable(new_group())
    

    
    p = reactive({
      View(new_test())
      plotHeatmaps(
        data.matrix(new_data()),
        formated()[[1]],
        new_group()$Grp,
        workingPath = wd_path,
        prefix,
        suffix,
        k = input$clusters,
        Rowdistfun = input$dist ,
        Coldistfun = input$dist,
        keysize = input$key,
        meanGrp = input$somevalue
        
      )
    })
    
      output$save <- downloadHandler(
      if (input$form == "eps") {
        filename = "save.eps"
      }
      else{
        filename = "save.png"
      },
      content = function(file) {
        ggsave(
          p(),
          filename = filename,
          width = 20,
          height = 20,
          limitsize = FALSE,
          units = "cm"
          ,dpi= 70
        )
        
      }
    )
    
  })
  
  observeEvent(input$second, {
    csvf1 <- reactive({
      csvtest = list()
      
      inFile <- input$file2
      name <- inFile$name
      for (i in 1:length(data)) {
        for (elem in input$file2[[i, 'datapath']]) {
          cat("loading file number" , i, "\n")
        }
        csvtest[i] = elem
      }
      csv <- lapply(csvtest, read.csv2)
      
      return (csv)
    })
    
    output$mytable2 <- DT::renderDataTable({
      DT::datatable(csvf1(), options = list(orderClasses = TRUE))
      
    })
    
  })
  
  }

shinyApp(ui = ui , server = server)
