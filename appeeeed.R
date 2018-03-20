###################################
##################################
##                              ##
##  Shiny application            ##
##################################
##                              ##
##  Author: Franck Soubès        ##
##################################
###################################


#source("plotHeatmaps.r")
source("compat.R")
source("formating.R")
source("global.R")
#source("PCA.R")

options(shiny.maxRequestSize = 40 * 1024 ^ 2) # Defined the maximum size in Mb that R can load for one file


ui <- fluidPage(
  theme = shinytheme("united"),
  
  navbarPage(
    'Test App',
    
    headerPanel("Import data files"),
    #sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      tabsetPanel(
        id = "tabset",
        tabPanel(
          "Heatmap",
          hr(),
          
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
          hr(),
          
          numericInput('clusters', 'Cluster count', 3,
                       min = 1, max = 15),
          hr(),
          
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
          
          hr(),
          
          selectInput(
            "dist",
            "Choose your matrix distance",
            choices = c("cor", "euclidian")
          ),
          
          checkboxInput("somevalue", "Some value", FALSE),
          verbatimTextOutput("value"),
          hr(),
          
          selectInput("form", "Choose your file format",
                      choices = c("png", "eps")),
          hr(),
          
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

            p("Vous avez sélectionné les individus : "),
            hr(),
            verbatimTextOutput("indiv")
            
          )
        ),
        tabPanel(
          "PCA",
          
          numericInput("normCount", "Count", 100),
          numericInput("normMean", "Mean", 0),
          numericInput("normSd", "Std Dev", 1)
          
        ),
        
        
        tabPanel(
          "Display data",
          
          DT::dataTableOutput("mytable3"),
          
          fileInput(
            "file2",
            "Choose your csv file",
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
            ,
            multiple = T
          ),
          style = " font-size:100%; font-family:Arial;
          border-color: #2e6da4; background-color: #337ab7, width: 28px; "
          
          ,
          actionButton("second", "Print Data", style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
        )
        
      )
    ),
    
    
    # Main panel for displaying outputs ----
    
    mainPanel(
      tags$style(
        type = "text/css",
        ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
      ),
      ### no more error messages
      
      bsAlert("alert"),

      
      plotOutput(outputId = "distPlot")
      
    ),
    
    column(
      12,
      
      h3(
        "Show the actual data frame with the columns selected"
      ),
      helpText(
        "Warning according to the number of NA for a given parameter, the analysis should be strongly biased"
      )
      ,
      dataTableOutput("new_data")
    ),
    column(
      12,
      
      h3(
        "Show the actual data frame with the columns selected"
      ),
      helpText(
        "Warning according to the number of NA for a given parameter, the analysis should be strongly biased"
      )
      ,
      dataTableOutput("new_group")
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
          content = "Are you sure it's the good number of files? you  have imported less than 3 files,
          you need to import 3 files
          Tips: Use ctrl+left click then choose your files with the good order",
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
          3 files, you need to import 3 files
          Tips: Use ctrl+left click then choose your files with the good order",
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
      
    }
    
    return (csvord)
    
  })
  
  
  output$individusel <- renderUI(
    checkboxGroupInput(
      inputId = "indiv" ,
      label =  "Choose Option:",
      choices =  colnames(csvf()[[1]][,-1]
      # selected = colnames(csvf()[[1]][,-1])
      )
  ))
  
  output$value <- renderText({ input$somevalue })
  
  mean_grp <- reactive({
    return(output$value)
  })
  
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
                             choices = colnames(csvf()[[1]][,-1]))
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
  
  
  formated <- reactive({
    df <- csvf()
    if (is.null(df))
      return(NULL)
    adj = csvf()[[3]][, grep(
      "^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|X|adj.P.Val_LKO_CTRL.LWT_CTRL",
      names(csvf()[[3]]),
      value = TRUE
    )]
    treated = formating(adj, csvf()[[1]], input$pval)
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
        formated()[[2]],
        formated()[[1]],
        csvf()[[2]]$Grp,
        workingPath = wd_path,
        prefix,
        suffix,
        k = input$clusters,
        Rowdistfun = input$dist ,
        Coldistfun = input$dist,
        keysize = input$key,
        meanGrp = input$somevalue
      )
      
    }, width = 1200 , height = 800, res = 100)
    
    new_data <- reactive(subset(csvf()[[1]],
                                select = choix_individus()))
    
    new_group <- reactive( csvf()[[2]] %>%
                             filter( X ==  list_ind()))
    
    
    output$new_data <- renderDataTable(new_data())
    
    output$new_group <- renderDataTable(new_group())
    
    # nb_na <-
    #   reactive(#----- if data selected is not null (nrow(new_data())) stations(length(choix_stations)) and parametres(length(choix_parametres)) are selected
    #     if (length(choix_individus()) >= 1 &
    #            nrow(new_data()) > 0) {
    #       na <-
    #         aggregate(new_data(), list(new_data_with_station()$NOM_SITE), function(x)
    #           sum(is.na(x)))
    #       colnames(na) <-
    #         c("NOM_SITE", choix_parametres()) # change the name of the first column
    #       return(na)
    #     })
    
    #output$nb_na <- renderDataTable(nb_na())
    
    p = reactive(
      plotHeatmaps(
        formated()[[2]],
        formated()[[1]],
        csvf()[[2]]$Grp,
        workingPath = wd_path,
        prefix,
        suffix,
        k = input$clusters
      )
    )
    
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
          filename = file,
          width = 1920,
          height = 1080,
          units = "in"
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
