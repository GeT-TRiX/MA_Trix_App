# Tab module
# This module creates new tab which renders dataTable


chartofa = function(datach){
  
  datach[] <- lapply( datach, factor)
  col_names <- names(datach)
  datach[col_names] <- lapply(datach[col_names] , factor)
  
  return(datach)
}



dataTabUI <- function(id, input, output) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(sidebarLayout(sidebarPanel(input),
                        
                        mainPanel(dataTableOutput(output))))
  
}

# Tab module
# This module creates new tab which renders plot
plotTabUI <- function(id, input, output) {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(sidebarLayout(sidebarPanel(input),
                        
                        mainPanel(plotOutput(output))))
  
}

dataTab <- function(input, output, session) {
  # do nothing...
  # Should there be some logic?
  
  
}

# File input module
# This module takes as input csv file and outputs dataframe
# Module UI function
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"),multiple = T, label),
    checkboxInput(ns("heading"), "Has heading"),
    selectInput(
      ns("quote"),
      "Quote",
      c(
        "None" = "",
        "Double quote" = "\"",
        "Single quote" = "'"
      )
    )
  )
}

# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  showmark <- T # Boolean uses to hide or show the mardkwon serving to load data
  
  
  #' Reactive function returned to the tab1.R 
  #'
  #' @return \showmark a reactive value of type boolean corresponding to the loading status by default it is set to True
  #'
  
  output$boolmark <- reactive({
    showmark
  })
  
  observe({
    print(showmark)
  })
  
  
  
  outputOptions(output,"boolmark",suspendWhenHidden=F) 
  
  #' Reactive function in the aim of loading csv files
  #'
  #' @param inFile loaded files
  #'
  #' @return \csvf a reactive value of type list containing three data frames toptable and workingset and the pData 
  #'
  csvf <- reactive({
    
    
    inFile <- input$file
    if (is.null(inFile)) {
      createAlert(
        session,
        "alert",
        style = "info",
        "entryalert",
        title = "First Step",
        content = "You need to import 3 csv files in the browser widget",
        dismiss = FALSE
        
      )
      
      #Sys.sleep(1.5)
      closeAlert(session, "entryalert")
      return(NULL)
    }
    
    #req(input$file)
    print(inFile)
    
    
    
    data <- as.list(inFile$datapath)
    csvtest = list()
    name = inFile$datapath
    iscsv = grep(pattern = '.csv$', name, value = T)
    
    if (length(iscsv) == 0) {
      createAlert(
        session,
        "alert",
        "exampleAlert",
        style = "danger",
        title = "Oops Error",
        content = "Are you sure you're importing csv files ?",
        append = FALSE
      )
      #return(NULL)
      return()
    }
    
    else{
      if (length(data) > 3)
      {
        createAlert(
          session,
          "alert",
          "exampleAlert",
          style = "danger",
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
          style = "danger",
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
          for (elem in input$file[[i, 'datapath']]) {
            cat("loading file number" , i, "\n")
          }
          csvtest[i] = elem
        }
      }
      
      #csv <- lapply(csvtest, read.csv2, check.names = F) # benchmark read.csv wrapper
      
      csv <- lapply(
        csvtest,
        
        #' apply the fread method for each element in the csvtest list
        #'
        #' @return \csv a data frame object
        #'
        FUN = function (x)
          
          # read.table( # benchmark read.table
          #   x,
          #   sep = ";" ,
          #   dec = ",",
          #   header = T,
          #   check.names = F # good col names
          # )
          
          fread(
            x,
            data.table = F,
            check.names = F,
            header = T,
            sep = ";",
            dec = ","
          ) #benchmark fread memory speed
      )
      
      csvord = list()
      print("ok")
      for (i in 1:length(csv)) {
        if (colnames(csv[[i]][2]) == "Grp") {
          csvord[[2]] = csv[[i]]
          
        }
        else if (any(grepl("adj.P.Val" , colnames(csv[[i]]))))
        {
          csvord[[3]] = csv[[i]]
          
        }
        else
          csvord[[1]] = csv[[i]]
      }
      
      csvord[[2]] = chartofa(csvord[[2]]) # transform dataframe containing characters to factors
      row.names(csvord[[1]]) = csvord[[1]][, 1]
      colnames(csvord[[3]])[1] = "X"
      colnames(csvord[[2]])[1] = "X"
      
    }
    
    observe({showmark <<-F
    print(showmark)
    }) # modify and lock the bool value to false
    
    output$boolmark <- reactive({
      showmark
    })
    
    
    
    #' Reactive function returned to the tab1.R 
    #'
    #' @return \showmark a reactive value of type boolean set to False
    #'
    
    
    
    createAlert(
      session,
      "alert",
      "succeeded",
      style = "success",
      title = "Sucess",
      content = " Your files have been loaded, you can choose your data now",
      append = FALSE
      
    )
    
    Sys.sleep(1)
    closeAlert(session, "succeeded")
    View(csvord[[1]])
    
    return (csvord)
  })
  

}
basicPlotUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("controls"))
  
}
# Functionality for dataselection for plot
# SelectInput is rendered dynamically based on data

basicPlot <- function(input, output, session, data) {
  output$controls <- renderUI({
    ns <- session$ns
    selectInput(ns("col"), "Columns", names(data), multiple = TRUE)
  })
  return(reactive({
    validate(need(input$col, FALSE))
    data[, input$col]
  }))
}

##################################################################################
# Here starts main program. Lines above can be sourced: source("path-to-module.R")
##################################################################################
options(shiny.maxRequestSize=2000000000)
library(shiny)
library(data.table)
library(shinyBS)

ui <- shinyUI(navbarPage(
  "My Application",
  tabPanel("File upload", dataTabUI(
    "tab1",
    csvFileInput("datafile", "User data (.csv format)"),
    "table"
  )),
  tabPanel("Plot", plotTabUI(
    "tab2", basicPlotUI("plot1"), "plotOutput"
  ))
  
))


server <- function(input, output, session) {
  datafile <- callModule(csvFile, "datafile",
                         stringsAsFactors = FALSE)
  
  output$table <- renderDataTable({
    req(datafile())
    datafile()
    View(datafile()[[1]])
  })
  
  plotData <- callModule(basicPlot, "plot1", datafile())
  
  output$plotOutput <- renderPlot({
    plot(plotData())
  })
}


shinyApp(ui, server)

#retest with https://stackoverflow.com/questions/27080089/how-to-organize-large-shiny-apps/27122115