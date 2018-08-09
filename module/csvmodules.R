### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


chartofa = function(datach){
  
  datach[] <- lapply( datach, factor)
  col_names <- names(datach)
  datach[col_names] <- lapply(datach[col_names] , factor)
  
  return(datach)
}



dataTabUI <- function(id, input, output) {
  ns <- NS(id)
  
  tagList(sidebarLayout(sidebarPanel(input),
                        
                        mainPanel(dataTableOutput(output))))
  
}

plotTabUI <- function(id, input, output) {
  ns <- NS(id)
  
  tagList(sidebarLayout(sidebarPanel(input),
                        
                        mainPanel(plotOutput(output))))
  
}


csvFileInput <- function(id, label = "CSV file") {
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
  
  showmark <<- T # Boolean uses to hide or show the mardkwon serving to load data
  
  userFile <- reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    
  })
  
 
  
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
    
    req(userFile())
    
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
      
      csv <- lapply(
        csvtest,
        

        FUN = function (x)

          fread(data.table = F,check.names = F,
            header = T,sep = ";",dec = ","
          ) 
      )
      
      csvord = list()
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
    
    observe({
    showmark <<-F
    print(showmark)
    }) # modify and lock the bool value to false
    
    output$boolmark <- reactive({
      showmark
    })
    
    
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

    return (csvord)
  })
  
  
}
