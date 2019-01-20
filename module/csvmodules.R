#' ### Author: Franck Soubès
#' ### Bioinformatics Master Degree - University of Bordeaux, France
#' ### Link: https://github.com/fsoubes/MA_Trix_App
#' ### Where: GET-TRiX's facility
#' ### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
#' ### Licence: GPL-3.0



csvFileInput <- function(id, label = "CSV file") {
  ns <- NS(id)
  fileInput(ns("file"),multiple = T, accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), label)

}

csvIdentifier <- function(id, label = "Unique Identifier") {
  ns <- NS(id)
  selectInput(ns("identifier"), label, choices = c("ProbeName", "Transcript"))
  
}

ordinput <- function(csvnord, identifier){
  
  csv <- lapply(
    csvnord,
    
    #' apply the fread method for each element in the csvnord list
    #'
    #' @return \csv a data frame object
    #'
    FUN = function (x)
      
      fread(
        x,
        data.table = F,
        check.names = F,
        header = T,
        sep = ";",
        dec = ","
      )
  )
  
  csvord = list()
  for (i in 1:length(csv)) {
    if (colnames(csv[[i]][2]) == "Grp") {
      csvord[[2]] <- csv[[i]]
      
    }
    else if (any(grepl("adj.P.Val" , colnames(csv[[i]]))))
    {
      csvord[[3]] <- csv[[i]]
      
    }
    else
      csvord[[1]] <- csv[[i]]
  }
  
  csvord[[2]] <- chartofa(csvord[[2]]) # transform dataframe containing characters to factors
  
  
  if(class(csvord[[3]][[1]]) == "integer"){
    csvord[[3]] <- csvord[[3]][-1] #remove useless column
    csvord[[3]] <- csvord[[3]] %>% 
      select(ProbeName,  everything())
  }
  
  colnames(csvord[[3]])[[1]] <- identifier
  colnames(csvord[[1]])[[1]] <- identifier
  colnames(csvord[[2]])[1] <- "X"
  csvord[[1]][1] <- csvord[[3]][1]
  
  return(csvord)
  
}

dirModuleUI = function(id) {
  ns = NS(id)
  
  fluidPage(
    fluidRow(
      shinyFilesButton(ns('files'), label='File select', title='Please select all the files', multiple=T)
    )
  )
}


# Module server function
csvFile <- function(input, output, session, stringsAsFactors) {

  #root = c(data = "//home/fsoubes/MA_Trix_App/data")
  root = c(data = "//home/franck1337/MA_Trix_App/data")
  shinyFileChoose(input, 'files', roots = root, session = session,filetype=c("csv"))
  shinyDirChoose(input, "directory", roots = root, session = session)
  shinyFileSave(input, "fileSave", roots = root, session = session)


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

  csvlocpath <- eventReactive(input$files, { print(parseFilePaths(root, input$files)$datapath)})

  #' Reactive function in the aim of loading csv files
  #'
  #' @param inFile loaded files
  #'
  #' @return \csvf a reactive value of type list containing three data frames toptable and workingset and the pData
  #'
  
  csvf <- reactive({

    
    inFile <- input$file
    if(!is.null(inFile)){

    data <- as.list(inFile$datapath)
    csvnord = list()
    name = inFile$datapath
    iscsv = grep(pattern = '.csv$', name, value = T)

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
          csvnord[i] <- elem
        }
      }

      csvord <- ordinput(csvnord, input$identifier)


      observe({showmark <<-F
      print(showmark)
      }) # modify and lock the bool value to false

      output$boolmark <- reactive({
        showmark
      })
    
    simil <- isimilar(csvord[[3]], csvord[[2]], csvord[[1]])
    
    if(!simil[[1]] ||!simil[[2]]){
      
      createAlert(
        session,
        "alert",
        "exampleAlert",
        style = "danger",
        title = "Oops Error",
        content = "Identifier error not the same column",
        append = FALSE
        
      )
      return(NULL)
    }
    
    
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
    csvord[[4]] <- inFile
    print(inFile$name)

    return (csvord)
  }


  else{

    req(length(csvlocpath())==3)

    csvnord <- list()
    csvnord[1] <-csvlocpath()[[1]]
    csvnord[2] <-csvlocpath()[[2]]
    csvnord[3] <-csvlocpath()[[3]]

    csvord <- ordinput(csvnord, input$identifier)
    
    
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
    csvord[[4]] <- csvlocpath()
    return (csvord)

   }
  })

}



