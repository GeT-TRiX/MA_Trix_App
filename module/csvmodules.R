#' ### Author: Franck Soubès
#' ### Bioinformatics Master Degree - University of Bordeaux, France
#' ### Link: https://github.com/fsoubes/MA_Trix_App
#' ### Where: GET-TRiX's facility
#' ### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
#' ### Licence: GPL-3.0


#' csvFileInput is a shiny widget for uploading data locally
#'
#' @param id Shiny id
#' @param label Shiny label
#' @param multiple Shiny boolean for selecting or not multiple files in the browser
#'
#' @return Widget in the gui
#'
#' @export
#'


csvFileInput <- function(id, label = "CSV file", multiple = T) {
  ns <- NS(id)
  fileInput(ns("file"),multiple = T, accept = c("text/csv","text/comma-separated-values,text/plain",".csv"), label)

}

#' csvIdentifier is a shiny widget for loading data from the server
#'
#' @param id Shiny id
#' @param label Shiny label
#'
#' @return Widget in the gui
#'
#' @export
#'


csvIdentifier <- function(id, label = "Unique Identifier") {
  ns <- NS(id)
  selectInput(ns("identifier"), label, choices = c("ProbeName", "Transcript"))

}


#' csvDecimal is a shiny widget to select the decimal separator
#'
#' @param id Shiny id
#' @param label Shiny label
#'
#' @return Widget in the gui
#'
#' @export
#'

csvDecimal <- function(id, label="Decimal"){
  ns <- NS(id)
  radioButtons(ns("csvdec"), label ,
               choices = c(Comma=',', Point='.' ), selected = ',')
}


#' ordinput is a function which aims to reorder an input list of dataframes
#'
#' @param csvnord A list of dataframe
#' @param identifier Character identifier (probes/transcripts)
#' @param dec Character to select the decimal separator
#'
#' @return An ordered list (WorkingSet, pData, restable)
#' @export
#'


ordinput <- function(csvnord, identifier, dec){

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
        dec = dec
      )
  )

  csvord = list()
  for (i in 1:length(csv)) {
    if (length(csv[[i]]) == 2) {
      csvord[[2]] <- csv[[i]]
      colnames(csvord[[2]]) <- c("X", "Grp")
    }
    else if (any(grepl("^adj.P.Val|^FDR|^padj_" , colnames(csv[[i]])))){
      csvord[[3]] <- csv[[i]]
      if(!any(grepl("GeneName", colnames(csvord[[3]]))))
        colnames(csvord[[3]])[[2]] <- "GeneName"
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
  csvord[[1]][1] <- csvord[[3]][1]

  return(csvord)
}

#' dirModuleUI is a shiny widget for loading data from the server
#'
#' @param id Shiny id
#' @param multiple Shiny boolean for selecting or not multiple files in the browser
#'
#' @return Widget in the gui
#'
#' @export
#'

dirModuleUI = function(id, multiple =T) {
  ns = NS(id)

  fluidPage(
    fluidRow(
      shinyFilesButton(ns('files'), label='File select', title='Please select all the files', multiple=T)
    )
  )
}



#' csvFile is shiny module which aims is to alert users on the status of his upload and return an ordered list of dataframe
#'
#' @param input Internal
#' @param output Internal
#' @param session Internal
#' @param stringsAsFactors
#'
#' @return The reactive that yields a list of dataframes
#'
#' @export
#'
#'

csvFile <- function(input, output, session, stringsAsFactors) {

  root = c(data = root)
  shinyFileChoose(input, 'files', roots = root, session = session,filetype=c("csv"))
  shinyDirChoose(input, "directory", roots = root, session = session)
  shinyFileSave(input, "fileSave", roots = root, session = session)


  showmark <- T # Boolean uses to hide or show the mardkwon serving to load data

  isuploading <- F

  #' Reactive function returned to the tab1.R
  #'
  #' @return  a reactive value of type boolean corresponding to the loading status by default it is set to True
  #'

  output$boolmark <- reactive({
    showmark
  })

  observe({
    print(showmark)
  })


  outputOptions(output,"boolmark",suspendWhenHidden=F)

  csvlocpath <- eventReactive(input$files, { print(parseFilePaths(root, input$files)$datapath)})


  csvf <- reactive({

    isuploading = ifelse( (length(parseFilePaths(root, input$files)$datapath) !=0) || !is.null(input$file)  , T, F)
    validate(need(isuploading == T, "You need to import the data"))
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

      csvord <- ordinput(csvnord, input$identifier, input$csvdec)


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

    csvord <- ordinput(csvnord, input$identifier, input$csvdec)


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
