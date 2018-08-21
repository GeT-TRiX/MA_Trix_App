dirModule = function(input, output, session, fileRoot = NULL,stringsAsFactors = FALSE) {
  
  root = c(data = "/home/franck/MA_Trix_App/data")
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
  
  
  
  test <- eventReactive(input$files, { print(parseFilePaths(root, input$files)$datapath)})
  
  csvf <- eventReactive(input$files,{
    
    
    req(test())
    
    
    csvtest <- list()
    csvtest[1] <-test()[[1]]
    csvtest[2] <-test()[[2]]
    csvtest[3] <-test()[[3]]
    
    print(csvtest)
    
    #iscsv = grep(pattern = '.csv$', name, value = T)
    
    
    #csv <- lapply(csvtest, read.csv2, check.names = F) # benchmark read.csv wrapper
    
    csv <- lapply(
      csvtest,
      
      #' apply the fread method for each element in the csvtest list
      #'
      #' @return list of data frame objects
      #'
      #' @export
      
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
    
    
    observe({showmark <<-print(showmark)}) # modify and lock the bool value to false
    
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
    csvord[[4]] <- test()
    
    
    return (csvord)
    
  })
  
  
}

dirModuleUI = function(id) {
  ns = NS(id)
  
  fluidPage(
    fluidRow(
      shinyFilesButton(ns('files'), label='File select', title='Please select a file', multiple=T),
      shinyDirButton(ns("directory"), label="Directory select", title = "Select directory"),
      shinySaveButton(ns("fileSave"), label = "File save", title = "Save file as", filetype=list(text='txt'))
    )
  )
}