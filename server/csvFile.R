csvFile <- function(input,output,session){


csvf <- reactive({
  inFile <- input$file1
  
  if (is.null(inFile)) {
    createAlert(
      session,
      "alert",
      style = "info",
      "entryalert",
      title = "First Step",
      content = "You need to import 3 csv files in the browser widget",
      dismiss = FALSE
      #append = TRUE
      
    )
    Sys.sleep(2.5)
    
    closeAlert(session, "entryalert")
    
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
      style = "danger",
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
        for (elem in input$file1[[i, 'datapath']]) {
          cat("loading file number" , i, "\n")
        }
        csvtest[i] = elem
      }
    }
    
    #csv <- lapply(csvtest, read.csv2, check.names = F)
    csv <-
      lapply(
        csvtest,
        FUN = function (x)
          read.table(
            x,
            sep = ";" ,
            dec = ",",
            header = T,
            check.names = F # good col names
          )
      )
    #csv <- lapply(csvtest, FUN = function (x) read_csv2(x))
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
    
    row.names(csvord[[1]]) = csvord[[1]][, 1]
    colnames(csvord[[3]])[1] = "X"
    colnames(csvord[[2]])[1] = "X"
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
  soso <<- T
  
  #sapply(strsplit(names(csvord[[3]]), "^adj.P.Val|^adj.P.Val"), `[[`, 1)
  
  return (csvord)
  
})

return(csvf)
}