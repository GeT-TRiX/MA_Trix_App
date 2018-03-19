source("compat.R")
source("formating.R")
source("global.R")

server <- function(input, output, session) {
  observeEvent(input$first, {
    csvf <- reactive({
      inFile <- input$file1
      
      if (is.null(inFile)) {
        createAlert(
          session,
          "alert",
          "exampleAlert",
          title = "Oops Error",
          content = "You need to import 3 files in the browser widget",
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
        
        for (i in 1:length(listed)) {
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
        
        output$individusel <- renderUI({
          selectInput("variable1", "Choose Option:", colnames(csv[[1]]))
        })
      }
      return (csvord)
      
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
        keysize = input$key
      )
      
    }, width = 1200 , height = 800, res = 100)
    
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
