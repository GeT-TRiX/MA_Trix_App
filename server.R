source("function/compat.R")
source("function/formating.R")
source("environnement/global.R")


shinyServer(server <- function(input, output, session) {
  n <- reactiveValues(a = 0)
  
  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "advanced", anim = TRUE)) ## hide and show event
  
  shinyjs::onclick("toggleAdvancedPCA",
                   shinyjs::toggle(id = "advancedPCA", anim = TRUE))
  
  shinyjs::onclick("toggleAdvancedcolors",
                   shinyjs::toggle(id = "advancedcol", anim = TRUE))
  
  #################################
  ######## Plot in the renderView #
  #################################
  
  observeEvent(input$heatm, {
    updateActionButton(session,
                       "heatm",
                       label = "Update Heatmap",
                       icon = icon("repeat"))
    
    output$distPlot <- renderPlot({
      plotHeatmaps(
        data.matrix(new_data()),
        formated(),
        droplevels(new_group()$Grp),
        workingPath = wd_path,
        prefix,
        suffix,
        k = input$clusters,
        Rowdistfun = input$dist ,
        Coldistfun = input$dist,
        keysize = input$key,
        meanGrp = input$meangrp
        
      )
    }, width = 900 , height = 1200, res = 100)
    
    #################################
    ######## Save plots             #
    #################################
    
    
    #' Reactive function that return a heatmap plot
    #'
    #' @param csv Data frame corresponding to the Alltoptable
    #'
    #' @return \p heatmap.2 plot
    #'
    
    
    p = reactive({
      plotHeatmaps(
        data.matrix(new_data()),
        formated(),
        new_group()$Grp,
        workingPath = wd_path,
        prefix,
        suffix,
        k = input$clusters,
        Rowdistfun = input$dist ,
        Coldistfun = input$dist,
        keysize = input$key,
        meanGrp = input$meangrp
        
      )
    })
    
    output$save <- downloadHandler(
      if (input$form == "eps") {
        filename = "save.eps"
      }
      else{
        filename = "save.png"
      },
      
      #' Save Heatmap in the good format
      #'
      #' @param file
      #'
      #' @return the image saved in eps or png
      #'
      
      
      content = function(file) {
        ggsave(
          p(),
          filename = filename,
          width = 12,
          height = 16,
          limitsize = FALSE,
          units = "cm",
          dpi = 200
        )
        
      }
    )
    
  })
  
  ###############################
  ######## Load the csv files   #
  ###############################
  
  
  #' Reactive function in the aim of loading csv files
  #'
  #' @param inFile
  #'
  #' @return csvord a list of csv files
  #'
  #' @examples
  #'
  
  source("server/csvFile.R")
  
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
        if (colnames(csv[[i]][2]) == "Grp") {
          csvord[[2]] = csv[[i]]
        }
        #else if (colnames(csv[[i]][10]) == "Amean")
        else if (any(grepl("adj.P.Val" , colnames(csv[[i]]))))
        {
          csvord[[3]] = csv[[i]]
          
        }
        else
          csvord[[1]] = csv[[i]]
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
  
  ###############################
  ######## click increase       #
  ###############################
  
  observeEvent(input$heatm, {
    n$a <<- n$a + 1
    updateNumericInput(session, 'num', value = n$a)
  })
  
  observe({
    print(n$a)
  })
  
  output$valuedd <- renderText({
    input$num
  })
  
  click <- 0
  isok <- T
  
  makeReactiveBinding('click')
  
  
  observeEvent(input$heatm, {
    if (click > 0)
    {
      isok <<- F
    }
    click <<- click + 1
  })
  
  observe(print(click))
  
  observe(if (click > 5)
    print("ok"))
  
  tested <- reactive(return(click))
  
  
  ###############################
  ######## Adding mean by group #
  ###############################
  
  output$value <- renderText({
    input$meangrp
  })
  
  mean_grp <- reactive({
    return(output$value)
  })
  
  #################################
  ######## Select the individuals #
  #################################
  
  
  output$individusel <- renderUI(
    checkboxGroupInput(
      inputId = "indiv" ,
      label =  "Choose your group to visualize",
      # choices =  colnames(csvf()[[1]][,-1]),
      # selected = colnames(csvf()[[1]][,-1])
      choices =  levels(csvf()[[2]]$Grp),
      selected = levels(csvf()[[2]]$Grp)
      
    )
  )
  
  observeEvent(input$allIndividus, {
    updateCheckboxGroupInput(
      session,
      "indiv",
      label = "Choose your group to visualize",
      #choices = colnames(csvf()[[1]][,-1]),
      #selected = colnames(csvf()[[1]][,-1])
      choices =  levels(csvf()[[2]]$Grp),
      selected = levels(csvf()[[2]]$Grp)
    )
  })
  
  observeEvent(input$noIndividus, {
    updateCheckboxGroupInput(session,
                             "indiv",
                             label = "Choose your group to visualize",
                             #choices = colnames(csvf()[[1]][,-1]))
                             choices =  levels(csvf()[[2]]$Grp))
  })
  
  #' Reactive function in the aim of selecting individuals
  #'
  #' @param input specific of the individuals data frame
  #'
  #' @return string of the different individuals selected
  #'
  
  
  # choix_individus <- reactive({
  #   return(input$indiv)
  # })
  
  choix_grp <- eventReactive(input$heatm, {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    return(input$indiv)
  }, ignoreNULL = F)
  
  
  # choix_grp <- reactive({
  #   return(input$indiv)
  # })
  
  #' Reactive function in the aim of having selected individuals in a list
  #'
  #' @param input specific of the individuals data frame
  #'
  #' @return a list of the different individuals selected
  #'
  
  
  list_ind <- reactive({
    return(list(input$indiv))
  })
  
  # output$indiv <-  renderText({
  #   choix_individus()
  # })
  
  output$indiv <-  renderText({
    choix_grp()
  })
  #################################
  ######## Select the comparisons #
  #################################
  
  output$testout <- renderUI(
    checkboxGroupInput(
      inputId = "test" ,
      label =  "Choose your comparison",
      choices =  colnames(adjusted()[, -1])
      #,selected = colnames(adjusted()[, -1])
      
    )
  )
  
  observeEvent(input$allTests, {
    updateCheckboxGroupInput(
      session,
      "test",
      label = "Choose your comparison",
      choices = colnames(adjusted()[, -1]),
      selected = colnames(adjusted()[, -1])
    )
  })
  
  observeEvent(input$noTests, {
    updateCheckboxGroupInput(session,
                             "test",
                             label = "Choose your comparison",
                             choices = colnames(adjusted()[,-1]))
  })
  
  
  #' Reactive function in the aim of selecting different comparison
  #'
  #' @param input specific of the comparison data frame
  #'
  #' @return \string of the different comparisons selected ### à verifier
  #'
  
  
  choix_test <- eventReactive(input$heatm, {
    return(input$test)
  }, ignoreNULL = F)
  
  # choix_test <- reactive({
  #   return(input$test)
  # })
  
  output$test <- renderText({
    choix_test()
  })
  
  #################################
  ######## Format the data frame  #
  #################################
  
  
  #' Reactive function that return a data frame with the adj.P.val selected by the individuals
  #'
  #' @param csv Data frame corresponding to the Alltoptable
  #'
  #' @return \adj a new data frame with all the adj.P.Val
  #'
  
  
  adjusted <- reactive({
    df <- csvf()
    if (is.null(df))
      return(NULL)
    adj = csvf()[[3]][, grep("X|^adj.P.Val",
                             names(csvf()[[3]]),
                             value = TRUE)]
    
    names(adj) =  gsub(
      pattern = "^adj.P.Val_",
      replacement = "",
      x = names(adj),
      perl =  TRUE
    )
    
    return(adj)
    
  })
  
  #' Reactive function that return a data frame with the logFC
  #'
  #' @param csv Data frame corresponding to the Alltoptable
  #'
  #' @return \adj a new data frame with all the adj.P.Val
  #'
  
  
  adjusted <- reactive({
    df <- csvf()
    if (is.null(df))
      return(NULL)
    adj = csvf()[[3]][, grep("X|^adj.P.Val",
                             names(csvf()[[3]]),
                             value = TRUE)]
    
    names(adj) =  gsub(
      pattern = "^adj.P.Val_",
      replacement = "",
      x = names(adj),
      perl =  TRUE
    )
    
    return(adj)
    
  })
  
  
  #' Reactive function that select specific individuals in the data frame
  #'
  #' @param csv Data frame corresponding to the pData table
  #'
  #' @return \new_group a new factor with the corresponding individuals from the checkbox with the good levels
  #'
  
  
  #new_group <-reactive(csvf()[[2]][csvf()[[2]]$X %in% choix_individus(),])
  
  
  new_group <- eventReactive(input$heatm, {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(), ]
  }
  , ignoreNULL = F)
  
  
  
  
  # new_group <- reactive({
  #   inFile <- input$file1
  #   if (is.null(inFile))
  #     return(NULL)
  #   csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(), ]
  # })
  
  #observeEvent(input$heatm, { ## React event
  
  #' Reactive function that return a data frame with significant genes for a defined p-value
  #'
  #' @param csv  Data frame corresponding to the WorkingSet
  #'
  #' @return \treated a data frame with the id for significant genes
  #'
  
  
  formated <- reactive({
    #treated = formating(new_test(), csvf()[[1]], input$pval)
    treated = formating(new_test(), input$pval)
    return(treated)
  })
  
  #' Reactive function that  select specific individuals in the data frame
  #'
  #' @param \csv Data frame corresponding to the Workingset
  #'
  #' @return adj a new data frame with all the adj.P.Val
  #'
  
  
  new_data <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    #subset(csvf()[[1]],select = choix_individus())
    select(csvf()[[1]], as.character(factor(new_group()$X)))
  })
  
  
  data_summary <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    myfinalfc(csvf()[[3]], input$pval1)
  })
  
  
  
  #' Reactive function that return a comparison data frame with the specific user's selection
  #'
  #' @param csv Data frame corresponding to the Alltoptable
  #'
  #' @return \new_data a  data frame with all the individuals selected
  #'
  
  new_test <- eventReactive(input$heatm, {
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    (subset(adjusted(),
            select = choix_test()))
  }, ignoreNULL = F)
  
  
  # new_group <- reactive( csvf()[[2]] %>%
  #                          filter( X ==  list_ind()))
  
  #' Reactive function that return a comparison data frame with the specific user's selection
  #'
  #' @param csv Data frame corresponding to the Alltoptable
  #'
  #' @return \new_data a  data frame with all the individuals selected
  #'
  
  # data_sign <- reactive({
  #   inFile <- input$file1
  #   if (is.null(inFile))
  #     return(NULL)
  #   createdfsign(adjusted())
  # })
  
  data_sign <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    ptv <- c(.01, .05)
    cbind.data.frame("FDR<1%" = colSums(adjusted()[,-1] < ptv[1]),
                     "FDR<5%" = colSums(adjusted()[,-1] < ptv[2]))
    
  })
  
  #########################################
  ######## Updating a colourInput         #
  #########################################
  
  
  colourpicker::updateColourInput(session, "col", label = "COLOUR:", value = "orange",
                    showColour = "background", allowTransparent = TRUE)
  
  
  
  #########################################
  ######## Plot the data frame wiht input #
  #########################################
  
  output$new_test <- renderDataTable(csvf()[[2]])
  
  output$new_data <- renderDataTable(head(csvf()[[1]][2:6]))
  
  output$new_group <- renderDataTable(new_group())
  
  output$data_sign <- renderDataTable(data_sign())
  
  output$data_summary <- renderDataTable(data_summary())
  
})
#shinyApp(ui = ui , server = server)
