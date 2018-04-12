source("function/compat.R")
source("function/formating.R")
source("function/PCA.R")
source("environnement/global.R")
source("function/decideTestTrix.R")


shinyServer(server <- function(input, output, session) {
  
  shinyjs::onclick("toggleAdvanced",
                   shinyjs::toggle(id = "advanced", anim = TRUE)) ## hide and show event
  
  shinyjs::onclick("toggleAdvancedPCA",
                   shinyjs::toggle(id = "advancedPCA", anim = TRUE))
  
  shinyjs::onclick("toggleAdvancedcolors",
                   shinyjs::toggle(id = "advancedcol", anim = TRUE))
  
  
  #################################
  ######## Plot in the renderView #
  #################################
  
  heatmapfinal <- function() {
    isolate({
      plotHeatmaps(
        data.matrix(new_data()),
        formated(),
        droplevels(new_group()$Grp),
        workingPath = wd_path,
        prefix,
        suffix,
        my_palette = colorRampPalette(c(
          choix_col1(), my_intermediate(), choix_col3()
        ))(n = 75),
        k = input$clusters,
        Rowdistfun = input$dist ,
        Coldistfun = input$dist,
        keysize = input$key,
        mycex = input$legsize ,
        cexrow = input$rowsize ,
        cexcol = input$colsize ,
        meanGrp = input$meangrp,
        labColu = input$colname ,
        labRowu = input$rowname,
        mypal =  unlist(colors()),
        showcol = input$colname,
        showrow = input$rowname,
        genename = csvf()[[3]]$GeneName
      )
    })
  }
  

  source(file.path("server", "plotandsave.R"), local = TRUE)$value
  
  
  ###############################
  ######## Load the csv files   #
  ###############################
  
  source(file.path("server", "csvFile.R"), local = TRUE)$value

  
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
    inFile <- input$file
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
      choices =  colnames(adjusted()[[1]][,-1])
      #,selected = colnames(adjusted()[, -1])
      
    )
  )
  
  observeEvent(input$allTests, {
    updateCheckboxGroupInput(
      session,
      "test",
      label = "Choose your comparison",
      choices = colnames(adjusted()[[1]][,-1]),
      selected = colnames(adjusted()[[1]][,-1])
    )
  })
  
  observeEvent(input$noTests, {
    updateCheckboxGroupInput(session,
                             "test",
                             label = "Choose your comparison",
                             choices = colnames(adjusted()[[1]][, -1]))
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
  
  
  source(file.path("server", "grepcol.R"), local = TRUE)$value
  
  
  
  #' Reactive function that select specific individuals in the data frame
  #'
  #' @param csv Data frame corresponding to the pData table
  #'
  #' @return \new_group a new factor with the corresponding individuals from the checkbox with the good levels
  #'
  
  
  #new_group <-reactive(csvf()[[2]][csvf()[[2]]$X %in% choix_individus(),])
  
  
  new_group <- eventReactive(input$heatm, {
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(),]
  }
  , ignoreNULL = F)
  
  
  # new_group <- reactive({
  #   inFile <- input$file
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
    treated = decTestTRiX(
      user_group()[[1]],
      user_group()[[2]],
      user_group()[[3]],
      DEGcutoff = input$pval,
      FC = input$fc,
      cutoff_meth = input$method2
    )
    return(treated)
  })
  
  #' Reactive function that  select specific individuals in the data frame
  #'
  #' @param \csv Data frame corresponding to the Workingset
  #'
  #' @return adj a new data frame with all the adj.P.Val
  #'
  
  
  new_data <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    #subset(csvf()[[1]],select = choix_individus())
    select(csvf()[[1]], as.character(factor(new_group()$X)))
  })
  
  
  data_summary <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    myfinalfc(csvf()[[3]], input$pval1)
  })
  
  ######
  ### Selected group
  #####
  
  source(file.path("server", "selectedgroup.R"), local = TRUE)$value
  
  # new_group <- reactive( csvf()[[2]] %>%
  #                          filter( X ==  list_ind()))
  
  
  #' Reactive function that return a comparison data frame with the specific user's selection
  #'
  #' @param csv Data frame corresponding to the Alltoptable
  #'
  #' @return \new_data a  data frame with all the individuals selected
  #'
  
  # data_sign <- reactive({
  #   inFile <- input$file
  #   if (is.null(inFile))
  #     return(NULL)
  #   createdfsign(adjusted())
  # })
  
  
  data_sign <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    ptv <- c(.01, .05)
    cbind.data.frame("FDR<1%" = colSums(adjusted()[, -1] < ptv[1]),
                     "FDR<5%" = colSums(adjusted()[, -1] < ptv[2]))
    
  })
  
  #########################################
  ######## Updating a colourInput         #
  #########################################
  
  
  colourpicker::updateColourInput(
    session,
    "col1",
    label = "downregulated genes:",
    value = firstcol,
    showColour = NULL,
    allowTransparent = FALSE,
    allowedCols = c("green", "orange", "blue"),
    returnName = T
  )
  
  
  colourpicker::updateColourInput(
    session,
    "col3",
    label = "upregulated genes:",
    value = lastcol ,
    showColour = NULL,
    allowTransparent = FALSE,
    allowedCols = c("red", "yellow"),
    returnName = T
  )
  
  
  
  choix_col1 <- reactive({
    return(input$col1)
  })
  
  choix_col3 <- reactive({
    return(input$col3)
  })
  
  
  my_intermediate <- reactive({
    if (choix_col1() == "green" & choix_col3() == "red")
      inter = "black"
    
    else if (choix_col1() == "orange" & choix_col3() == "red")
      inter = "yellow"
    
    else if (choix_col1() == "blue" & choix_col3() == "red")
      inter = "white"
    
    else if (choix_col1() == "blue" & choix_col3() == "yellow")
      inter = "green"
    
    
    return(inter)
    
  })
  
  
  #########################################
  ######## Colors for the  groups         #
  #########################################
  
  mycolgrp <- reactive  ({
    mygrpcol <- new_group()$Grp %>%
      sort() %>%
      unique() %>%
      droplevels()
    
    return(mygrpcol)
  })
  
  
  cols <- reactive({
    if (is.null(mypaletA()))
      mypaletA() = palette
    
    lapply(seq_along(mycolgrp()), function(i) {
      colourInput(
        paste("col", i, sep = "_"),
        levels(mycolgrp())[i],
        mypaletA()[i],
        allowedCols =  palette,
        palette = "limited",
        returnName = T
      )
    })
  })
  
  mypaletA <- reactive  ({
    if (is.null(mypal))
      return(NULL)
    else
      mypal = (colors())
    
    return(mypal)
  })
  
  mypal <- reactive({
    unlist(colors())
  })
  
  
  output$myPanel <- renderUI({
    cols()
  })
  
  
  colors <- reactive({
    lapply(seq_along(mycolgrp()), function(i) {
      input[[paste("col", i, sep = "_")]]
    })
  })
  
  
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
