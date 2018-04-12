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
  
  source(file.path("server", "computemean.R"), local = TRUE)$value
  
  #################################
  ######## Select the individuals #
  #################################
  
  source(file.path("server", "checkboxgrp.R"), local = TRUE)$value
  
  #################################
  ######## Select the comparisons #
  #################################
  
  source(file.path("server", "checkboxcontrast.R"), local = TRUE)$value
  
  #################################
  ######## Format the data frame  #
  #################################
  
  source(file.path("server", "grepcol.R"), local = TRUE)$value
  
  source(file.path("server", "indexselected.R"), local = TRUE)$value
  
  source(file.path("server", "datasummary.R"), local = TRUE)$value
  
  
  #################################
  ### Selected group and contrast #
  #################################
  
  source(file.path("server", "selgroupandcont.R"), local = TRUE)$value
  
  
  #########################################
  ######## Updating a colourInput         #
  #########################################
  
  source(file.path("server", "backgroundcolor.R"), local = TRUE)$value

  #########################################
  ######## Colors for the  groups         #
  #########################################
  
  source(file.path("server", "groupcolor.R"), local = TRUE)$value
  
  #########################################
  ######## Plot the data frame wiht input #
  #########################################
  
  source(file.path("server", "renderertable.R"), local = TRUE)$value
  
})

#shinyApp(ui = ui , server = server)
