source("function/compat.R")
source("function/formating.R")
source("function/PCA.R")
source("environnement/global.R")
source("function/decideTestTrix.R")
source("function/vennplot.R")
source("function/create_forked_task.R")
source("function/cutheat.R")

shinyServer(server <- function(input, output, session) {
  
  ###############################
  ######## Load the csv files   #
  ###############################

  
  source(file.path("server", "csvFile.R"), local = TRUE)$value #
  
  
  output$downloadData <- downloadHandler(
    filename <-function(){
      paste("sampleData",".zip", sep='')
    },
    content <- function(file) {
      file.copy("data/sampleData.zip", file)
    },
    contentType = "zip"
  )
  
  observeEvent(input$resetAll, {
    reset("form")
  })
  
  
  observeEvent(input$session,{
    output$SessionInfo <- renderText(paste(capture.output(sessionInfo()), collapse = "<br>"))
  })
  
  
  
  
  
  
  
  ##################################
  ######## Hide and modify buttons #  
  ##################################
  
  source(file.path("server", "changeheatmbut.R"), local = TRUE)$value #
  source(file.path("server", "hidevent.R"), local = TRUE)$value #
  
  #################################
  ######## Plot in the renderView #
  #################################
  
  source(file.path("server", "heatmapshiny.R"), local = TRUE)$value #
  
  ###############################
  ######## tracker              #
  ###############################
  
  source(file.path("server", "tracker.R"), local = TRUE)$value #
  
  ###############################
  ######## Plot&save heatm PCA  #
  ###############################
  
  source(file.path("server", "PCAshiny.R"), local = TRUE)$value #
  source(file.path("server", "plotandsave.R"), local = TRUE)$value #
  
  ###############################
  ######## Adding mean by group #
  ###############################
  
  source(file.path("server", "computemean.R"), local = TRUE)$value #
  
  #################################
  ######## Select the individuals #
  #################################
  
  source(file.path("server", "checkboxgrp.R"), local = TRUE)$value #
  
  #################################
  ######## Select the comparisons #
  #################################
  
  source(file.path("server", "checkboxcontrast.R"), local = TRUE)$value #
  
  #################################
  ######## Format the data frame  #
  #################################
  
  source(file.path("server", "grepcol.R"), local = TRUE)$value #
  source(file.path("server", "indexselected.R"), local = TRUE)$value #
  source(file.path("server", "datasummary.R"), local = TRUE)$value #
  
  #################################
  ### Selected group and contrast #
  #################################
  
  source(file.path("server", "selgroupandcont.R"), local = TRUE)$value #
  
  #########################################
  ######## Updating a colourInput         #
  #########################################
  
  source(file.path("server", "backgroundcolor.R"), local = TRUE)$value #
  
  #########################################
  ######## Colors for the  groups         #
  #########################################
  
  source(file.path("server", "groupcolor.R"), local = TRUE)$value #
  
  #########################################
  ######## Plot the data frame wiht input #
  #########################################
  
  source(file.path("server", "renderertable.R"), local = TRUE)$value #
  
  #########################################
  ######## PCA part                       #
  #########################################
  
  source(file.path("server", "PCAsandp.R"), local = TRUE)$value #
  source(file.path("server", "colforpca.R"), local = TRUE)$value #
  
  #########################################
  ######## Venn part                      #
  #########################################
  
  source(file.path("server", "Venn.R"), local = TRUE)$value #
  source(file.path("server", "Vennrender.R"), local = TRUE)$value #
  
  #########################################
  ######## cutheatmap part                #
  #########################################
  
  source(file.path("server", "cutheatmap.R"), local = TRUE)$value #
  
})

#shinyApp(ui = ui , server = server)
