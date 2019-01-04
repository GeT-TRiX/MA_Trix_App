### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



plotHeight <- reactive({
  ifelse(is.null(input$plotHeight), 0, (input$plotHeight/1.25)) ## responsive plot
})


#########################################
######## Loading screen                 #
#########################################

hide(id = "loading-content", anim = TRUE, animType = "fade",time=2)
hide(id = "loading-content-bar", anim = TRUE, animType = "fade",time=2)

observe({
  collapsestate <- input$sidebarCollapsed
  session$sendCustomMessage(type="iscollapse", collapsestate)
})


#########################################
######## Redirection Panel              #
#########################################

observeEvent(input$heatmapanel, {

isolate(
  if (grepl("widgetheat", input$heatmapanel)) {

    updateTabsetPanel(session, "heatmapmainp",
                      selected = "hmmainpan")
  }
  else if (grepl("cutheatmainp", isolate(input$heatmapanel))) {

    updateTabsetPanel(session, "heatmapmainp",
                      selected = "cuthmmainpan")
    
  }
)
})

#########################################
######## FC step 0.1 if FC <2           #
#########################################

observe({
  
  if (input$fcvenn <= 2)
    updateSliderInput(
      session,
      "fcvenn",
      label = "FC treshold",
      value = NULL,
      min = 1,
      max = 10,
      step = .1
    )
  else
    updateSliderInput(
      session,
      "fcvenn",
      label = "FC treshold",
      value = NULL,
      min = 1,
      max = 10,
      step = 1
    )
  
  if (input$fc <= 2)
    updateSliderInput(
      session,
      "fc",
      label = "FC treshold",
      value = NULL,
      min = 1,
      max = 10,
      step = .1
    )
  else
    updateSliderInput(
      session,
      "fc",
      label = "FC treshold",
      value = NULL,
      min = 1,
      max = 10,
      step = 1
    )
  
})

#################################################
######## Download data and reset button heatmap #
#################################################

output$downloadData <- downloadHandler(filename <- function() {
  paste("sampleData", ".zip", sep = '')
},
content <- function(file) {
  file.copy("data/sampleData.zip", file)
},
contentType = "zip")

observeEvent(input$resetAll, {
  reset("form")
})


#########################################
######## Citation packages              #
#########################################

#' mypacklist is a reactive function which aim is to display the different packages used in the current session
#'
#' @param sessionInfo version information about R, the OS and attached or loaded packages. 
#'
#' @return a data frame
#'
#' @export
#' 


mypacklist <- reactive({
  mysess <- sessionInfo()
  dfpack <- names(sessionInfo()$otherPkgs) %>%
    lapply(function(x)
      return(
        paste(mysess$otherPkgs[[x]]$Package, mysess$otherPkgs[[x]]$Version)
      )) %>%
    unlist() %>%
    cbind(., unlist(lapply(names(mysess$otherPkgs), function(x)
      return(paste(mysess$otherPkgs[[x]]$Title))))) %>%
    as.data.frame() %>%
    setNames(c('Version', "Title"))
  
  return(dfpack)
})


observeEvent(input$session, {
  req(mypacklist())
  output$sessinfo <- renderDataTable(mypacklist())
})



#########################################
######## Grep project name              #
#########################################

file_name <- reactive({
  req(csvf())
  inFile <- csvf()[[4]] 
  if (class(inFile)== "character")
    return(tools::file_path_sans_ext(inFile))
  else
    return (tools::file_path_sans_ext(inFile$name))
})

projectname <- reactive({
  req(file_name())
  splitbyunder <- strsplit(file_name(), "_")
  MAindex = grepl("^MA", splitbyunder[[2]])
  isindex = which(MAindex == T)
  outputname = list(splitbyunder[[2]][isindex], MAindex)
  if(length(outputname[[1]]) == 0){
    return(Sys.Date())
  }
  else
    return(outputname)
  
})


