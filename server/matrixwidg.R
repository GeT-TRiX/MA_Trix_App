observeEvent(input$tabset25,{

isolate(
  if (grepl("widgetheat", input$tabset25)) {

    updateTabsetPanel(session, "tabset1",
                      selected = "hmmainpan")
  }
  else if (grepl("test3", isolate(input$tabset25))) {

    updateTabsetPanel(session, "tabset1",
                      selected = "cuthmmainpan")
    
  }
)
})


# observeEvent(input$tabset1,{
#   isolate(
#   if (grepl("cuthmmainpan", input$tabset1)) {
#     print("ko")
#     updateTabsetPanel(session, "tabset25",
#                       selected = "widgetheat")
#   }
#   else if (grepl("hmmainpan", input$tabset1)) {
#     print("koo")
#     updateTabsetPanel(session, "tabset25",
#                       selected = "test3")
#   }
#   )
# })





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
######## citation packages              #
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
