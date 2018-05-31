###############################
########heatmap function & co #
###############################

boolhm <- F


#hmneed <- T # Boolean uses to hide or show the mardkwon serving to load data



output$heatmbool <- reactive({

  boolhm
})



outputOptions(output, "heatmbool", suspendWhenHidden = F)



observe({
  req(csvf(),length(choix_test()) >0,global$clicked )
  
  observe({boolhm <<-T}) # modify and lock the bool value to false
  
  output$heatmbool <- reactive({
    boolhm
  })
  
  
})


#' rowname is a reactive function which aim is to hide or show the rownames
#'
#' @param input$rowname  a boolean radio button input
#'
#' @return \rowname a reactive boolean value
#'

rowname <- reactive({
  rowname <- switch(input$rowname,
                    hide = F,
                    show = T,
                    F)
  return(rowname)
})

#' colname is a reactive function which aim is to show or hide the colnames
#'
#' @param input$colname  a boolean radio button input
#'
#' @return \colname a reactive  reactive boolean value
#'

colname <- reactive({
  colname <- switch(input$colname,
                    hide = T,
                    show = F,
                    F)
  return(colname)
})


heatmapobj <- NULL # declare outside the observeEvent
formatidus <- NULL
hmbis <- reactiveValues()
hmboth <- reactiveValues()
hmobj <- reactiveValues()
hmsize <- reactiveValues()


observe({
  
  #   output$required <- renderPrint({
  #
  #   validate(
  #     need(csvf(), 'You need to import data to visualize to plot the Heatmap') %then%
  #     need(choix_test() > 0, 'You need to select your comparison(s)'))
  #
  # })
  
  #click <- reactive({
  # boolneed <- F
  #   observeEvent(input$heatm,{
  #     boolneed <<- T
  #   })
   # return(bool)
  #})
  

  
  
  
  output$warningsheat <- renderPrint({#renderPlot({
    validate(need(
      csvf(),
      'You need to import data to visualize to plot the Heatmap' ) %next% 
      need(length(choix_test()) >0, 'You need to select a contrast(s), then click on the heatmap button down below the heatmap settings')

    )
  })
  
  
  heatid <- input$matrixapp
  if (grepl("Heatmap", heatid)) {
    if (input$reactheat == T)
      source(file.path("server", "plotreact.R"), local = TRUE)$value #
    else
      source(file.path("server", "plotreact2.R"), local = TRUE)$value #
    
  }
  
  
  
  
  output$savehm <- downloadHandler(filename <- function() {
    paste0(basename(file_path_sans_ext(projectname())),
           '_heatmap.',
           input$formhm,
           sep = '')
  },
  content <- function(file) {
    if (input$formhm == "emf")
      
      emf(
        file,
        width = 7,
        height = 7,
        pointsize = 12,
        coordDPI = 300
      )
    
    else if (input$formhm == "png")
      png(
        file,
        width = 900,
        height = 1200,
        units = "px",
        pointsize = 12,
        res = 100
      )
    else
      eps(file,
          width = 7,
          height = 9)
    
    if (!is.null(formated()[[1]]))
      withProgress(message = 'Saving heatmap:',
                   value = 0, {
                     n <- NROW(formated()[[1]])
                     for (i in 1:n) {
                       incProgress(1 / n, detail = "Please wait...")
                     }
                     heatmapfinal(isplot = F)
                   })
    dev.off()
    
  })
  
  
  output$downloadcut <- downloadHandler(
    filename = function() {
      paste(basename(file_path_sans_ext(projectname())),
            '_clustered_hm',
            '.csv',
            sep = '')
    },
    content = function(file) {
      write.csv(ordered(), file, row.names = FALSE)
    }
  )
  
  ordered <- reactive({
    req(hmobj$hm)
    
    if (input$method2 == "FDR")
      met = "adj.P.Val_"
    else
      met = "P.value_"
    
    mycont = paste0(met, choix_test())
    ordered = csvf()[[3]] %>% filter(ProbeName %in% hmobj$hm$ProbeName)  %>%
      select(ProbeName,  mycont) %>%
      full_join(hmobj$hm[,-1], ., by = "ProbeName") %>%
      select(ProbeName, GeneName, mycont, cluster)
    rightor = sort(as.integer(rownames(ordered)), decreasing = T)
    ordered = ordered[match(rightor, rownames(ordered)), ]
    
    return(ordered)
  })
  

  
  output$clustering <-
    renderDataTable(ordered()) # Summary of the significant genes depending on the pvalue with FC set to (1.2,2,4,6,10)
  
})




# cutedhm <- reactive({
#   req(hm())
#   cut02 = cut(hm()$rowDendrogram, h = input$cutheatm)
#   return(cut02)
# })


# cluster <-  reactive({

#cut02 = cut(hm()$rowDendrogram, h = input$cutheatm)
#   dfclust = heatmtoclust( hm(), formated(), data.matrix(new_data()), csvf()[[3]], input$cutheatm)
#
#   return(dfclust)
# })