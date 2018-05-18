###############################
########heatmap function & co #
###############################



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
hmobj <- reactiveValues()


observe({
  
  heatid <- input$matrixapp
  if (grepl("Heatmap", heatid)) {
    formatidus <<- length(formated()) # doit trouver une autre valeur que formated pour donner la taille
    if (formatidus < 2000)
      source(file.path("server", "plotreact.R"), local = TRUE)$value #
   else
     source(file.path("server", "plotreact2.R"), local = TRUE)$value #
  }
 


# observe({
#   obj <- hmobj$hm 
#   print(obj)
# 
# })


output$save <- downloadHandler(filename <- function() {
  paste0(basename(file_path_sans_ext("myfile")),
         '_heatmap.',
         input$form,
         sep = '')
},
content <- function(file) {
  if (input$form == "emf")
    
    emf(
      file,
      width = 7,
      height = 7,
      pointsize = 12,
      coordDPI = 300
    )
  
  else if (input$form == "png")
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
        height = 7)
  
  if (!is.null(formated()))
    withProgress(message = 'Saving heatmap:',
                 value = 0, {
                   n <- NROW(formated())
                   for (i in 1:n) {
                     incProgress(1 / n, detail = "Please wait...")
                   }
                   heatmapfinal(isplot = F)
                 })
  dev.off()
  
  })


  output$downloadcut <- downloadHandler(
    filename = function() {
      paste(basename(file_path_sans_ext(input$filename)),
            '_clustered',
            '.csv',
            sep = '')
    },
    content = function(file) {
      write.csv(heatmapfinal(isplot = T), file, row.names = FALSE)
    }
  )
  
  output$clustering <-
    renderDataTable(heatmapfinal(isplot = T)) # Summary of the significant genes depending on the pvalue with FC set to (1.2,2,4,6,10)
})


obsC <- observe(quote({ print(formatidus) }), quoted = TRUE)

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