#########################################
######## Cut heatmap Part               #
#########################################


#' PCAres is a reactive function that computed a PCA of non-normalized data
#'
#' @param csvf a data frame corresponding to the WorkingSet
#'
#' @return \PCAres a reactive data frame with PCA attributes
#'

p <- eventReactive(input$updateheatm,{
  isolate(heatmapfinal())
})

#' PCAres is a reactive function that computed a PCA of non-normalized data
#'
#' @param csvf a data frame corresponding to the WorkingSet
#'
#' @return \PCAres a reactive data frame with PCA attributes
#'

test <- reactive({
  mycsv = csvf()[[3]]
  row.names(mycsv) = mycsv$X
  return(test)
  
})

#' PCAres is a reactive function that computed a PCA of non-normalized data
#'
#' @param csvf a data frame corresponding to the WorkingSet
#'
#' @return \PCAres a reactive data frame with PCA attributes
#'

cutfinal <- reactive({
    cutHeatmaps(
      p(),
      height = input$cutheight ,
      exprData = data.matrix(new_data()),
      groups = droplevels(new_group()$Grp),
      DEGres =  test()[, -1],
      num = input$cutcluster,
      type = input$cutinfo
    )
})



output$cutcluster <- renderUI({
  req(p())
  
  cut02 = cut(p()$rowDendrogram, h = input$cutheight)
  selectInput("cutcluster",
              "Choose your cluster",
              choices =  seq(1, length(cut02$lower), by = 1))
})


output$event <- renderPrint({
  d <- event_data("plotly_hover")
  if (is.null(d))
    "Hover on a point!"
  else
    cat("Average expression Z-score over replicates; ",
        length(d$pointNumber),
        " probes")
})

observe({
  if (req(input$cutinfo) == "Heatmap") {
    output$cutheatmap <- renderPlotly({
       cutfinal()

    })
  }
  else{
    output$cutheatmap <- renderPlotly({
      ggplotly(cutfinal(), height = 800, width = 1200)

    })
  }
})
