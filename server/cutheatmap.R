#########################################
######## Cut heatmap Part               #
#########################################


#' p is a reactive function that return an heatmap gplots object
#'
#' @param heatmapfinal a function
#'
#' @return \p an object
#'

p <- eventReactive(input$updateheatm,{
  isolate(hmobj$obj)
})

#' PCAres is a reactive function that change the rownames values
#'
#' @param csvf a data frame 
#'
#' @return \rownamtoX a data frame with the row.names index corresponding to the first column index
#'

rownamtoX <- reactive({
  mycsv = csvf()[[3]]
  row.names(mycsv) = mycsv$X
  
  return(rownamtoX)
})

#' cutfinal is a reactive function that ....
#'
#' @param p an heatmap object
#' @param input$cutheight a numeric value to cut the dendogram 
#' @param new_data a data frame with specific columns depending on the user's choices
#' @param rownamtoX a data frame
#' @param groups a data frame of the choosen groups
#' @param input$cutcluster an heatmap object
#' @param input$cutinfo a character to select the plot to display heatmap, boxplot or stripchart
#'
#' @return \cutfinal a ggplot object or heatmapply object
#'

cutfinal <- reactive({
    cutHeatmaps(
      hmobj$obj,
      height = input$cutheight ,
      exprData = data.matrix(new_data()),
      groups = droplevels(new_group()$Grp),
      DEGres =  rownamtoX()[, -1],
      num = input$cutcluster,
      type = input$cutinfo
    )
})


# render to the ui the number of clusted for a define height in function of the current heatmap object
output$cutcluster <- renderUI({ 

   cut02 = cut( hmobj$obj$rowDendrogram, h = input$cutheight)
  #cut02 = cut(p()$rowDendrogram, h = input$cutheight)
  selectInput("cutcluster",
              "Choose your cluster",
              choices =  seq(1, length(cut02$lower), by = 1))
})


output$event <- renderPrint({ # interactive cursor that shows the selected points 
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
    output$cutheatmap <- renderPlotly({ # Plot/Render an object of class plotly
       cutfinal()

    })
  }
  else{
    output$cutheatmap <- renderPlotly({
      ggplotly(cutfinal(), height = 800, width = 1200)

    })
  }
})
