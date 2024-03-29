### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#########################################
######## Cut heatmap Part               #
#########################################


#' cutfinal is a reactive function that return heatmap or ggplot2 object
#'
#' @param hmobj$obj heatmap object
#' @param cut a numeric input corresponding to the height where the dendogram is cut
#' @param subsetwset a data frame with specific columns depending on the user's choices
#' @param genename a data frame
#' @param groups a data frame of the choosen groups
#' @param cutcluster a numeric input corresponding to the selected cluster to display
#' @param cutinfo a character input to select the plot to display heatmap, boxplot or stripchart
#'
#' @return a ggplot object or heatmapply object
#'
#' @export
#'

cutfinal <- reactive({
    req(hmobj$obj,hmsize$cut)

    pdf(NULL)
    cutHeatmaps(
      hmobj$obj$hm,
      height =  hmsize$cut,
      genename = csvf()[[3]],
      exprData = data.matrix(subsetwset()),
      groups = droplevels(subsetgroup_hm()$Grp),
      num = input$cutcluster,
      type = input$cutinfo,
      mypal = unlist(colors())
    )
})


# render to the ui the number of clusted for a define height in function of the current heatmap object
output$cutcluster <- renderUI({
  req(hmobj$obj)
  cut02 = cut(hmobj$obj$hm$rowDendrogram, h = hmsize$cut)
  selectInput("cutcluster",
              "Choose your cluster",
              choices =  seq(1, length(cut02$lower), by = 1))
})


output$event <- renderPrint({ # interactive cursor that shows the selected points
  d <- event_data("plotly_hover")
  if (is.null(d)) "Hover on a point!" else d
})

observe({
  req(hmobj$obj)
  if (req(input$cutinfo) == "Heatmap") {
     output$cutheatmap <- renderPlot({ # Plot/Render an object of class plotly
        cutfinal()
     })
  }
  else{
    output$cutheatmap <- renderPlotly({
      ggplotly(cutfinal())

    })
  }
})


callModule(downoutputfiles, "saveboxclust", projectname = projectname , suffix= "_cutheat." , data = cutfinal,  w = 10, h = 10 , cutheat = T )


