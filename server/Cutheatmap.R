### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


#########################################
######## Cut heatmap Part               #
#########################################


#' p is a reactive function that return a heatmap gplots object
#'
#' @param updateheatm  clickable input button
#' @param hmobj$obj a reactive value object
#'
#' @return p an heatmap object isolate
#'
#' @export
#'

p <- eventReactive(input$updateheatm,{
  isolate(hmobj$obj)
})



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
    req(hmobj$obj)

    pdf(NULL)
    cutHeatmaps(
      hmobj$obj,
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
  cut02 = cut( hmobj$obj$rowDendrogram, h = hmsize$cut)
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


output$savecut <- downloadHandler(

  filename <- function() {
    paste0(basename(file_path_sans_ext(projectname())), '_cutheat.',input$formcut, sep='')
  },
  content <- function(file) {
    if (input$formcut == "pdf")

      pdf(file,
          width = 10,
          height = 10,
          pointsize = 12)


    else if (input$formcut == "png")

      png(file,
          width =1000,
          height = 1000,
          units = "px",
          pointsize= 12,
          res=100
      )
    else
      cairo_ps(filename=file, width=10, height=10,pointsize = 12)

    plot(cutfinal())
    dev.off()
  })
