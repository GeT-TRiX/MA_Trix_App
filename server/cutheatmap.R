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
    req(hmobj$obj)
  
    pdf(NULL)
    cutHeatmaps(
      hmobj$obj,
      height =  hmsize$cut,
      exprData = data.matrix(new_data()),
      groups = droplevels(new_group()$Grp),
      DEGres =  rownamtoX()[, -1],
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
  if (is.null(d))
    "Hover on a point!"
  else {
    
    round(sort(d),digits=2)
  }
})

observe({ 
  req(hmobj$obj)
  if (req(input$cutinfo) == "Heatmap") {
    output$cutheatmap <- renderPlotly({ # Plot/Render an object of class plotly
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
    #plot(PCAplot())
    dev.off()
  })


