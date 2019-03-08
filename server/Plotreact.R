### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0



###############################
######## Reactive side        #
###############################

shinyjs::disable("heatm")


#' hmbis is an event reactive function that pre-computed hierarchical clustering on microarray data
#'
#' @param subsetwset a data frame with all the selected groups
#' @param subsetDEG  a data frame with the indexes corresponding to the sigificant genes
#' @param subsetgroup_hm  a data frame with the corresponding groups
#' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
#' @param Rowdistfun a function used to compute the distance for the rows
#' @param Coldistfun a function used to compute the distance for the columns
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#' @param genename a data frame
#'
#' @return  a list of objects which aim is to being passed as argument in the plotHeatmaps function
#'
#' @export
#'


hmbis <- reactive( {

      truncatedhat(
        data.matrix(subsetwset()),
        subsetDEG()[[1]],
        droplevels(subsetgroup_hm()$Grp),
        k = input$clusters,
        mypal = unlist(colors()),
        Rowdistfun = input$dist ,
        Coldistfun = input$dist,
        meanGrp = input$meangrp,
        genename =  csvf()[[3]],
        algo = input$algomet

      )



})




output$distPlot <- renderPlot({

    validate(need(
      csvf(),
      'You need to import data to visualize to plot the Heatmap' ) %next%
        need(length(selected_test()) >0, 'You need to select a contrast(s) with reactivity triggered you dont need to click on the update heatmap button')
    )

    if(is.null(my_intermediate())){

      isolate({pdf(NULL)
        heatmapfinal(isplot = F)
        })
      shinyjs::alert("The colors defined for the heatmap are not fit to be together!!")
      return(NULL)

    }


    if (input$reactheat == T){

      hmbis()

      isolate({
      hmsize$cut <- hmbis()[[8]]
      hmobj$obj$rows <- hmbis()[[6]]
      hmobj$obj$cols <-  hmbis()[[5]]
      hmobj$obj$colgroup <- unlist(colors())
      hmobj$obj$groups <-  droplevels(subsetgroup_hm()$Grp)
      hmobj$obj$rownames <- hmbis()[[7]]
      })

      observe({boolhm <<-T})

      output$heatmbool <- reactive({
        boolhm
      })

      hmboth$tot <- heatmapfinal(isplot = F)
      hmobj$hm <- hmboth$tot[[1]]
      isolate(hmobj$obj$hm <- hmboth$tot[[2]])



    }
    else{

      validate(
          need(input$heatm, 'You are not in reactive mod anymore, please click on the heatmap button in order to update the heatmap' )
      )
      NULL
    }

})
