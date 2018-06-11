###############################
######## Reactive side        #
###############################

shinyjs::disable("heatm")


#' hmbis is an event reactive function that pre-computed hierarchical clustering on microarray data 
#'
#' @param new_data a data frame with all the individuals selected
#' @param formated  a data frame with the indexes corresponding to the sigificant genes
#' @param new_group  a data frame with the corresponding groups 
#' @param workingPath the current user's repository 
#' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
#' @param Rowdistfun a function used to compute the distance for the rows
#' @param Coldistfun a function used to compute the distance for the columns
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#'
#' @return  a list of objects which aim is to being passed as argument in the plotHeatmaps function
#' 
#' @export
#' 


hmbis <- reactive( {

      truncatedhat(
        data.matrix(new_data()),
        formated()[[1]], 
        droplevels(new_group()$Grp),
        workingPath = wd_path,
        k = input$clusters,
        mypal = unlist(colors()),
        Rowdistfun = input$dist ,
        Coldistfun = input$dist,
        meanGrp = input$meangrp,
        genename =  csvf()[[3]]
      )
    
    
  
})

output$distPlot <- renderPlot({
    
    validate(need(
      csvf(),
      'You need to import data to visualize to plot the Heatmap' ) %next%
        need(length(choix_test()) >0, 'You need to select a contrast(s) with reactivity triggered you dont need to click on the update heatmap button')
    )
    
    if(is.null(my_intermediate())){
      isolate(heatmapfinal(isplot = F))
      shinyjs::alert("your choice color are not fit to be together!!")
    }
    
    
    if ( input$reactheat == T){
      
      hmbis()
      observe({boolhm <<-T})
      output$heatmbool <- reactive({
        boolhm
      })

      hmboth$tot <- heatmapfinal()
      hmobj$hm <- hmboth$tot[[1]]
      hmobj$obj <-hmboth$tot[[2]]
      
    }
    else{
      #isolate(heatmapfinal(isplot = F))
      #isolate(heatmapfinal())
      #isolate(isisolatehm())
      print("ok")
      validate(
          need(input$heatm, 'You are not in reactive mod anymore, please click on the heatmap button in order to update the heatmap' )
      )
      NULL
    }
      
      
      

}, width = 800 , height = 1100, res = 100)





