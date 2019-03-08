### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#' renderncolour is a shiny widget which aims is to return for each group a colorbox picker
#'
#' @param id Shiny id
#'
#' @return Widget in the gui
#'
#' @export


renderncolour <- function(id){
  ns <- NS(id)
  uiOutput(ns("colorbox"))
}

#' colorChooser is a shiny module which aims is to dynamically affect for each group a color picker
#'
#' @param input Internal
#' @param output Internal
#' @param session Internal
#' @param data A reactive character vector for each selected group
#'
#' @return A reactive character vector that associated for each group a selected color
#'
#' @export
#'


colorChooser <- function(input, output, session, data){

  output$colorbox <- renderUI({
    ns <- session$ns
    colorfluid()
  })


  colorfluid <- reactive({
    req(initwidgetcol())
    lapply(1:length(initwidgetcol()), function(i){

      j = length(initwidgetcol())
      if(length(initwidgetcol()) %%2==0){
        if (i %% 2 == 0) {
          fluidRow(column(6, initwidgetcol()[[i - 1]]), column(6, initwidgetcol()[[i]]))
        }
      }
      else{
        if (i %% 2 ==0 && j!=i) {
          fluidRow(column(6, initwidgetcol()[[i - 1]]), column(6, initwidgetcol()[[i]]))
        }
        else if (j == i){
          fluidRow(column(6, initwidgetcol()[[i]]))
        }
      }

    })

  })

  initwidgetcol <- reactive({

    req(mycolgrp())
    ns <- session$ns
    pal = brewer.pal(8,"Dark2") %>%
      list(brewer.pal(10,"Paired")) %>%
      unlist()

    lapply(seq_along(unique(mycolgrp())), function(x) {
      colourInput(
        ns(paste("colgroup", x, sep = "_")),
        levels(mycolgrp())[x],
        pal[x],
        allowedCols =  pal,
        palette = "limited",
        returnName = T)
    })
  })


  getwidgvalues <- reactive({
    req(mycolgrp())
    lapply(seq_along(unique(mycolgrp())), function(i) {

      input[[paste("colgroup", i, sep = "_")]]
    })
  })

  mycolgrp <- reactive  ({
    req(data())
    mygrpcol <- data() %>%
      sort() %>%
      unique() %>%
      droplevels()

    return(mygrpcol)
  })


  return(reactive(getwidgvalues()))
}
