### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0



#' cutoffElements is a global namespace containing a list of shiny widgets filter the restable with cutoff parameters
#'
#' @param id Shiny id
#' @param c1 A numeric value to specify the column size of the first element
#' @param c2 A numeric value to specify the column size of the second element
#'
#' @return Widgets in the gui
#'
#' @export
#'

cutoffElements <- function(id, c1 =6 ,c2=6){
	SImin <- 0.01;
	SImax <- 0.1
	SIstep <- 0.01
	if(ns=="degstrip"){
		SImin <- 0.05
		SImax <- 1
		SIstep <- 0.05
	}

  ns <- NS(id)
  tagList(column(c1,
       sliderInput(ns("pval"),"P-value treshold",

                   min = SImin,max = SImax,
                   value = 0.05,step = SIstep
       )),

  column(c2,
       sliderInput(ns("fc"),"FC treshold",min = 1, max = 10,
                   value = 1,step = 1
       ))
)}


#' cutoffElements is a global namespace containing a list of htmlOuput from widget inputs
#'
#' @param id Shiny widget
#'
#' @return Widgets in the gui
#' 
#' @export
#'

tracketCutoff <- function(id){
  ns <- NS(id)
  tagList(
  htmlOutput(ns("myPVAL")),
  p("and"),
  htmlOutput(ns("myFC"))
  )
}



#' getDegenes is a shiny module which aims is to return an index or list of each significant genes from the restable for a set of comparison(s)
#'
#' @param input Internal
#' @param output Internal
#' @param session Internal
#' @param data A subset data frame with the selected comparisons for the adj.p.val or p.val and with the logfc for the hm
#' @param dflogfc A subset data frame with the selected comparisons for the logfc
#' @param maxDe A reactive numeric value
#' @param case A numeric value (case = 1 for heatmap and strip and 2 for venn diagram)
#' @param reg A reactive character 
#' @param meth A reactive Character
#'
#' @return
#' 
#' @export
#'

getDegenes <- function(input, output, session, data, dflogfc=NULL,  maxDe = NULL , case = NULL, reg = NULL, meth = NULL ){
  
  
  #########################################
  ######## FC step 0.1 if FC <2           #
  #########################################
   
  observe({
    
    req(input$fc)
    if (input$fc < 2)
      updateSliderInput(session,"fc",
        label = "FC treshold",value = NULL,
        min = 1,max = 10,
        step = .1
      )
    else
      updateSliderInput(session,"fc",
        label = "FC treshold",value = NULL,
        min = 1,max = 10,
        step = 1
      )

  })

  output$myPVAL <- renderText({ #pvalue selected
    ns <- session$ns
    input$pval
  })
  
  
  output$myFC <- renderText({ #Fold change value selected, default =1
    ns <- session$ns
    input$fc
  })


return(reactive({ 
  switch(case,
         decTestTRiX(data()[[1]],data()[[2]],data()[[3]], DEGcutoff = input$pval, FC = input$fc, cutoff_meth = meth(), maxDE=maxDe()),
         Vennlist(data(),dflogfc(), reg(), input$pval, input$fc))
}))

}



