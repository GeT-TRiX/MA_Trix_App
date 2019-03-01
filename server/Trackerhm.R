### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0

output$myNUM <- renderPrint({ # number of signficant genes in the heatmap produced
  req(subsetDEG())
  if(is.null(subsetDEG()[[1]]))
    return("X")
  else
    cat(length(subsetDEG()[[1]]))
})


output$maxGen <- renderPrint({ # number of signficant genes in the heatmap produced
  req(input$maxgen)
  cat(input$maxgen)
})


output$col <-  renderText({ # Groups selected
  my_final <<- paste(input$grouphm,as.character(),  sep=",") 
  my_final[length(input$grouphm)] <<- gsub(",","",my_final[length(input$grouphm)])
  my_final
})


output$testtt <- renderText({ #Contrast selected
  my_final <<- paste(selected_test(),as.character(),  sep=",") 
  my_final[length(selected_test())] <<- gsub(",","",my_final[length(selected_test())])
  my_final
})


output$myPVAL <- renderText({ #pvalue selected
  input$pval
})


output$myFC <- renderText({ #Fold change value selected, default =1
  input$fc
})

output$myMET <- renderText({ #Method for choosing the signficant genes, default = FDR (BH method)
  input$decidemethod
})

output$myCLUST <- renderText({ #number of clusted selected, default = 3
  input$clusters
})

output$myMAT <- renderText({ #Method for the matrix distance, default = correlation method (pearson)
  input$dist
})

output$myPAL <- renderText({ #Colors selected for the different groups, default see palette in the global environment
  if(is.null(colors()))
    palette[1:length(input$grouphm)]
  else
    paste(unlist(colors()),as.character(),  sep=",")
})

output$myLEG <- renderText({ #Legend size, default = 0.8
  input$legsize
})

output$myROW <- renderText({#Row size, default = 0.9
  input$rowsize
})
output$myCOL <- renderText({#Col size, default = 0.9
  input$colsize
})


