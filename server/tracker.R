### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0

output$myNUM <- renderPrint({ # number of signficant genes in the heatmap produced
  if(is.null(formated()[[1]]))
    return("X")
  else
    cat(length(formated()[[1]]))
})


output$indivcol <-  renderText({ # Groups selected
  my_final <<- paste(choix_grp(),as.character(),  sep=",") 
  my_final[length(choix_grp())] <<- gsub(",","",my_final[length(choix_grp())])
  print(my_final)
})


output$testtt <- renderText({ #Contrast selected
  my_final <<- paste(choix_test(),as.character(),  sep=",") 
  my_final[length(choix_test())] <<- gsub(",","",my_final[length(choix_test())])
  print(my_final)
})


output$myPVAL <- renderText({ #pvalue selected
  input$pval
})


output$myFC <- renderText({ #Fold change value selected, default =1
  input$fc
})

output$myMET <- renderText({ #Method for choosing the signficant genes, default = FDR (BH method)
  input$method2
})

output$myCLUST <- renderText({ #number of clusted selected, default = 3
  input$clusters
})

output$myMAT <- renderText({ #Method for the matrix distance, default = correlation method (pearson)
  input$dist
})

output$myPAL <- renderText({ #Colors selected for the different groups, default see palette in the global environment
  if(is.null(mypal()))
    palette[1:length(choix_grp())]
  else
    paste(mypal(),as.character(),  sep=",")
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


