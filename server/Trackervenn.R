### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


output$venngenes <- renderPrint({ # number of signficant genes in the heatmap produced
  req(input$selcontjv)
  if(any(grepl("probes|transcripts", input$dispvenn)))
    cat(length(vennfinal()[[1]][[1]]))
  else
    cat(length(vennfinal()[[1]]$GeneName))
})


output$contvenn <- renderText({ #Contrast selected
  my_final <<- paste(colnames(user_cont()),as.character(),  sep=",")
})

output$continter <- renderText({ #Contrast selected
  my_final <<- paste(vennchoice(),as.character(),  sep=",")
})

output$totalgenes <- renderText({

  req(vennlist())
  sum(sapply(vennlist()[[1]],length))

})



output$topgenesdf <- renderText({ #Method for choosing the signficant genes, default = FDR (BH method)
  input$topgenes
})
