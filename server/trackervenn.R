output$venngenes <- renderPrint({ # number of signficant genes in the heatmap produced
  req(vennfinal())
  if(input$dispvenn == "probes")
    cat(length(vennfinal()[[1]]$ProbeName))
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


output$myPVALvenn <- renderText({ #pvalue selected
  input$pvalvenn
})


output$myFCvenn <- renderText({ #Fold change value selected, default =1
  input$fcvenn
})

output$topgenesdf <- renderText({ #Method for choosing the signficant genes, default = FDR (BH method)
  input$topgenes
})

