vennchoice <- reactive({
  if (is.null (input$intscol))
    return(NULL)
  else
    return(input$intscol)
})

output$myselvenn <- renderUI({
  req(user_cont())
  #intscol <- names(user_cont())#names(adjusted()[[1]][,-1])
  selectInput(
    'intscol',
    'Specify your interaction(s):',
    choices = names(user_cont()),
    multiple = TRUE
  )
})

venninter <- reactive({
  req(vennlist(), user_cont())
  
  myelist <- setvglobalvenn(vennlist(), user_cont())
  #print(myelist)
  
  
  return(myelist)
})


vennfinal <- reactive({
  req(vennchoice())
  if (is.null(vennchoice))
    return(NULL)
  
  reslist = list()
  reordchoice <- vennchoice() %>%
    factor(levels = names(adjusted()[[1]][,-1])) %>%
    sort() %>%
    paste(collapse = "")
  
  
  resfinal = csvf()[[3]] %>%
    filter(ProbeName %in% venninter()[[reordchoice]]) %>%
    select(ProbeName, GeneName, paste0("logFC_", vennchoice())) %>%
    mutate_if(is.numeric, funs(format(., digits = 3)))
  
  reslist[[1]] = resfinal
  
  mycont = paste0("logFC_", vennchoice())
  if(input$dispvenn == "genes"){
    for (i in mycont) {
      resfinal[[i]] = as.numeric(as.character(resfinal[[i]]))
    }
  
    resfinal <- resfinal[,-1] %>% as.data.table() %>% .[,lapply(.SD,mean),"GeneName"] 
    resfinal = as.data.frame(resfinal)
    reslist[[2]] = resfinal
  }
  #mutate_if(is.numeric, funs(formatC(., format = "f")))
  
  return(reslist)
  #return(resfinal)
})


# label {
#   display: inline-block;
#   max-width: 100%;
#   margin-bottom: 0px;
#   font-weight: 700;
# }

output$topgenesvenn <- renderUI({
  req(vennfinal(), vennchoice())
  
  tags$div(
    class = "topgeness",numericInput('topgenes',
               'Top genes', value = 50,
               min = 1,
               max = length(vennfinal()[[1]]$ProbeName))
    )
})


output$venntitle <- renderText({
  req(input$topgenes)
  if(input$dispvenn == "probes")
    mytitlevenn <<- print(paste("Barplot showing the top ", input$topgenes ," genes"))
  else
    mytitlevenn <<- print(paste("Barplot showing the computationnal logFC mean of the top " ,input$topgenes , " genes before the rendering table"))
})


output$venngenesbef <- renderText({
  req(input$topgenes)
  if(input$dispvenn == "genes")
  mytitlevenn <<- print(paste("Barplot showing the computationnal logFC mean of the top " ,input$topgenes , " genes after the rendering table"))
  
})


output$dfvenn <- renderText({
  req(input$topgenes)
  if(input$dispvenn == "probes")
    mytitlevenn <<- print(paste("Table showing the ProbeNames and GeneNames associated with the respective logFC for the intersection(s) selected"))
  else
    mytitlevenn <<- print(paste("Table showing the GeneNames associated with the average logFC for the intersection(s) selected"))
    
  
})

output$dfvennbef <- renderText({
  req(input$topgenes)
  if(input$dispvenn == "genes")
    mytitlevenn <<- print(paste("Table showing the GeneNames associated with the respective logFC for the intersection(s) selected"))
  
})




venntopgenes <- reactive({
    if (is.null (input$topgenes))
      return(NULL)
    else
      return(input$topgenes)
  })

output$downloadvennset = downloadHandler(
  'venns-filtered.csv',
  content = function(file) {
    s = input$vennresinter_rows_all
    if(input$dispvenn == "probes")
      write.csv2(vennfinal()[[1]][s, , drop = FALSE], file)
    else
      write.csv2(vennfinal()[[2]][s, , drop = FALSE], file)
  }
)


plottopgenes <- eventReactive(input$topdegenes, {
  req(vennfinal(), vennchoice(), venntopgenes())
  mycont = paste0("logFC_", vennchoice())
  if(input$dispvenn == "probes")
    myplot <- topngenes(vennfinal()[[1]][input$vennresinter_rows_all, , drop = FALSE], mycont, venntopgenes(), input$dispvenn)
  else
    myplot <- topngenes(vennfinal()[[2]][input$vennresinter_rows_all, , drop = FALSE], mycont, venntopgenes(), input$dispvenn)
  
  
  return(myplot)
})


plottopgenesmean <- eventReactive(input$topdegenes, {
  req(vennfinal(), vennchoice(), venntopgenes())
  mycont = paste0("logFC_", vennchoice())
    myplot <- topngenes(vennfinal()[[1]][input$vennresintergen_rows_all, , drop = FALSE], mycont, venntopgenes(), input$dispvenn, mean = T)
  
  return(myplot)
})



observeEvent(input$topdegenes, {
  isolate(output$barplotvenn <- renderPlot({
    req(plottopgenes())
    plotOutput(plottopgenes())
    
  }))
  
})


observeEvent(input$topdegenes, {
  isolate(output$barplotvennmean <- renderPlot({
    req(plottopgenesmean(), input$dispvenn == "genes")
    plotOutput(plottopgenesmean())
    
  }))
  
})



observe({
  validate(need(csvf(), 'You need to import data to visualize this plot!'))
  
  output$savebarplot <- downloadHandler(filename <- function() {
    paste0(
      basename(tools::file_path_sans_ext(projectname())),
      '_venn_barplot.',
      input$formvenbar,
      sep = ''
    )
  },
  content <- function(file) {
    if (input$formvenbar == "pdf")
      
      pdf(file,
          width = 16,
          height = 7,
          pointsize = 12)
    
    else if (input$formvenbar == "png")
      png(
        file,
        width = 1600,
        height = 700,
        units = "px",
        pointsize = 12,
        res = 100
      )
    else
      eps(file,
          width = 16,
          height = 7,
          pointsize = 12)
    
    print(plottopgenes())
    
    dev.off()
  })
  
})