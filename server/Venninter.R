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
  return(myelist)
})


vennfinal <- reactive({
  req(vennchoice())
  if (is.null(vennchoice))
    return(NULL)
  
  
  reordchoice <- vennchoice() %>%
    factor(levels = names(adjusted()[[1]][,-1])) %>%
    sort() %>%
    paste(collapse = "")
  
  
  resfinal = csvf()[[3]] %>%
    filter(ProbeName %in% venninter()[[reordchoice]]) %>%
    select(ProbeName, GeneName, paste0("logFC_", vennchoice())) %>%
    mutate_if(is.numeric, funs(format(., digits = 3)))
  
  mycont = paste0("logFC_", vennchoice())
  if(input$meandup){
    for (i in mycont) {
      resfinal[[i]] = as.numeric(as.character(resfinal[[i]]))
    }
  
    resfinal <- resfinal[,-1] %>% as.data.table() %>% .[,lapply(.SD,mean),"GeneName"] 
    resfinal = as.data.frame(resfinal)
  }
  #mutate_if(is.numeric, funs(formatC(., format = "f")))
  
  
  return(resfinal)
})


output$topgenesvenn <- renderUI({
  req(vennfinal(), vennchoice())
  
  numericInput('topgenes',
               'Top genes',
               50,
               min = 1,
               max = length(vennfinal()$ProbeName))
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
    write.csv2(vennfinal()[s, , drop = FALSE], file)
  }
)


plottopgenes <- eventReactive(input$topdegenes, {
  req(vennfinal(), vennchoice(), venntopgenes())
  mycont = paste0("logFC_", vennchoice())
  myplot <-
    topngenes(vennfinal()[input$vennresinter_rows_all, , drop = FALSE], mycont, venntopgenes(), input$meandup)
  return(myplot)
  
})

observeEvent(input$topdegenes, {
  isolate(output$barplotvenn <- renderPlot({
    req(plottopgenes())
    plotOutput(plottopgenes())
    
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