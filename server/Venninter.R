### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


#' vennchoice is a reactive function that return user's selected comparisons
#'
#' @param intscol character input
#'
#' @return character vector
#' @export
#'


vennchoice <- reactive({
  if (is.null (input$intscol))
    return(NULL)
  else
    return(input$intscol)
})


#' venninter is a reactive function which aim is to return a set of lists for each possible logical relations between a finite collection of different sets
#'
#' @param vennlist list of probenames
#' @param user_cont character vector
#'
#' @return multiple lists
#' @export
#'

venninter <- reactive({
  req(vennlist(), user_cont())
  myelist <- setvglobalvenn(vennlist()[[1]], user_cont())
  
  return(myelist)
})


#' vennfinal is a reactive function which return a list of data frame corresponding to the computationnal mean of each logFC for the possible logical relations between a finite collection of different sets
#' and a data frame with as primary key the probenames associated with the corresponding gene names and logFC
#' 
#'
#' @param vennchoice reactive character vector
#' @param adjusted dataframe subset of the alltoptable
#' @param dispvenn character input between probes and genes
#' @param venninter multiple lists of probenames
#'
#' @return a list of two data frames
#' @export
#'

vennfinal <- reactive({
  
  
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(choix_cont(), 'You need to choose your comparison to display the Venn diagram!')%next%
      need(input$selcontjv ,'You need to click on a number (Venn diagram) to display the data table!'))
  
  if (is.null(vennchoice))
    return(NULL)
  

  reslist = list()
  reordchoice <- input$selcontjv %>%
    factor(levels = names(adjusted()[[1]][,-1,drop = FALSE])) %>%
    sort() %>%
    paste(collapse = "")


  if(!input$Allcont)
    resfinal <- csvf()[[3]] %>%
      filter(ProbeName %in% venninter()[[reordchoice]]) %>%
      select(ProbeName, GeneName, paste0("logFC_",  input$selcontjv)) %>%
      mutate_if(is.numeric, funs(format(., digits = 3)))
  else
    resfinal <- csvf()[[3]] %>%
    filter(ProbeName %in% venninter()[[reordchoice]]) %>%
    select(ProbeName, GeneName, paste0("logFC_", choix_cont())) %>%
    mutate_if(is.numeric, funs(format(., digits = 3)))

  
  if(input$Notanno){
    resfinal <- resfinal %>%  filter(., !grepl("^chr[A-z0-9]{1,}:|^ENSMUST|^LOC[0-9]{1,}|^[0-9]{4,}$|^A_[0-9]{2}_P|^NAP[0-9]{4,}|[0-9]{7,}",GeneName)) %>% as.data.frame()
  }
  
  reslist[[1]] <- resfinal
  
  if(!input$Allcont)
    mycont = paste0("logFC_",input$selcontjv)
  else 
    mycont = paste0("logFC_",choix_cont())

  if(input$dispvenn == "genes"){
    options(datatable.optimize=1)
    for (i in mycont) {
      print(i)
      resfinal[[i]] = as.numeric(as.character(resfinal[[i]]))
    }
    
    if(input$Notanno){
      resfinal <- resfinal[,-1] %>% as.data.table() %>% .[,lapply(.SD,function(x) list(mean=round(mean(x), 3))),"GeneName"] %>% filter(., !grepl( "^chr[A-z0-9]{1,}:",GeneName)) %>% as.data.frame()
    }
    else 
      resfinal <- resfinal[,-1] %>% as.data.table() %>% .[,lapply(.SD,function(x) list(mean=round(mean(x), 3))),"GeneName"] %>% as.data.frame()
    
    reslist[[2]] <- resfinal
  }

  return(reslist)
})




output$topgenesvenn <- renderUI({

  req( input$selcontjv)
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
    mytitlevenn <<- print(paste("Table showing the ProbeNames and GeneNames associated with their respective logFC for the intersection(s) selected"))
  else
    mytitlevenn <<- print(paste("Table showing the GeneNames associated with the average logFC for the intersection(s) selected"))
    
  
})

output$dfvennbef <- renderText({
  req(input$topgenes)
  if(input$dispvenn == "genes")
    mytitlevenn <<- print(paste("Table showing the GeneNames associated with their respective logFC for the intersection(s) selected"))
  
})


#' venntopgenes is a reactive function which aim is to return the user's input top n genes
#'
#' @param topgenes numeric input
#'
#' @return numeric input
#' @export
#'

venntopgenes <- reactive({
    if (is.null (input$topgenes))
      return(NULL)
    else
      return(input$topgenes)
  })

output$downloadvennset = downloadHandler('venns-filtered.csv',
  content = function(file) {
    s = input$vennresinter_rows_all
    if(input$dispvenn == "probes")
      write.csv2(vennfinal()[[1]][s, , drop = FALSE], file)
    else
      write.csv2(vennfinal()[[2]][s, , drop = FALSE], file)
  }
)

#' plottopgenes is an event reactive function which aim is to plot the top n genes selected by the user from the rendering data table
#'
#' @param topdegenes clickable event button
#' @param venntopgenes numeric input
#' @param vennchoice reactive character vector
#' @param vennfinal a list of two data frames
#' @param dispvenn character input between probes and genes
#'
#' @return ggplot object
#' @export
#'

plottopgenes <- eventReactive(input$topdegenes, {
  req(vennfinal(), venntopgenes(), input$selcontjv)

  if(input$Allcont)
    mycont <- paste0("logFC_", choix_cont())
  else 
    mycont <- paste0("logFC_", input$selcontjv)
    
  print(mycont)
  
  if(input$dispvenn == "probes")
    myplot <- topngenes(vennfinal()[[1]][input$vennresinter_rows_all, , drop = FALSE],mycont, venntopgenes(), input$dispvenn)
  else
    myplot <- topngenes(vennfinal()[[2]][input$vennresinter_rows_all, , drop = FALSE],mycont, venntopgenes(), input$dispvenn)
  
  
  return(myplot)
})


#' plottopgenesmean is an event reactive function which aim is to plot the top n genes selected by the user from the rendering data table with the average logFC
#'
#' @param topdegenes clickable event button
#' @param venntopgenes numeric input
#' @param vennchoice reactive character vector
#' @param vennfinal a list of two data frames
#'
#' @return ggplot object
#' @export
#'

plottopgenesmean <- eventReactive(input$topdegenes, {
  req(vennfinal(), vennchoice(), venntopgenes())
  #myselcont <- ifelse(input$Allcont, choix_cont(), input$selcontjv)
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