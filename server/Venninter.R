### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#' vennchoice is a reactive function that return user's selected comparisons
#'
#' @param intscol character input
#'
#' @return character vector
#' @export
#'

vennchoice <- reactive({
  if (is.null(input$intscol))
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


jvenndup <- reactiveValues()

#' vennfinal is a reactive function which return a list of data frame corresponding to the computationnal mean of each logFC for the possible logical relations between a finite collection of different sets
#' and a data frame with as primary key the probenames associated with the corresponding gene names and logFC
#'
#'
#' @param vennchoice reactive character vector
#' @param subsetstat dataframe subset of the alltoptable
#' @param dispvenn character input between probes and genes
#' @param venninter multiple lists of probenames
#'
#' @return a list of two data frames
#' @export
#'


vennfinal <- reactive({

  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(choix_cont(), 'Set your thresholds and then select your comparison to display the Venn diagram!')%next%
      need(input$selcontjv ,'You need to click on a number (Venn diagram) to display the data table!'))

  outputjvennlist = list()

  if(!input$Allcont && !input$dispvenn == "genes")
    outputjvenntab <- filterjvenn(input$jvennlist, input$selcontjv, csvf()[[3]],dataid(), input$dispvenn )
  else if (input$Allcont && !input$dispvenn == "genes")
    outputjvenntab <- filterjvenn(input$jvennlist, choix_cont(),  csvf()[[3]], dataid(), input$dispvenn)
  else if (!input$Allcont && input$dispvenn == "genes")
    outputjvenntab <- filterjvenn(input$jvennlist, input$selcontjv,   csvf()[[3]], dataid(),  input$dispvenn, unlist(vennlist()[[1]]))
  else
    outputjvenntab <- filterjvenn(input$jvennlist, choix_cont(), csvf()[[3]] ,dataid(), input$dispvenn, unlist(vennlist()[[1]]))

  if(input$Notanno){
    outputjvenntab <- outputjvenntab %>%  filter(., !grepl("^chr[A-z0-9]{1,}:|^ENSMUST|^LOC[0-9]{1,}|^[0-9]{4,}$|^A_[0-9]{2}_P|^NAP[0-9]{4,}|[0-9]{7,}",GeneName)) %>% as.data.frame()
  }

  outputjvennlist[[1]] <- outputjvenntab
  if(!input$Allcont)
    mycont =input$selcontjv
  else
    mycont =choix_cont()
  if(input$dispvenn == "genes"){
    outputjvennlist[[2]] <- meanrankgenes(outputjvenntab, stat = prefstat$greppre[[2]], multcomp = mycont , jvenn=  T)

  jvenndup$duplicated <- outputjvenntab %>%
      group_by(GeneName) %>%
      filter(n()>1)
  }

  return(outputjvennlist)
})


output$venntitle <- renderText({
  req(input$selcontjv)
  if(any(grepl("probes|transcripts", input$dispvenn)) )
    mytitlevenn <<- print(paste("Barplot showing the top ", input$topgenes ," genes"))
  else
    mytitlevenn <<- print(paste("Barplot showing the computationnal logFC mean of the top " ,input$topgenes , " genes before the rendering table"))
})


output$venngenesbef <- renderText({
  req(input$selcontjv)
  if(input$dispvenn == "genes")
  mytitlevenn <<- print(paste("Barplot showing the computationnal logFC mean of the top " ,input$topgenes , " genes after the rendering table"))

})


output$dfvenn <- renderText({
  req(input$selcontjv)
  if(any(grepl("probes|transcripts", input$dispvenn)) )
    mytitlevenn <<- print(paste("Table showing the ", ifelse(dataid() == "ProbeName", "probes", "transcripts")  , "and genes associated with their respective logFC for the intersection(s) selected"))
  else
    mytitlevenn <<- print(paste("Table showing the genes associated with the average logFC for the intersection(s) selected"))

})


#' venntopgenes is a reactive function which aim is to return the user's input top n genes
#'
#' @param filtertopjvenn numeric input
#'
#' @return numeric input
#' @export
#'

venntopgenes <- reactive({
  if (is.null (input$filtertopjvenn))
    return(NULL)
  else
    return(input$filtertopjvenn)
})



output$downloadvennset = downloadHandler('venns-filtered.csv',
  content = function(file) {
    s = input$vennresinter_rows_all
    if(any(grepl("probes|transcripts", input$dispvenn)) )
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
    mycont <- paste0(prefstat$greppre[[2]], choix_cont())
  else
    mycont <- paste0(prefstat$greppre[[2]], input$selcontjv)

  if(any(grepl("probes|transcripts", input$dispvenn)) &&  (is.null(input$filteredcompjv) || input$filteredcompjv == "" ) )
    myplot <- topngenes(vennfinal()[[1]][input$vennresinter_rows_all, , drop = FALSE],mycont, venntopgenes(), input$dispvenn)
  else if(input$dispvenn == "genes" &&  (is.null(input$filteredcompjv) || input$filteredcompjv == "" ))
    myplot <- topngenes(vennfinal()[[2]][input$vennresinter_rows_all, , drop = FALSE],mycont, venntopgenes(), input$dispvenn)
  else
    myplot <- topngenes(topngenesDT()[input$vennresinter_rows_all, , drop = FALSE],mycont, venntopgenes(), input$dispvenn)



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

callModule(downoutputfiles, "savebarvenn", projectname = projectname , suffix= "_venn_barplot." , data = plottopgenes , w =16, h = 7  )


####################
# Addition for report
# MA0439
####################


filteredcolvenn <- reactive ({

  req(vennfinal(), venntopgenes(), input$selcontjv, input$dispvenn)

  filteredcol = na.omit((as.numeric(gsub("([0-9]+).*$", "\\1", unlist(input$vennresinter_state$order)))))
  if(any(grepl("probes|transcripts", input$dispvenn)))
    colnamefil = colnames(vennfinal()[[1]][filteredcol])
  else
    colnamefil = colnames(vennfinal()[[2]][filteredcol])

  colnamefil = gsub(
    pattern = prefstat$greppre[[2]] ,
    replacement = "",
    x = colnamefil,
    perl = T
  )

  return(colnamefil)
})


topngenesDT <- reactive ({

  req(input$filteredcompjv, vennfinal())

  topngenesDT <- csvf()[[3]] %>% select( dataid() , GeneName, paste0(ifelse(input$filtermethjvenn == "FDR", prefstat$greppre[[1]] , prefstat$greppre[[3]]), input$filteredcompjv)) %>%
  {if (input$dispvenn == "genes") filter ( ., GeneName  %in% vennfinal()[[2]]$GeneName) else filter(., .[[1]]  %in% vennfinal()[[1]][[1]] )}
  topngenesDT$rank <- topngenesDT %>% select( paste0(ifelse(input$filtermethjvenn == "FDR",prefstat$greppre[[1]] , prefstat$greppre[[3]]), input$filteredcompjv)) %>% rank(.)
  topngenesDT <- topngenesDT %>% arrange( desc(rank) ) %>% top_n(-input$filtertopjvenn, rank)
  if (input$dispvenn == "genes")
    topngenesDT <- vennfinal()[[2]] %>% filter (GeneName %in% topngenesDT$GeneName)
  else
    topngenesDT <-vennfinal()[[1]] %>% filter (vennfinal()[[1]][[1]] %in% topngenesDT[[1]])

 return(topngenesDT)
})


output$filtercompjvenn <- renderUI({

  req( input$selcontjv)
  tags$div(
    class = "jvennfiltparam",selectInput('filteredcompjv',
                                     'filter comp', choices = c("", input$selcontjv), selected = ""))
})


output$dispidvenn <- renderUI( ##validate

  selectInput("dispvenn",
              label = paste("Choose if you want to display", ifelse(dataid() == "ProbeName", "probes", "transcripts") ,  "or genes"),
              choices = c(ifelse(dataid() == "ProbeName", "probes", "transcripts"), "genes"))
)
