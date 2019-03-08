### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


url <- reactiveValues()
gores <- reactiveValues()

observe({
  req(url)

  output$DAVID <- renderUI({
    shiny::actionButton(
      inputId = 'DAVID',
      "Open DAVID",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
      onclick = paste("window.open(", url$myurl)
    )
  })
})


observe({

  #' totaclust is a reactive function which aim is to dynamically return a widget object of selectinput type ranging from 1 to the maximum number of cluster
  #'
  #' @param hmobj data frame of the significant genes associated with the corresponding cluster index
  #'
  #' @return selectInput widget
  #' @export
  #'


  totalclust <- reactive({
    req(hmobj$hm)
    n <- unique(hmobj$hm$cluster)
    selectInput("cutgo",
                "Choose your cluster",
                choices =  seq(1, NROW(n) , by = 1))
  })


  output$cutgo <- renderUI({
    totalclust()
  })

})


#' clustergrep is a reactive function which aim is to return a list of genes for the selected cluster without the non-annotated genes
#'
#' @param hm data frame of the significant genes associated with the corresponding cluster index
#' @param cutgo a numeric input
#'
#' @return list of genes
#' @export
#'
#'

clustergrep <- reactive({

  req(hmobj$hm, input$cutgo)

  genlist <- hmobj$hm[!duplicated(hmobj$hm$GeneName),] %>%
    dplyr::select(cluster, GeneName)   %>%
    filter(cluster == input$cutgo)

  mygensymb = genlist$cluster %>%
    length() %>%
    matrix(1, .) %>%
    as.double() %>%
    setNames(genlist$GeneName) %>%
    names() %>% as.list() %>%
    .[lapply(., function(x)
      length(grep("chr", x, value = FALSE))) == 0]

  return(mygensymb)
})


davidwebservice <- callModule(queryDavid, "hmanalysis", data = reactive(hmobj$hm) , parent_session = session, tabsetpanid= "heatmapmainp", tabPanel= "maingo", hmana = T)


#' davidurl is a reactive function that aim is to return an url of grouped genes
#'
#' @param clustergrep list of genes
#'
#' @return character
#' @export
#'

davidurl <- reactive({
  req(clustergrep())

  source_python('./python/enrichmurl.py')
  mydavurl = enrichmentdav(clustergrep())
  mygloburl <- paste(`mydavurl`, ",", "'_blank')")

  return(mygloburl)
})


observe({
  req(davidurl())
  url$myurl = davidurl()
})

myentreztosymb <- callModule(entrezIdstosymb, "hmanalysis", data = davidwebservice , cutgo = reactive(input$cutgo), rows_selected= davidRselected) #reactive(input$davidgo_rows_selected) )


output$printmessage <- renderPrint({
  req(davidwebservice())
  cat("You can select the rows in the table above in order to display the gene names")
  cat("\n")
  cat("\n")
})


output$printselected <- renderPrint({

  req(myentreztosymb())

    for(i in 1:length(myentreztosymb())){
      cat(paste("GOID and Term: " , unique(myentreztosymb()[[i]]$Term)))
      cat("\n")
      cat("Genes: ")
      cat(paste( myentreztosymb()[[i]]$Genes, collapse = " ,"))
      cat("\n")
      cat("\n")
    }

})


myresdavitab <- reactive({
  req(davidwebservice())
  mygotabres(davidwebservice()$mygodavid[[as.numeric(input$cutgo)]], input$enrichbased)
})


output$titlegomain <- renderText({
  req(input$GO)
  mytitlevenn <<- print("DAVID Gene Set Enrichment Analysis")
})


output$titlegotop <- renderText({
  req(input$GO)
    mytitlevenn <<- print("Top 10 Significantly Enriched GO and KEGG Terms")
})

callModule(downoutputables, "savegohmdavxlsx", projectname = projectname , suffix = "_go.xlsx" , data = reactive(davidwebservice()$mygodavid) , xlsx = T )


