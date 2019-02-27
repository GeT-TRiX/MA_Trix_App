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

#' davidwebservice is an eventreactive function which aim is to querrying the DWS to return a dataframe summary
#'
#' @param GO clickable event
#' @param hm data frame of the significant genes associated with the corresponding cluster index
#' @param Species list of annotated elements
#' @param catinfo vector of enrichment categories, BP, CC, MF, Kegg
#'
#' @return data frame
#' @export
#'
#'


davidwebservice <- eventReactive(input$GO, {

    req(hmobj$hm)

    withProgress(message = 'Performing GO enrichment:',
                 value = 0, {
                   n <- NROW(50)
                   for (i in 1:n) {
                     incProgress(1 / n, detail = "Please wait...")
                   }
                   library(RDAVIDWebService)

                   timeoutdav <- function(y)
                     if (any(grepl("Read timed out", y)))
                       invokeRestart("muffleWarning")

                   tryCatch({
                     mygodavid = probnamtoentrez(hmobj$hm, Specieshm()[[1]]) %>%
                       davidquery(input$Species, input$catinfo) %>% withCallingHandlers(error = timeoutdav)
                   }, warning = function(e) {
                     warning("David's server is busy")

                     return(cbind("David's server is busy") %>% as.data.frame() %>% setNames("Error"))

                   })
                 })


    updateTabsetPanel(session, "heatmapmainp",
                      selected = "maingo")

    return(mygodavid)
  })

#davidwebservice <- callModule(queryDavid, "vennanalysis", data = vennfinal , parent_session = session )



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





#' myentreztosymb is a reactive function which aim is to convert entrez ID to GENE  the selected rows in the output data table
#'
#' @param davidwebservice data frame
#' @param cutgo a numeric input
#' @param davidgo_rows_selected selected rows
#' @param Species list of annotated elements
#'
#' @return a data frame
#' @export
#'


myentreztosymb <- reactive({

  req( davidwebservice())


  myselectedrows = (davidwebservice()[[as.numeric(input$cutgo)]][input$davidgo_rows_selected, c("Genes", "Term"),  drop = FALSE])

  if(length(myselectedrows["Genes"][[1]])>0){

    myentreztosymb = lapply(1:NROW(myselectedrows),function(x){
      myselectedrows$Genes[[x]] %>% strsplit( ", ") %>% unlist() %>% mget(x= .,envir = Specieshm()[[2]],ifnotfound = NA) %>%  unlist() %>%
        unique() %>% cbind(myselectedrows$Term[[x]]) %>% as.data.frame() %>% setNames(., c("Genes", "Term"))

    })

    return(myentreztosymb)
  }
  else{

    return(NULL)
  }

})

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
  mygotabres(davidwebservice()[[as.numeric(input$cutgo)]], input$enrichbased)
})


output$titlegomain <- renderText({
  req(input$GO)
  mytitlevenn <<- print("DAVID Gene Set Enrichment Analysis")
})


output$titlegotop <- renderText({
  req(input$GO)
    mytitlevenn <<- print("Top 10 Significantly Enriched GO and KEGG Terms")
})


output$savegohmdavxlsx = downloadHandler(filename <- function() { paste0(basename(file_path_sans_ext(projectname())), '_go.',"xlsx", sep = '')},
  content = function(file) {

    withProgress(message = 'Creation of the xlsx table:',
                 value = 0, {
                   n <- NROW(50)
                   for (i in 1:n) {
                     incProgress(1 / n, detail = "Please wait...")
                   }


    library(xlsx)

    for (i in 1:length(davidwebservice())) {
      if (i == 1)
        write.xlsx(file = file,
                   davidwebservice()[[i]],
                   sheetName = paste("Cluster", i))
      else
        write.xlsx(
          file = file,
          davidwebservice()[[i]],
          sheetName = paste("Cluster", i),
          append = TRUE
        )
      }
    })

  }
)


#' Species is a reactive function which aim is to return annotated packages for a specific genome
#'
#' @param Species character input
#' @param Speciesvenn character input
#'
#' @return
#' @export
#'


Specieshm <- reactive({
  if (input$Species == "Homo sapiens" ) {# human
    library("org.Hs.eg.db")
    return(list(org.Hs.egALIAS2EG, org.Hs.egSYMBOL))
  }
  else if (input$Species == "Mus musculus"  ) { # Mouse
    library("org.Mm.eg.db")
    return( list(org.Mm.egALIAS2EG, org.Mm.egSYMBOL))
  }
  else if (input$Species == "Danio rerio" ) {#Zebra fish
    library("org.Dr.eg.db")
    return(list(org.Dr.egALIAS2EG, org.Dr.egSYMBOL))
  }
  else if (input$Species == "Gallus gallus" ) {# chicken
    library("org.Gg.eg.db")
    return(list(org.Gg.egALIAS2EG, org.Gg.egSYMBOL))
  }
  else if (input$Species == "equCab2" ) {# horse
    library("org.Gg.eg.db")
    return(list(org.Gg.eg.dbALIAS2EG))
  }
  else if (input$Species == "Caenorhabditis elegans" ) {# cC elegans
    library("org.Ce.eg.db")

    return(list(org.Ce.egALIAS2EG, org.Ce.egSYMBOL))
  }
  else if (input$Species == "Rattus norvegicus" ) {# Rat
    library("org.Rn.eg.db")
    return(list(org.Rn.egALIAS2EG, org.Rn.egSYMBOL ))
  }
  else if (input$Species == "Sus scrofa") {# Pig
    library("org.Ss.eg.db")
    return(list(org.Ss.egALIAS2EG, org.Ss.egSYMBOL))
  }

})
