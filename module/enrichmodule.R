#' selSpecies is a shiny widget which aims is to select the Species for the functional analysis
#'
#' @param id Shiny id
#'
#' @return Widget in the gui
#'
#' @export

selSpecies <- function(id) {
  ns <- NS(id)
  selectInput( ns("selspecies"), label = "Choose your Species:", selected = "Mus musculus",
              c("Mouse" = "Mus musculus", "Human" = "Homo sapiens", "Rat" = "Rattus norvegicus", "C. elegans" = "Caenorhabditis elegans",
                "Zebrafish" = "Danio rerio",  "Pig" = "Sus scrofa",
                "Chicken" = "Gallus gallus", "Chimpanzee" = " Pan troglodytes" ))
}

#' runAnalysis is a shiny widget action button for run the functional analysis
#'
#' @param id Shiny id
#'
#' @return Widget in the gui
#'
#' @export
#'

runAnalysis <- function(id) {
  ns <- NS(id)
  actionButton(ns("GOana"), label = "Run Analysis",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
}

#' catHm is a shiny widget which aims is to select the categories for the functional analysis
#'
#' @param id Shiny id
#'
#' @return Widget in the gui
#'
#' @export
#'

catHm  <- function(id, label = "Run Analysis") {
  ns <- NS(id)

  selectInput(ns('catinfo'),'Category: ',
              choices =  c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY"),
              selected=  c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY"),
              multiple = TRUE
  )
}


#' entrezIdstosymb is a shiny module which aims that takes as input a dataframe with entrez ids and return a dataframe with gene symbols
#'
#' @param input Internal
#' @param output Internal
#' @param session Internal
#' @param data A statistical output dataframe of the DAVID web service
#' @param cutgo A numeric value to specify which cluster is selected
#' @param rows_selected A reactive character vector of the selected rows within the DAVID output table
#'
#' @return A reactive dataframe that associates Genes (genesymbols) with the corresponding term
#'
#' @export
#'

entrezIdstosymb <- function(input, output, session , data, cutgo, rows_selected ) {


myentreztosymb <- reactive({

  req(data())
  myselectedrows = (data()$mygodavid[[as.numeric(cutgo())]][rows_selected(), c("Genes", "Term"),  drop = FALSE])
  if(length(myselectedrows["Genes"][[1]])>0){
    myentreztosymb = lapply(1:NROW(myselectedrows),function(x){
      myselectedrows$Genes[[x]] %>% strsplit( ", ") %>% unlist() %>% mget(x= ., envir = data()$packages , ifnotfound = NA) %>%  unlist() %>%
        unique() %>% cbind(myselectedrows$Term[[x]]) %>% as.data.frame() %>% setNames(., c("Genes", "Term"))

    })

    return(myentreztosymb)
  }
  else{

    return(NULL)
  }


  })
}


#' queryDavid is a shiny module which aims is to query the DWS for performing Functional Annotation or Gene Functional Classification
#'
#' @param input Internal
#' @param output Internal
#' @param session Internal
#' @param data A reactive dataframe(s)
#' @param parent_session External session
#' @param tabsetpanid Widget tabset panel
#' @param tabPanel Plot tabset panel
#' @param hmana A boolen value (T for heatmap and F for Venn)
#'
#' @return A reactive statistical dataframe of S4 class
#'
#' @export
#'


queryDavid <- function(input, output, session , data, parent_session, tabsetpanid, tabPanel, hmana= F ) {


resAnalysis <- eventReactive(input$GOana , {

  req(data())

  enrichmanalysis <- reactiveValues()
  enrichmanalysis$packages <- Species()[[2]]



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
                     mygodavid = probnamtoentrez(data(), Species()[[1]], gohm = hmana ) %>%
                    {if (!hmana) davidqueryvenn(., input$selspecies) else davidquery(. , input$selspecies, input$catinfo) } %>% withCallingHandlers(error = timeoutdav)
                 }, warning = function(e) {

                   shinyjs::alert("David's server is busy")
                   warning("David's server is busy")
                   return(NULL)

                 })
               })

  if(!hmana)
    pdf(NULL)

  enrichmanalysis$mygodavid <- mygodavid

  updateTabsetPanel(session = parent_session, inputId=  tabsetpanid,
                    selected = tabPanel)


  return(enrichmanalysis)
})


Species <- reactive({

  if ( input$selspecies == "Homo sapiens") { # human
    library("org.Hs.eg.db")
    return(list(org.Hs.egALIAS2EG, org.Hs.egSYMBOL))
  }
  else if ( input$selspecies == "Mus musculus" ) { # Mouse
    library("org.Mm.eg.db")
    return( list(org.Mm.egALIAS2EG, org.Mm.egSYMBOL))
  }
  else if (input$selspecies == "Danio rerio") { #Zebra fish
    library("org.Dr.eg.db")
    return(list(org.Dr.egALIAS2EG, org.Dr.egSYMBOL))
  }
  else if (input$selspecies == "Gallus gallus") { # chicken
    library("org.Gg.eg.db")
    return(list(org.Gg.egALIAS2EG, org.Gg.egSYMBOL))
  }
  else if ( input$selspecies == "equCab2") { # horse
    library("org.Gg.eg.db")
    return(list(org.Gg.eg.dbALIAS2EG))
  }
  else if (input$selspecies == "Caenorhabditis elegans") { # cC elegans
    library("org.Ce.eg.db")

    return(list(org.Ce.egALIAS2EG, org.Ce.egSYMBOL))
  }
  else if ( input$selspecies == "Rattus norvegicus") { # Rat
    library("org.Rn.eg.db")
    return(list(org.Rn.egALIAS2EG, org.Rn.egSYMBOL ))
  }
  else if (input$selspecies == "Sus scrofa") { # Pig
    library("org.Ss.eg.db")
    return(list(org.Ss.egALIAS2EG, org.Ss.egSYMBOL))
  }

})

  return(reactive({
    validate(need((input$GOana),'You need to click on the run Analysis button!'))
    resAnalysis()
  }))

}


