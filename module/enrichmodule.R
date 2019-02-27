selSpecies <- function(id) {
  ns <- NS(id)
  selectInput( ns("selspecies"), label = "Choose your Species:", selected = "Mus musculus",
              c("Mouse" = "Mus musculus", "Human" = "Homo sapiens", "Rat" = "Rattus norvegicus", "C. elegans" = "Caenorhabditis elegans",
                "Zebrafish" = "Danio rerio",  "Pig" = "Sus scrofa",
                "Chicken" = "Gallus gallus", "Chimpanzee" = " Pan troglodytes" ))
}

runAnalysis <- function(id) {
  ns <- NS(id)
  actionButton(ns("GOana"), label = "Run Analysis",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")
}

catHm  <- function(id, label = "Run Analysis") {
  ns <- NS(id)
  selectInput(ns('catinfo'),'Category: ',
              choices =  c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY"),
              selected=  c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY"),
              multiple = TRUE
  )
}

queryDavid <- function(input, output, session , data, parent_session, tabsetpanid, tabPanel, hmana= F, case ) {


resAnalysis <- eventReactive(input$GOana , {
  
  req(data())
  
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
                   mygodavid = probnamtoentrez(switch(case, data() ,data()[[1]]$GeneName), Species()[[1]], gohm = hmana ) %>% #probnamtoentrezvenn(data() , Species()[[1]]) %>% 
                     {if (!hmana) davidqueryvenn(., input$selspecies) else davidquery(. , input$selspecies, input$catinfo) } %>% withCallingHandlers(error = timeoutdav)
                 }, warning = function(e) {
                   
                   shinyjs::alert("David's server is busy")
                   warning("David's server is busy")
                   return(NULL)
                   
                 })
               })
  
  if(!hmana)
    pdf(NULL)
  
  updateTabsetPanel(session = parent_session, inputId=  tabsetpanid,
                    selected = tabPanel)
  
  return(mygodavid)
})


Species <- reactive({
  
  if ( input$selspecies == "Homo sapiens") {# human
    library("org.Hs.eg.db")
    return(list(org.Hs.egALIAS2EG, org.Hs.egSYMBOL))
  }
  else if ( input$selspecies == "Mus musculus" ) {# Mouse
    library("org.Mm.eg.db")
    return( list(org.Mm.egALIAS2EG, org.Mm.egSYMBOL))
  }
  else if (input$selspecies == "Danio rerio") {#Zebra fish
    library("org.Dr.eg.db")
    return(list(org.Dr.egALIAS2EG, org.Dr.egSYMBOL))
  }
  else if (input$selspecies == "Gallus gallus") {# chicken
    library("org.Gg.eg.db")
    return(list(org.Gg.egALIAS2EG, org.Gg.egSYMBOL))
  }
  else if ( input$selspecies == "equCab2") {# horse
    library("org.Gg.eg.db")
    return(list(org.Gg.eg.dbALIAS2EG))
  }
  else if (input$selspecies == "Caenorhabditis elegans") {# cC elegans
    library("org.Ce.eg.db")
    
    return(list(org.Ce.egALIAS2EG, org.Ce.egSYMBOL))
  }
  else if ( input$selspecies == "Rattus norvegicus") {# Rat
    library("org.Rn.eg.db")
    return(list(org.Rn.egALIAS2EG, org.Rn.egSYMBOL ))
  }
  else if (input$selspecies == "Sus scrofa") {# Pig
    library("org.Ss.eg.db")
    return(list(org.Ss.egALIAS2EG, org.Ss.egSYMBOL))
  }
  
})

  return(reactive({
    validate(need((input$GOana),'You need to click on the run Analysis button!'))
    resAnalysis()
  }))
  
}


