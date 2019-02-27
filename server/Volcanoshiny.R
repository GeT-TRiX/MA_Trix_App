### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#######################################################
######## grep gene family                             #
#######################################################


genetodisplay <- reactive({
  if(is.null(input$fillvolc))
    return(NULL)
  else{
    if(!input$fillvolc == "")
      mycol = gsub("^\\s+|\\s+$", "", unlist(strsplit(input$fillvolc, ",")))
    else
      mycol = ""
    return(toupper(mycol))
  }
})


family_input <- reactive({
  input$findfamily
})


family_d <- shiny::debounce(family_input, 1200) # Delay input debounche also pour search genes

top_volc <- reactive({
  input$topvolc
})


top_volcd <- shiny::debounce(top_volc, 1000) # Delay input debounche also pour search genes



#######################################################
######## parse gene symbol for min and maj            #
#######################################################


familytopdisp <- reactive({
  if(is.null(family_d))
    return(NULL)
  else{
    if(!family_d() == ""){
      genfam = grep(pattern = toupper(family_d()), toupper(csvf()[[3]]$GeneName)) %>% slice(csvf()[[3]],.)%>% select(GeneName)  %>% unlist() %>% as.character()
    }
    else
      genfam =""
    return(toupper(genfam))
  }
})


#######################################################
######## Desactivate volcano input dep on user choice #
#######################################################


observe({

  if(input$findfamily != ""){
    shinyjs::disable("topvolc")
    shinyjs::disable("fillvolc")
  }
  else if(input$fillvolc != ""){
    shinyjs::disable("topvolc")
    shinyjs::disable("findfamily")
  }
  else if(!is.na(input$topvolc)){
    shinyjs::disable("findfamily")
    shinyjs::disable("fillvolc")
  }
  else{
    shinyjs::enable("topvolc")
    shinyjs::enable("findfamily")
    shinyjs::enable("fillvolc")
  }

})


#################################################
######## Plot and save volcano                  #
#################################################

volcano <- reactive({
  req(csvf(),input$volcacomp )
  EnhancedVolcano(csvf()[[3]], lab= csvf()[[3]]$GeneName , x = paste0(prefstat$greppre[[2]],input$volcacomp) ,
                  y = paste0(ifelse(input$method == "FDR",prefstat$greppre[[1]] ,prefstat$greppre[[3]]),input$volcacomp),
                  topgenes = top_volcd(),DrawConnectors= T,#DrawConnectors = ifelse(is.na(input$topvolc),T,F),
                  pCutoff = input$volcpval ,FCcutoff = input$volcfc ,transcriptPointSize = input$volcpt,transcriptLabSize = input$volclab,
                  title =  gsub("-"," versus " ,input$volcacomp),cutoffLineType = "twodash", findfamily =  ifelse(familytopdisp() == "" , NA,familytopdisp()),regulationvolc = input$regulationvolc,
                  displaylab = ifelse(genetodisplay() =="", NA, genetodisplay()),legendLabSize = 10,
                  cutoffLineCol = "black",cutoffLineWidth = 1,legend=c("NS","Log (base 2) fold-change","P value",
                                                                       "P value & Log (base 2) fold-change"))
})

volcboth <- reactiveValues()
volcobj <- reactiveValues()


output$volcanoplot <- renderPlot({

  validate(need(csvf(), 'You need to import data to visualize this plot!'))

  req(volcano())
  volcboth$tot <- volcano()
  volcobj$plot <- volcboth$tot[[1]]
  volcobj$dt <-volcboth$tot[[2]]
  volcobj$plot

},  height = plotHeight)

output$compvolc <- renderUI({
  req(subsetstat())
  selectInput("volcacomp", "Choose a comparison", choices = colnames(subsetstat()[[1]]))
})


callModule(downoutputfiles, "savevolc", projectname = projectname , suffix= "_volcano." , data = reactive(volcano()[[1]]),  w = 12, h = 12 ,volcform = T)

vocfilt <- reactive({
  req(csvf(), top_volcd(),  volcanocomp())
  volcobj$top <- meanrankgenes(volcobj$dt, prefstat$greppre[[2]] , input$volcacomp,  volcanocomp(), input$regulationvolc  )
  return(volcobj$top)
})


volcanocomp <- reactive({
  return(c(input$volcacomp, input$addvolcacomp))
})

volcplototp <- reactive({
  req(vocfilt())

  selcomp <-  paste0(prefstat$greppre[[2]],  volcanocomp())
  myplot <- topngenes(vocfilt(), selcomp ,top_volcd()  ,meandup = "genes")
  return(myplot)
})




observe({

output$barplotvolc <- renderPlot({
  validate(need(top_volcd(), 'Add an input in the max number of genes widget!'))
    req(volcplototp())
    plotOutput(volcplototp())
  })

})

callModule(downoutputfiles, "savebarvolc", projectname = projectname , suffix= "_volc_barplot." , data = volcplototp, w=16 , h=7  )




output$addcompvolc <- renderUI({
  req(input$volcacomp)
  compleft <- names(subsetstat()[[1]])[!names(subsetstat()[[1]]) %in% input$volcacomp]
  selectizeInput(
  'addvolcacomp', 'Add comparison(s) for the barplot', choices = compleft, multiple = TRUE
  )
})
