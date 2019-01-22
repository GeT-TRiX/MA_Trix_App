### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
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


family_d <- shiny::debounce(family_input, 750) # Delay input debounche also pour search genes



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
  req(csvf())
  EnhancedVolcano(csvf()[[3]], lab= csvf()[[3]]$GeneName , x = paste0("logFC_",input$volcacomp) ,
                  y = paste0(ifelse(input$method == "FDR", "adj.P.Val_","P.value_"),input$volcacomp),
                  topgenes = input$topvolc,DrawConnectors= T,#DrawConnectors = ifelse(is.na(input$topvolc),T,F),
                  pCutoff = input$volcpval ,FCcutoff = input$volcfc ,transcriptPointSize = input$volcpt,transcriptLabSize = input$volclab,
                  title =  gsub("-"," versus " ,input$volcacomp),cutoffLineType = "twodash", findfamily =  ifelse(familytopdisp() == "" , NA,familytopdisp()),
                  displaylab = ifelse(genetodisplay() =="", NA, genetodisplay()),legendLabSize = 10,
                  cutoffLineCol = "black",cutoffLineWidth = 1,legend=c("NS","Log (base 2) fold-change","P value",
                                                                       "P value & Log (base 2) fold-change"))
})



output$volcanoplot <- renderPlot({
  
  validate(need(csvf(), 'You need to import data to visualize this plot!'))
  
  req(volcano())
  volcano()[[1]]
  
},  height = plotHeight)

output$compvolc <- renderUI({
  req(subsetstat())
  selectInput("volcacomp", "Choose a comparison", choices = colnames(subsetstat()[[1]]))
})


output$savevolcano <- downloadHandler(filename <- function() {
  paste0(basename(file_path_sans_ext(projectname())), '_volcano.', input$formvolc, sep ='')
},
content <- function(file) {
  if (input$formvolc == "pdf")
    
    pdf(file,
        width = 12,
        height = 12,
        pointsize = 12)
  
  
  else if (input$formvolc == "png")
    
    png(
      file,
      width = 2500,
      height = 2500,
      units = "px",
      pointsize = 12,
      res = 100
    )
  else
    ggsave(file,device=cairo_ps, fallback_resolution = 600)
  
  
  plot(volcano()[[1]])
  dev.off()
})
