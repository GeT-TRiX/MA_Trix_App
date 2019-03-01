renderncolour <- function(id){
  ns <- NS(id)
  uiOutput(ns("colorbox"))
}


colorChooser <- function(input, output, session, data){
  
  output$colorbox <- renderUI({ 
    ns <- session$ns
    colorfluid()
  })
  
  
  colorfluid <- reactive({
    req(initwidgetcol())
    lapply(1:length(initwidgetcol()), function(i){
      
      j = length(initwidgetcol())
      if(length(initwidgetcol()) %%2==0){
        if (i %% 2 == 0) {
          fluidRow(column(6, initwidgetcol()[[i - 1]]), column(6, initwidgetcol()[[i]]))
        }
      }
      else{
        if (i %% 2 ==0 && j!=i) {
          fluidRow(column(6, initwidgetcol()[[i - 1]]), column(6, initwidgetcol()[[i]]))
        }
        else if (j == i){
          fluidRow(column(6, initwidgetcol()[[i]]))
        }
      }
      
    })
    
  })
  
  initwidgetcol <- reactive({
    
    req(mycolgrp())
    ns <- session$ns
    pal = brewer.pal(8,"Dark2") %>%
      list(brewer.pal(10,"Paired")) %>%
      unlist()
    
    lapply(seq_along(unique(mycolgrp())), function(x) {
      colourInput(
        ns(paste("colgroup", x, sep = "_")),
        levels(mycolgrp())[x],
        pal[x],
        allowedCols =  pal,
        palette = "limited",
        returnName = T)
    })
  })
  
  
  getwidgvalues <- reactive({
    req(mycolgrp())
    lapply(seq_along(unique(mycolgrp())), function(i) {
      
      input[[paste("colgroup", i, sep = "_")]]
    })
  })
  
  mycolgrp <- reactive  ({
    req(data())
    mygrpcol <- data() %>% 
      sort() %>%
      unique() %>%
      droplevels()
    
    return(mygrpcol)
  })
  

  return(reactive(getwidgvalues()))
}