
renderoutputTable <- function(id){
  ns <- NS(id)
  dataTableOutput(ns("outputable"))
}

stylishTables <- function(input, output, session , data, lengthpage=  c('5', '10', '15') , extensions = c("Buttons",'Scroller'), scrollX= F, scrollY= F, pageLength = NULL , ordering =T, server =F, buttons = NULL, stateSave =NULL , searching =T, filter=NULL , dom= NULL){

  
output$outputable <- DT::renderDataTable(data()  ,extensions= extensions,  
                options = list(lengthMenu = lengthpage, scrollX = scrollX , pageLength =  pageLength , scrollY = scrollY,  stateSave = stateSave , dom = dom ,searching = searching,ordering = ordering,
                buttons = buttons, filter = filter)  , server = server) 
}