renderoutputTable <- function(id){
  ns <- NS(id)
  dataTableOutput(ns("outputable"))
}

stylishTables <- function(input, output, session , data, lengthpage=  c('5', '10', '15') , extensions = c("Buttons",'Scroller'), scrollX= F, scrollY= F, pageLength = NULL , ordering =T, 
                          server =F, buttons = NULL, stateSave =NULL , searching =T, filter=NULL , dom= NULL, rownames = F, selection =  "multiple", dupgenes = NULL, case = 1){

getJvennord <- reactiveValues()
  
output$outputable <- DT::renderDataTable(DT::datatable(data()  ,extensions= extensions,  rownames = rownames , selection = selection , 
                options = list(lengthMenu = lengthpage, scrollX = scrollX , pageLength =  pageLength , scrollY = scrollY,  stateSave = stateSave , dom = dom ,searching = searching,ordering = ordering,
                buttons = buttons, filter = filter)) %>%
                {if (case == 3 && !is.null(dupgenes())) { formatStyle( . ,'GeneName', color = styleEqual(unique(dupgenes()), rep('orange', length(unique(dupgenes())))))} else formatStyle(., NULL) }  , server = server) 

return(reactive({switch(case, NULL, input$outputable_row_last_clicked, input$outputable_rows_all , input$outputable_rows_selected)}))
}