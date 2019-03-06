#' renderoutputTable is a shiny widget which aims is return a table in the gui
#'
#' @param id Shiny id
#'
#' @return Widget in the gui
#'
#' @export


renderoutputTable <- function(id){
  ns <- NS(id)
  dataTableOutput(ns("outputable"))
}

#' stylishTables is a shiny module which aims is to render a table in the gui
#'
#' @param input Internal
#' @param output Internal
#' @param session Internal
#' @param data A reactive dataframe
#' @param lengthpage A character vector (number of rows to display in the table)
#' @param extensionsa A character vector of the names of the DataTables extensions (check DT API)
#' @param scrollX A boolean value (horizontal scrolling )
#' @param scrollY A numeric value (Vertical scrolling)
#' @param pageLength A numeric value (number of rows in the output table)
#' @param ordering A boolean value
#' @param server A boolean value (server-side processing)
#' @param buttons A button collection that provides column visibility control
#' @param stateSave A boolean value (Enable or disable state saving)
#' @param searching A boolean value (Enable or disable research in table)
#' @param filter A function with two argumentsdataandparams
#' @param dom Define the table control elements to appear on the page and in what order.
#' @param rownames TRUE(show row names) or FALSE(hide row names) or a character vector of rownames;
#' @param selection the row/column selection mode (single or multiple selection or disable selection) when a table widget is rendered in a Shiny app
#' @param dupgenes A reactive character vector of the duplicated genes
#' @param case A Numeric value
#'
#' @return An outpout table in the gui
#'
#' @export
#'


stylishTables <- function(input, output, session , data, lengthpage=  c('5', '10', '15') , extensions = c("Buttons",'Scroller'), scrollX= F, scrollY= F, pageLength = NULL , ordering =T,
                          server =F, buttons = NULL, stateSave =NULL , searching =T, filter=NULL , dom= NULL, rownames = F, selection =  "multiple", dupgenes = NULL, case = 1){


output$outputable <- DT::renderDataTable(DT::datatable(data()  ,extensions= extensions,  rownames = rownames , selection = selection ,
                options = list(lengthMenu = lengthpage, scrollX = scrollX , pageLength =  pageLength , scrollY = scrollY,  stateSave = stateSave , dom = dom ,searching = searching,ordering = ordering,
                buttons = buttons, filter = filter)) %>%
                {if (case == 3 && !is.null(dupgenes())) { formatStyle( . ,'GeneName', color = styleEqual(unique(dupgenes()), rep('orange', length(unique(dupgenes())))))} else formatStyle(., NULL) }  , server = server)

return(reactive({switch(case, NULL, input$outputable_row_last_clicked, input$outputable_rows_all , input$outputable_rows_selected)}))
}
