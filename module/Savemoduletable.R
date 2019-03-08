#' downloadFilestab is a shiny widget which aims is to download a reactive plot
#'
#' @param id Shiny id
#' @param label Shiny label
#'
#' @return Widget in the gui
#'
#' @export
#'

downloadFilestab <- function(id, label = "Save your Scree plot") {
  ns <- NS(id)
  downloadButton(ns("downloadtable"), label ,   style ="color: #fff; background-color: #337ab7; border-color: #2e6da4")
}



#' downoutputables is a shiny module which aims is to export reactive table
#'
#' @param input Internal
#' @param output Internal
#' @param session Internal
#' @param projectname A reactive character (MA project or session data)
#' @param suffix A character
#' @param data A reactive expression to be plot
#' @param cont A reactive character vector of the selected comparison(s)
#' @param case A numeric value
#' @param xlsx A boolean value
#'
#' @export
#'

downoutputables <- function(input, output, session , projectname, suffix = "plot.csv",  data = NULL , cont = NULL,  xlsx = F , case ) {
  

  
  output$downloadtable  <- downloadHandler(
    
    filename <- function() {
      
      paste0(basename(file_path_sans_ext(projectname())), suffix  , sep ='')},
    

    content <- function(file) {
      
    if(!xlsx)  
      write.table(try(switch(case, 
                   myventocsv(data()  ,cont()), 
                   mysetventocsv(setvglobalvenn(data(), cont(), dll =T )),
                   data())),
        file,
        na = "",
        row.names = F,
        col.names = T,
        append = TRUE,
        sep = ";"
      )
    else
      withProgress(message = 'Creation of the xlsx table:',
                   value = 0, {
                     n <- NROW(50)
                     for (i in 1:n) {
                       incProgress(1 / n, detail = "Please wait...")
                     }
                     
                     
                     library(xlsx)
                     
                     for (i in 1:length(data())) {
                       if (i == 1)
                         write.xlsx(file = file,data()[[i]],
                                    sheetName = paste("Cluster", i))
                       else
                         write.xlsx(
                           file = file,
                           data()[[i]],
                           sheetName = paste("Cluster", i),
                           append = TRUE
                         )
                     }
                   })
      
    })
  
}
