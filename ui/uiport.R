# Module UI function
csvFileInput <- function(id, label = "CSV file") {
  # Create a namespace function using the provided id
  ns <- NS(id)
  
 #tagList(
    fileInput(ns("file"), label, multiple = T,
    accept = c(
      "text/csv",
      "text/comma-separated-values,text/plain",
      ".csv"
    ))
  #)
}