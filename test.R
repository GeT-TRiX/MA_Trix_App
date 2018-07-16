library(shiny)
library(shinyFiles)
library(BH)
library(data.table)


chartofa = function(datach){
  
  datach[] <- lapply( datach, factor)
  col_names <- names(datach)
  datach[col_names] <- lapply(datach[col_names] , factor)
  
  return(datach)
}


### ui end, to browse to desired folder
ui = fluidPage(
  #shinyFilesButton('directory', 'Folder select', 'Please select a folder', multiple = T),
  
  shinyDirButton('directory', 'Folder select', 'Please select a folder'),
               tableOutput(outputId = "datpat")
)

### extracting the folder path
server = function(input, output, session) {
  volumes <- c(root = "/home/franck/MA_Trix_App/data")
  shinyDirChoose(input, 'directory', roots=volumes, session=session)
  path1 <- reactive({
    return(print(parseDirPath(volumes, input$directory)))
  })
  ### constructing the 3 file paths

      csvf <- reactive({
        req(path1())
        csvtest <- list()
        csvtest[1] <- paste0(path1(),"/TOXA_HEGU_MA0191_AllChip_pData.csv")
        csvtest[2] <- paste0(path1(),"/TOXA_HEGU_MA0191_AllChip_WorkingSet.csv")
        csvtest[3] <- paste0(path1(),"/All_topTableAll.csv")
        
          
          #csv <- lapply(csvtest, read.csv2, check.names = F) # benchmark read.csv wrapper
          print(csvtest[1])
          csv <- lapply(
            csvtest,
            
            #' apply the fread method for each element in the csvtest list
            #'
            #' @return list of data frame objects
            #'
            #' @export
            
            FUN = function (x)
              
              # read.table( # benchmark read.table
              #   x,
              #   sep = ";" ,
              #   dec = ",",
              #   header = T,
              #   check.names = F # good col names
              # )
              
              fread(
                x,
                data.table = F,
                check.names = F,
                header = T,
                sep = ";",
                dec = ","
              ) #benchmark fread memory speed
          )
          
          csvord = list()
          print("ok")
          for (i in 1:length(csv)) {
            if (colnames(csv[[i]][2]) == "Grp") {
              csvord[[2]] = csv[[i]]
              
            }
            else if (any(grepl("adj.P.Val" , colnames(csv[[i]]))))
            {
              csvord[[3]] = csv[[i]]
              
            }
            else
              csvord[[1]] = csv[[i]]
          }
          
          csvord[[2]] = chartofa(csvo
                                 rd[[2]]) # transform dataframe containing characters to factors
          row.names(csvord[[1]]) = csvord[[1]][, 1]
          colnames(csvord[[3]])[1] = "X"
          colnames(csvord[[2]])[1] = "X"
          
        
        return (csvord)
        
      })
      
  
}

shinyApp(ui = ui, server = server)