#' Reactive function that return a list containing multiple data frames
#' with the adj.P.val, logFC and P.value selected for the corresponding groups
#'
#' @param csvf Data frame corresponding to the Alltoptable
#'
#' @return \mygrep a list of data frames 
#'

adjusted <- reactive({
  
  df <- csvf()
  if (is.null(df))
    return(NULL)
  
  myrpl = c("^adj.P.Val_","^logFC_","^P.value_")
  grepdf = c("X|^adj.P.Val","X|^logFC","X|^P.value")
  
  adj = csvf()[[3]][, grep("X|^adj.P.Val",
                           names(csvf()[[3]]),
                           value = TRUE)]
  
  logfc = csvf()[[3]][, grep("X|^logFC",
                             names(csvf()[[3]]),
                             value = TRUE)]
  
  pval = csvf()[[3]][, grep("X|^P.value",
                            names(csvf()[[3]]),
                            value = TRUE)]
  
  
  mygrep = list(adj,logfc,pval)
  
  
  for(i in 1:length(mygrep))
    names(mygrep[[i]]) = gsub(
      pattern = myrpl[i],
      replacement = "",
      x = names(mygrep[[i]]),
      perl = T
    )
  
  return(mygrep)
  
})