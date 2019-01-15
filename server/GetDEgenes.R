### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



#' subsetstat is a reactive function that return a list containing multiple data frames
#' with the adj.P.val, logFC and P.value selected for the corresponding groups
#'
#' @param csvf Data frame corresponding to the Alltoptable
#'
#' @return subsetstat a reactive list of data frames
#'
#' @export


subsetstat <- reactive({
  
  req(csvf())
  
  
  adj = csvf()[[3]][, grep("^adj.P.Val|^FDR|^padj",
                           names(csvf()[[3]]),
                           value = TRUE), drop= F]
  
  logfc = csvf()[[3]][, grep("^logFC|log2FoldChange|logFC", 
                             names(csvf()[[3]]),
                             value = TRUE), drop= F]
  
  pval = csvf()[[3]][, grep("^P.value|PValue|pvalue",
                            names(csvf()[[3]]),
                            value = TRUE), drop= F]
  
  vecstat = c("^adj.P.Val_","^logFC_","^P.value_") # Put your statistical prefix here for multitest comparisons
  subsetstat = list(adj,logfc,pval)
  for(i in 1:length(subsetstat))
    names(subsetstat[[i]]) = gsub(
      pattern = vecstat[i],
      replacement = "",
      x = names(subsetstat[[i]]),
      perl = T
    )
  
  
  return(subsetstat)
  
})




# subsetcomp is a reactive function that return a list of data frame depending on the selected comparisons
#'
#' @param subsetstat list of three data frame corresponding to the grep of respectively Adj.pval, P.val and logFC columns
#' @param selected_test character corresponding to the defined contrast set by the user
#'
#' @return usergroup a reactive list containing three data frame for each contrast selected
#'
#' @export

subsetcomp <- reactive({ 
  
  req(selected_test(),subsetstat())
  
  subsetedcomp = list()
  for (i in 1:3)
    subsetedcomp[[i]] = (subset(subsetstat()[[i]],
                           select = selected_test()))
  
  return(subsetedcomp)
})



#' subsetDEG is a reactive function that return the indexes for the signficant genes 
#'
#' @param subsetcomp a list of three data frame with rows selected according to the contrasts selected
#' @param intput$fc a numeric FC selected
#' @param input$decidemethod a character method, default = BH
#' @param input$pval a numeric pvalue
#' @param input$maxgen a numeric maxgen, default = NULL
#'
#' @return subsetDEG a reactive data frame with the indexes corresponding to the sigificant genes
#'
#' @export
#' 


subsetDEG <- reactive({
  
  req(subsetcomp(), csvf())
  
  indexDEG = decTestTRiX(
    subsetcomp()[[1]],
    subsetcomp()[[2]],
    subsetcomp()[[3]],
    DEGcutoff = input$pval,
    FC = input$fc,
    cutoff_meth = input$decidemethod,
    maxDE = input$maxgen)
  
  return(indexDEG)
  
})


