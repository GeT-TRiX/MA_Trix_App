### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


prefstat <- reactiveValues() #A reactive value to identify prefix for the foldchange

#' subsetstat is a reactive function that return a list containing multiple data frames
#' with the adj.P.val, logFC and P.value selected for the corresponding groups
#'
#' @param csvf Data frame corresponding to the restable
#'
#' @return subsetstat a reactive list of data frames
#'
#' @export


subsetstat <- reactive({

  req(csvf())

  adj = csvf()[[3]][, grep("^adj.P.Val|^FDR|^padj_",
                           names(csvf()[[3]]),
                           value = TRUE), drop= F]

  logfc = csvf()[[3]][, grep("^logFC|^log2FoldChange|logFC",
                             names(csvf()[[3]]),
                             value = TRUE), drop= F]

  pval = csvf()[[3]][, grep("^P.value|^PValue|^pvalue",
                            names(csvf()[[3]]),
                            value = TRUE), drop= F]

  vecstat = c("^adj.P.Val_","^padj_", "^FDR", "^logFC_","^log2FoldChange", "^P.value_", "^pvalue", "^PValue" ) # Put your statistical prefix here for multitest comparisons

  subsetstat = list(adj,logfc,pval)
  subsetinfo = list()
  for(i in 1:length(subsetstat)){
    for(j in 1:length(vecstat)){
      if(any(grepl(vecstat[j], names(subsetstat[[i]]) )))
        subsetinfo[[i]] <- vecstat[j]

      names(subsetstat[[i]]) = gsub(
      pattern = vecstat[j],
      replacement = "",
      x = names(subsetstat[[i]]),
      perl = T
      )
    }

  }

  prefstat$greppre <- gsub(pattern= "\\^", subsetinfo, replacement = "")
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


subsetDEG <- callModule(getDegenes, "deghm", data = subsetcomp , meth = reactive(input$decidemethod), case = 1 , maxDe = reactive(input$maxgen))


