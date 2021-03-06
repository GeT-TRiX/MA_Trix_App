### Author: Yannick Lippi
### Modified: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



#' decTestTRiX is a function
#'
#' @param adj A data frame with the "adj.P.Val"
#' @param logfc A data frame with the "logFC"
#' @param pval A data frame with the "P.value"
#' @param DEGcutoff A numeric value for tresholding the data on the pvalue
#' @param FC A numeric value for tresholding the data on the FC value
#' @param cutoff_meth A character to choose the method appropriate, "FDR" in order to cut off with the "adj.P.Val" and "None" for the "P.value"; default is set to "FDR"
#' @param maxDE A numeric value that gives a maximal number of genes to display for all the differents contrasts
#' @param contrast A numeric value representing the length of the data frame
#'
#' @return \DEsel a matrix of double values containing the signficant genes
#'
#' @export

decTestTRiX <- function(adj,logfc,pval, DEGcutoff = 0.05 ,FC = 1,cutoff_meth = "FDR",maxDE = NULL,contrast = 1:ncol(adj)){
  ## select probes with cutoff_meth<= DEGcutoff and FoldChange > FC and nbr of selected probes < maxDE (if nb FC selected >maxDE)


  if (length(contrast) == 1)
    contrast = c(contrast, contrast)


  
  if (is.na(maxDE) || is.null(maxDE))
    maxDE = nrow(adj)


  if (cutoff_meth == "FDR")
    pList = adj[, contrast]


  
  if(cutoff_meth=="None")
    pList= pval[,contrast]


  ## select on pvalue
  DEp = pList <= DEGcutoff
  


  ## select on FC
  DEFC = 2 ** abs(logfc[, contrast]) >= FC

  ## reduce selection to maxDE
  if (any(colSums(DEFC) > maxDE)) {


    DEmax = pList

    for (i in 1:ncol(DEFC))

    {
      if (maxDE > sum(DEFC[, i])) {
        maxDEi = sum(DEFC[, i])
      } else
        maxDEi = maxDE

      ord = order(DEmax[, i])
      msi = max(DEmax[ord, i][DEFC[ord, i]][1:maxDEi])

      DEmax[, i] = DEmax[, i] <= msi

    }

    DEsel = DEp & DEFC & DEmax
  }

  else{
    DEsel = DEp & DEFC
  }

  DEsel = which(rowSums(DEsel, na.rm = T) > 0)
  elements= list(DEsel, length(DEsel))

  return(elements)

}
