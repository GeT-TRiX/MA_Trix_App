decTestTRiX <- function(adj,logfc,pval, DEGcutoff = 0.05 ,FC = 1,cutoff_meth = "FDR",maxDE = NULL,contrast = 1:ncol(adj))

{
  ## select probes with cutoff_meth<= DEGcutoff and FoldChange > FC and nbr of selected probes < maxDE (if nb FC selected >maxDE)
  
  if (length(contrast) == 1)
    contrast = c(contrast, contrast)
  
  if (is.na(maxDE))
    maxDE = nrow(adj)
  
  #print(maxDE)
  
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
    
    # reduce the nbr of selecte probes to maxDE for each cont
    # cat("\n -> reduction of selected probes to",
    #     maxDE,
    #     "in each contrast\n")
    
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
    #print(colSums(DEsel, na.rm = T))
  }
  
  else{
    DEsel = DEp & DEFC
    #print(colSums(DEsel, na.rm = T))
  
  }

  
  DEsel = which(rowSums(DEsel, na.rm = T) > 0)
  #cat("Il y a",length(DEsel),"gène significatifs")
  
  return(DEsel)
  
}
