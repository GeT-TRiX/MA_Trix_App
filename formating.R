formating = function( adj, musmuscu, pval){
  
  passingval = adj %>%
    apply(2,FUN = function(x){return(x < pval)}) %>%
    apply(1,sum) 

  passingval = which( passingval > 0)
  cat("Il y a",length(passingval),"gène significatifs")
  
  row.names(musmuscu) = musmuscu$X
  musmuscu <- data.matrix(musmuscu[,-1])
  
  newlist = list(passingval, musmuscu )
  return(newlist)
}