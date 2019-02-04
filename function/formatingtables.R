### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#' formating is a function alpha version of the higher elaborate decTestTRiX function 
#'
#' @param adj
#' @param pval 
#'
#' @return
#' @export
#'
#' 
#' @export

formating = function( adj, pval){
  
  
  passingval = adj %>%
    apply(2,FUN = function(x){return(x < pval)}) %>%
    apply(1,sum) 
  
  passingval = which( passingval > 0)
  
  cat("Il y a",length(passingval),"gène significatifs")

  return(passingval)

}


#' transform a dataframe containing factor for different levels not optimal tho
#'
#' @param dataframe 
#'
#' @return
#' 
#' @export


transform <- function(dataframe,toast){
  
  myl = list()
  cpt = 1
  for (i in toast) {
    command <- paste0(i, "<-subset(dataframe, Grp=='", i, "')")
    test = eval(parse(text=command))
    X = test$X
    Grp = test$Grp
    myl[[cpt]] = data.frame(X ,Grp)
    cpt = cpt+1
    dyn_grp <- Reduce(function(x, y) merge(x, y, all=TRUE), myl, accumulate=FALSE)
  }
  
  return(dyn_grp)
}





#' This function returns an integer for the number of significant genes
#'
#' @param adj a data frame
#' @param elem a list
#' @param pv a list
#'
#' @return \grp1 of class data frame
#' 
#' @export

evaluatesign = function(adj,elem,pv){
  

  grp1 = adj[,c(elem)] %>%
    sapply( FUN = function(x){return(x < pv)}) %>%
    data.frame() %>%
    filter(. == T) %>%
    nrow()
  
  return(grp1)
}


#' This function returns an integer for the number of significant genes using parallelism
#' 
#' @param adj 
#' @param elem 
#' @param pv 
#'
#' @return \grp1 of class data.frame
#' 
#' @export




evaluatesignpar = function(adj,elem,pv) { ### for benchmarking 
  
  
  grp1 = foreach(i = iter(adj[elem], by = "col"), .combine = c) %dopar%  
    (sign= {i < pv}) %>%
    as.data.frame() %>%
    filter(. == T) %>%
    nrow()
  
 return(grp1) 
  
}


#' Create a data frame containing the number of signficant genes for different conditions pval and log fc
#'
#' @param adj a data frame containing the adjusted p-value
#'
#' @return \dtsign a data frame 
#' 
#' @export

createdfsign = function(adj) {


  dtsign = data.frame(matrix(ncol = 2, nrow = length(adj)))
  y <- c("FDR < 0.01", "FDR < 0.05")

  dtsign = data.frame(matrix(ncol <- 2, nrow <- length(adj)))
  y <- c("pvalue(0.01)", "pvalue(0.05)")

  colnames(dtsign) <- y
  rownames(dtsign) <- colnames(adj)
  pvalue = c(0.01, 0.05)
  
  i <- 1
  for (pv in pvalue) {
    for (elem in colnames(adj)) {
      
      if (i %% constmod == 0) {
        i <- 1
      }
      if (pv == 0.05)
      {

        dtsign$`FDR < 0.05`[i] = evaluatesignpar(adj, elem, pv)
        i = i + 1
      }
      else{
        
        dtsign$`pvalue(0.01)`[i] = evaluatesignpar(adj, elem, pv)
        i <- i + 1

      }
    }
  }
  return(dtsign)
}


isimilar <- function(restab, pdata, workingset){
  
  sameids <- all(restab[[1]] == workingset [[1]])
  samedesign <- all(colnames(workingset[-1])== pdata[[1]])
  return(list(sameids,samedesign))
  
}

#' This function returns a data frame of the element which are superior to a vector character 1.2,2,4,6 and 10 and for a defined pvalue
#'
#' @param alltop a data frame
#' @param pval a numeric pvalue
#'
#' @return \fcpval a data frame 
#' 
#' @export

myfinalfc <- function(alltop, pval, testrix) {
  j = 1
  colnames(myfinalfc)
  whatest  = ifelse(testrix == "FDR", T, F)
  if (whatest)
    adj = alltop[, grep("^adj.P.Val", names(alltop), value = TRUE), drop= F]
  else
    adj = alltop[, grep("^P.value", names(alltop), value = TRUE),drop= F]
  
  logfc = alltop[, grep("^logFC", names(alltop), value = TRUE),drop= F]
  myfc = c(1, 1.2, 2, 4, 6, 10)
  fcpval = data.frame(matrix(ncol = length(myfc), nrow = length(adj)))
  mycolnames = c("FC>1.0", "FC >1.2" , "FC >2", "FC >4", "FC >6", "FC >10")
  

  for (fc in myfc) {
    fcpval[j] = cbind.data.frame(colSums(adj < pval &
                                           2 ** abs(logfc) > fc))
    j = j + 1
  }
  
  names(logfc) =  gsub(
    pattern = "^logFC_",
    replacement = "",
    x = names(logfc),
    perl =  TRUE
  )
  
  colnames(fcpval) = mycolnames
  rownames(fcpval) = colnames(logfc)
  return(fcpval)
  
}

#' This function returns a transformed data frame of character type to a data frame of factor type
#'
#' @param datach a data frames
#'
#' @return \datach a data frame 
#' 
#' @export


chartofa = function(datach){
  
  datach[] <- lapply( datach, factor)
  col_names <- names(datach)
  datach[col_names] <- lapply(datach[col_names] , factor)
  
  return(datach)
}


meanrankgenes  <- function(dtsign, stat , rankcomp=NULL, multcomp, regulationvolc=NULL, jvenn = F){
  
  selcomp <-  paste0(stat, multcomp )
  options(datatable.optimize=1)
  
  for (i in selcomp) {
    dtsign[[i]] = as.numeric(as.character(dtsign[[i]]))
  }
  
  summarizetable <- dtsign %>% select(GeneName, paste0(stat, multcomp))  %>% 
    as.data.table() %>% .[,lapply(.SD,function(x) mean=round(mean(x), 3)),"GeneName"] %>% as.data.frame() 
  
  if(!jvenn){
  summarizetable$rank <- summarizetable %>% select(paste0(stat , rankcomp) ) %>% rank(.) 
  summarizetable <- if(regulationvolc == "down") summarizetable %>% arrange( desc(-rank) ) else summarizetable %>% arrange( desc(rank) )  
  }
  
  return(summarizetable)
}


#' This function returns a data frame of the significant genes associated with the corresponding cluster index
#'
#' @param hmp01_All a heatmap object
#' @param exprData a vector of indices for the significant genes who have crossed the treshold pval and fc
#' @param pval a data frame of the alltoptable
#' @param height numeric value where the dendogram is cut off
#'
#' @return a data frame
#' @export
#'

heatmtoclust = function( hmp01_All, exprData, pval ,height= 5){
  
  cut02 = cut(hmp01_All$rowDendrogram, h = height )
  
  HCgroupsLab = lapply(cut02$lower, function(x)
    labels(x))
  id = colnames(pval[1])
  
  final = exprData[rev(hmp01_All$rowInd), hmp01_All$colInd]
  
  my_last= as.integer(lapply(seq(length(HCgroupsLab)), function(x)
  {return(tail(HCgroupsLab[[x]],1))}))
  
  mygen = as.integer(row.names(final))
  pval$X = as.integer(rownames(pval))
  
  heatmclust = pval %>%
    dplyr::select (X,id,GeneName) %>%
    filter( X %in% mygen) %>%
    left_join(data.frame(X=mygen), . , by="X") %>%
    arrange(-row_number())
  
  i = 1
  for(row in 1:nrow(heatmclust)){
    if(heatmclust$X[row] == my_last[i] ){
      heatmclust$cluster[row] = i
      i = i+1
    }
    else
      heatmclust$cluster[row] = i
  }
  
  return(heatmclust)
  
}





