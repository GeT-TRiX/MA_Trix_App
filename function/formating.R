##### Formating function for microarray data #### 
##### Franck Soubès


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


### ANother method faster and stronger
### thisisit <- select(musmuscu, as.character(factor(mydata$X)))
# selected = c("LKO_MCD ", "LKO_CTRL" )
# selected = levels(groupss$Grp)
# 
# 
#groupss[match(as.character(groupss$Grp), selected, nomatch = T), ]
# test <- groupss[groupss$Grp %in% selected,]
# 
# 
# uniquegrp = unique(test$Grp)
# btestos <- droplevels(test)


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


  dtsign = data.frame(matrix(ncol = 2, nrow = length(adj[, -1])))
  y <- c("FDR < 0.01", "FDR < 0.05")

  dtsign = data.frame(matrix(ncol <- 2, nrow <- length(adj[, -1])))
  y <- c("pvalue(0.01)", "pvalue(0.05)")

  colnames(dtsign) <- y
  rownames(dtsign) <- colnames(adj[, -1])
  pvalue = c(0.01, 0.05)
  
  i <- 1
  for (pv in pvalue) {
    for (elem in colnames(adj[, -1])) {
      
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



#' This function returns a data frame of the element which are superior to a vector character 1.2,2,4,6 and 10 and for a defined pvalue
#'
#' @param alltop a data frame
#' @param pval a numeric pvalue
#'
#' @return \fcpval a data frame 
#' 
#' @export

myfinalfc = function(alltop, pval, testrix) {
  
  j = 1
  
  whatest  = ifelse(testrix == "FDR", T, F)
  if (whatest)
    adj = alltop[, grep("X|^adj.P.Val", names(alltop), value = TRUE)]
  else
    adj = alltop[, grep("X|^P.value", names(alltop), value = TRUE)]
    
  logfc = alltop[, grep("X|^logFC", names(alltop), value = TRUE)]
  myfc = c(1, 1.2, 2, 4, 6, 10)
  fcpval = data.frame(matrix(ncol = length(myfc), nrow = length(adj[, -1])))
  mycolnames = c("FC>1.0", "FC >1.2" , "FC >2", "FC >4", "FC >6", "FC >10")

  
  for (fc in myfc) {

    fcpval[j] = cbind.data.frame(colSums(adj[,-1] < pval &
                                           2 ** abs(logfc[,-1]) > fc))
    j = j + 1
  }
  
  names(logfc) =  gsub(
    pattern = "^logFC_",
    replacement = "",
    x = names(logfc),
    perl =  TRUE
  )

  colnames(fcpval) = mycolnames
  rownames(fcpval) = colnames(logfc[, -1])
  
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


#' This function returns a data frame of the significant genes associated with the corresponding cluster index
#'
#' @param cut02 a heatmap object
#' @param ind a vector of integer
#' @param signws a matrix 
#' @param pval a data frame
#'
#' @return a data frame
#' @export
#'


#heatmtoclust = function( hmp01_All, ind, signws, pval, myval= 5){

heatmtoclust = function( hmp01_All, exprData, pval, height= 5){
  
  cut02 = cut(hmp01_All$rowDendrogram, h = height )
  
  
  HCgroupsLab = lapply(cut02$lower, function(x)
    labels(x))
  
  #exprData=signws[ind,]
  final = exprData[rev(hmp01_All$rowInd), hmp01_All$colInd]
  
  my_last= as.integer(lapply(seq(length(HCgroupsLab)), function(x)
  {return(tail(HCgroupsLab[[x]],1))}))
  
  mygen = as.integer(row.names(final))
  
  heatmclust = pval %>%
    dplyr::select (X,ProbeName,GeneName) %>%
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




