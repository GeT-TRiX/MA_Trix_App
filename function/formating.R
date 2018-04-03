##### Formating function for microarray data #### 
##### Franck Soubès


#' Can be upgraded
#'
#' @param adj
#' @param pval 
#'
#' @return
#' @export
#'
#' @examples
#' 

formating = function( adj, pval){
  
  
  passingval = adj %>%
    apply(2,FUN = function(x){return(x < pval)}) %>%
    apply(1,sum) 

  passingval = which( passingval > 0)
  cat("Il y a",length(passingval),"gène significatifs")
  
  
  newlist = list(passingval, adj )
  return(newlist)

}


#' transform a dataframe containing factor for different levels not optimal tho
#'
#' @param dataframe 
#'
#' @return


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


#' This function return an integer for the number of significant genes
#'
#' @param adj a data frame
#' @param elem a list
#' @param pv a list
#'
#' @return \grp1 of class data frame

evaluatesign = function(adj,elem,pv){
  

  grp1 = adj[,c(elem)] %>%
    sapply( FUN = function(x){return(x < pv)}) %>%
    data.frame() %>%
    filter(. == T) %>%
    nrow()
  
  return(grp1)
}


#' This function return an integer for the number of significant genes using parallelism
#' 
#' @param adj 
#' @param elem 
#' @param pv 
#'
#' @return \grp1 of class data.frame


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

createdfsign = function(adj) {

  
  constmod <- (length(colnames(adj[,-1]))+1)
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
        dtsign$`pvalue(0.05)`[i] = evaluatesignpar(adj, elem, pv)
        i <- i + 1
      }
      else{
        dtsign$`pvalue(0.01)`[i] = evaluatesignpar(adj, elem, pv)
        i <- i + 1
      }
    }
  }
  return(dtsign)
}


