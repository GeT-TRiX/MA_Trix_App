
#' Title
#'
#' @param adj
#' @param musmuscu 
#' @param pval 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 

formating = function( adj, musmuscu, pval){
  
  
  passingval = adj %>%
    apply(2,FUN = function(x){return(x < pval)}) %>%
    apply(1,sum) 

  passingval = which( passingval > 0)
  cat("Il y a",length(passingval),"gène significatifs")
  
  #row.names(musmuscu) = musmuscu$X
  #musmuscu <- data.matrix(musmuscu[,-1])
  
  newlist = list(passingval, adj )
  return(newlist)

}


#' transform a dataframe containing factor for different levels function is not optimal right now
#'
#' @param dataframe 
#'
#' @return
#' @export
#'
#' @examples
#' toto <- levels(dataframe$Grp)[1:3]
#' mydata <- transform(groupss,toto)


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

