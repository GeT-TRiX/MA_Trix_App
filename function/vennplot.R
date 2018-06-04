require(VennDiagram)

#Intersect, Union and Setdiff (https://stackoverflow.com/questions/34130233/give-name-to-list-variable)

Intersect <- function (x) {  
  # Multiple set version of intersect
  # x is a list
  
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    intersect(x[[1]], x[[2]])
  } else if (length(x) > 2){
    intersect(x[[1]], Intersect(x[-1]))
  }
}

Union <- function (x) {  
  # Multiple set version of union
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    union(x[[1]], x[[2]])
  } else if (length(x) > 2) {
    union(x[[1]], Union(x[-1]))
  }
}

Setdiff <- function (x, y) {
  # Remove the union of the y's from the common x's. 
  # x and y are lists of characters.
  xx <- Intersect(x)
  yy <- Union(y)
  setdiff(xx, yy)
}



#' Vennlist is a function which aim is to return a list of signficant genes for a treshold pvalue of 5%
#'
#' @param pval a data frame
#' @param adj a data frame with the contrast selected
#' @param fc a data frame with the constrast selected
#' @param regulation a character
#'
#' @return \myl a list
#' 

Vennlist <- function(pval,adj,fc, regulation, cutoffpval, cutofffc){ ## ajout de foreach parallel
  
  if(is.null(pval)) 
    return(NULL)
  
  reguser = ifelse(regulation == "up", T, F)
  reguserboth = ifelse(regulation == "both", T, F)
  lapply(1:ncol(adj), FUN = function(x){
    if(reguser && !reguserboth)
      return(as.character(which(adj[[x]] < cutoffpval & fc[[x]] > log2(cutofffc))))
    else if(!reguser && !reguserboth)
      return( as.character(which(adj[[x]] < cutoffpval & fc[[x]] < -log2(cutofffc))))
    else
      return(as.character(which(adj[[x]] < cutoffpval & abs(fc[[x]]) > log2(cutofffc))))
  })

}




#' Vennfinal is a function which aim is to return an object containing a venn diagram 
#' 
#' @param myl a list of genes for the different contrasts
#' @param adj a data frame 
#' @param cex a vector giving the size for each area label 
#'
#' @return \final draw on the current device
#' 

Vennfinal <- function(myl,adj, cex=1, cutoffpval, cutofffc){
  
  if(is.null(myl))
    return(NULL)
  
  
  indexnull = which( sapply(myl ,length) == 0)
  myl <- myl[sapply(myl, length) > 0]
  final = length(myl)-1
  test = sum(sapply(myl,length))
  mynumb = paste("total genes", test , collapse = ":")
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  mytresh = paste0("DEG BH ", cutoffpval, " and FC " , cutofffc)
  if(length(indexnull)>0)
    g = venn.diagram(x = myl, filename = NULL, scaled = F, 
                   category.names = colnames(adj[,-c(indexnull)]),fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                   fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
  else
    g = venn.diagram(x = myl, filename = NULL, scaled = F, 
                     category.names = colnames(adj),fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                     fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
  
  final = grid.arrange(gTree(children=g), top="Venn Diagram", bottom= mytresh)
  
  return(final)
}


Vennsev <- function(myl, adj){
  
  myl <- myl[sapply(myl, length) > 0]
  final = length(myl)-1
  tot = sum(sapply(myl,length))
  mynumb = paste("total genes", tot , collapse = ":")
  g = venn(myven, ilabels= F, zcolor ="style", sname = colnames(adj), cexil = 0.5, size = 5, cexsn = 0.5)
  return(g)
}

#' myventocsv is a function that create a csv file of the signficant genes for the different contrasts for a cutoff of 5%
#'
#' @param myven a list of genes for the different contrasts
#' @param adj a data frame
#'
#' @return
#' 

myventocsv <- function(myven,adj){
  

  max.length <- max(sapply(myven, length))
  myven %>%
    lapply(function(v){ c(v, rep("", max.length-length(v)))}) %>%
    setNames(names(myven)) %>%
    as.data.frame()

}

setvglobalvenn <- function(vennlist,adj){
  
  names(vennlist) = colnames(adj)

  
  global <- unlist(lapply(1:length(vennlist), 
                  function(x) combn(names(vennlist), x, simplify = FALSE)),
           recursive = FALSE)
  
  names(global) <- sapply(global, function(p) paste0(p, collapse = ""))

  elements <- 
    lapply(global, function(i) Setdiff(vennlist[i], vennlist[setdiff(names(vennlist), i)]))
  
  
  elements <- elements[sapply(elements, length) > 0]
  
  return(elements)
}


rowtoprob <- function(myven,pval,adj) {
  
  pval$rownames = rownames(pval)
  names(myven) = colnames(adj)
  final = lapply(
    names(myven),
    FUN = function(x) {
      test = pval[pval$rownames %in% myven[[x]],]
      
      what <- test %>%
        select(ProbeName) %>%
        unlist() %>%
        as.character()
      
      return(what)
    }
  )
}


