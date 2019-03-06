### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#' formating is a alpha function of the higher elaborate decTestTRiX function
#'
#' @param adj A subset dataframe
#' @param pval Cutoff pvalue
#'
#' @return A numeric value of the number of significant genes


formating = function( adj, pval){


  passingval = adj %>%
    apply(2,FUN = function(x){return(x < pval)}) %>%
    apply(1,sum)

  passingval = which( passingval > 0)

  cat("Il y a",length(passingval),"gène significatifs")

  return(passingval)

}



#' Create a data frame containing the number of signficant genes for different conditions pval and log fc
#'
#' @param adj A subset dataframe
#'
#' @return A data frame
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


#' isimilar is a function which aims is to ensure that the unique ids are equal between the workingset and the restable as for the samples.
#'
#' @param restab A statistical dataframe
#' @param pdata A dataframe that associates samples to their respective biological conditions
#' @param workingset A normalized expression dataframe
#'
#' @return A boolean list
#'
#' @export

isimilar <- function(restab, pdata, workingset){

  sameids <- all(restab[[1]] == workingset [[1]])
  samedesign <- all(colnames(workingset[-1])== pdata[[1]])
  return(list(sameids,samedesign))

}

#' This function returns a data frame of the element which are superior to a logFC cutoff (1.2,2,4,6 and 10) and for a defined pvalue
#'
#' @param alltop A statistical dataframe
#' @param pval Cutoff pvalue
#' @param testrix A character value of the statistical criterions (pvalue, FDR)
#' @param grepre A list of subset dataframes
#'
#' @return A dataframe of the number of significant genes depending of both cutoff (logFC and pvalue)
#'
#' @export

restabfc <- function(alltop, pval, testrix, grepre) {

  j = 1
  whatest  = ifelse(testrix == "FDR", T, F)
  if (whatest)
    adj = alltop[, grep( grepre[[1]], names(alltop), value = TRUE), drop= F]
  else
    adj = alltop[, grep(grepre[[3]], names(alltop), value = TRUE),drop= F]

  logfc = alltop[, grep( grepre[[2]], names(alltop), value = TRUE),drop= F]
  myfc = c(1, 1.2, 2, 4, 6, 10)
  fcpval = data.frame(matrix(ncol = length(myfc), nrow = length(adj)))
  thresholds = c("FC>1.0", "FC >1.2" , "FC >2", "FC >4", "FC >6", "FC >10")

  for (fc in myfc) {
    fcpval[j] = cbind.data.frame(colSums(adj < pval & 2 ** abs(logfc) > fc))
    j = j + 1
  }

  names(logfc) =  gsub(
    pattern =  grepre[[2]] ,
    replacement = "",
    x = names(logfc),
    perl =  TRUE
  )

  colnames(fcpval) = thresholds
  rownames(fcpval) = colnames(logfc)
  return(fcpval)

}

#' This function returns a transformed data frame of character type to a data frame of factor type
#'
#' @param datach A dataframe that associates samples to their respective biological conditions
#'
#' @return A dataframe of factor type
#'
#' @export


chartofa = function(datach){

  datach[] <- lapply( datach, factor)
  col_names <- names(datach)
  datach[col_names] <- lapply(datach[col_names] , factor)
  return(datach)
}


#' meanrankgenes is a function which aims is to return a dataframe with the average logFC for each duplicate genes
#'
#' @param dtsign A statistical dataframe (ids and logFC)
#' @param stat A character to grep logFC or pvalue values within the statistical table
#' @param rankcomp A character vector of the selected contrast(s)
#' @param multcomp A choosen comparison to sort the genes based on the logFC
#' @param regulationvolc A character to specific the regulation (both, up, down) for the volcano page
#' @param jvenn A boolean value
#'
#' @return A dataframe
#'
#' @export
#'

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
#' @param hmp01_All An heatmap object
#' @param exprData An index vector of significant genes from the restable
#' @param pval A data frame of the restable
#' @param height A cutoff point of a dendogram
#'
#' @return A data frame (unique ids, gene symbols, pvalue and cluster)
#'
#' @export
#'

heatmtoclust = function( hmp01_All, exprData, pval ,height= 5){

  cut02 = cut(hmp01_All$rowDendrogram, h = height )

  HCgroupsLab = lapply(cut02$lower, function(x)
    labels(x))
  id = colnames(pval[1])

  reorderdend = exprData[rev(hmp01_All$rowInd), hmp01_All$colInd]

  taildendo= as.integer(lapply(seq(length(HCgroupsLab)), function(x)
  {return(tail(HCgroupsLab[[x]],1))}))

  mygen = as.integer(row.names(reorderdend))
  pval$X = as.integer(rownames(pval))

  heatmclust = pval %>%
    dplyr::select (X,id,GeneName) %>%
    filter( X %in% mygen) %>%
    left_join(data.frame(X=mygen), . , by="X") %>%
    arrange(-row_number())

  i = 1
  for(row in 1:nrow(heatmclust)){
    if(heatmclust$X[row] == taildendo[i] ){
      heatmclust$cluster[row] = i
      i = i+1
    }
    else
      heatmclust$cluster[row] = i
  }

  return(heatmclust)

}
