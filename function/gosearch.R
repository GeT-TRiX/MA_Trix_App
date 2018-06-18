#require(goseq)
#require(GO.db)
library(dplyr)


# enrichment_empty <- function () {
#   data.frame(category=numeric(0),
#              over_represented_pvalue=numeric(0),
#              under_represented_pvalue=numeric(0),
#              num_in_subset=numeric(0),
#              num_total=numeric(0),
#              over_represented_pvalue_adj=numeric(0),
#              under_represented_pvalue_adj=numeric(0))
# }

gosearch <- function(hm01, species, ids, clusterlist) {
  #clusterlist = NULL
  library(goseq)
  library(GO.db)
	
  for (i in 1:NROW(unique(hm01$cluster))) {
    genlist <- hm01[!duplicated(hm01$GeneName),]
    genlist <-genlist %>% dplyr::select(cluster, GeneName)   %>% filter(cluster == i)
    final = as.double(matrix(1, length(genlist$cluster)))
    names(final) = (genlist$GeneName)
    
    h <- function(w)
        if (any(grepl("constraints|library", w)))
          invokeRestart("muffleWarning")
    
    e <- function(y)
      if(any(grepl("Rplots.pdf", y)))
        invokeRestart("muffleWarning")

    pwf <- tryCatch({
      withCallingHandlers(nullp(final, species, ids ,plot.fit=FALSE), warning = h, error = e) %>% na.omit()
    }, warning = function(e) {
      warning("40 % of genes are misssing")
      #return(enrichment_empty())
      return(NULL)
    })
    
    #pwf <- nullp(final, species, ids,plot.fit=FALSE) %>% na.omit()
    cat(length(row.names(pwf)))
    
    if (!is.null(pwf)) {
      finalons <- goseq(pwf, species , ids, use_genes_without_cat = F)
      clusterlist[[i]] = filter(finalons, numInCat > 1 ) %>%
        arrange(desc(numInCat))
    }
    else
      clusterlist[[i]] = NULL
  }
  return(clusterlist)
}


wclust <- function(clusterlist, filename, min, top)  {
  cat("Clustering", file = filename)
  sink(filename)
  sink()
  con <- file(filename, "w")
  for (i in 1:NROW(clusterlist)) {
    if (!i == 1)
      cat("--------------------------------------\n", file = con)
    cat(paste("cluster:", i),  file = con)
    if (!i == 1)
      cat("\n--------------------------------------\n", file = con)
    cat("\n--------------------------------------\n", file = con)
    if (!length(clusterlist[[1]][[i]]) == 0) {
      for (go in top:min) {
        cat(paste("GOID:", as.character(GOID(
          clusterlist[[i]][[1]][[go]]
        ))), file = con)
        cat("\n", file = con)
        cat(paste("Term:", as.character(Term(
          clusterlist[[i]][[1]][[go]]
        ))), file = con)
        cat("\n", file = con)
        cat(paste("Definition:", as.character(Definition(
          clusterlist[[i]][[1]][[go]]
        ))), file = con)
        cat("\n", file = con)
        cat("--------------------------------------\n", file = con)
      }
      cat("\n", file = con)
    }
  }
  
  close(con)
}


probnamtoentrez <- function(hm01,  mypack) {
  
  lapply(1:NROW(unique((hm01$cluster))), function(x) {
    entrezids <- hm01 %>%
      filter(cluster == x) %>%
      dplyr::select(GeneName) %>%
      unlist() %>%
      as.character() %>%
      mget(x = .,envir = mypack,ifnotfound = NA) %>%
      unlist() %>%
      unique() %>%
      .[!is.na(.)]
    
    return(entrezids)
  })
}

entreztosymb <- function(myentz, mypack){
lapply(1:NROW(myentz), function(x)
  as.vector(unlist(mget(myentz[[x]], envir=mypack, ifnotfound=NA))))
}


davidquery <- function(entrezids, species) {
  test = lapply(1:NROW(entrezids), function(x) {
    david <- DAVIDWebService$new(email = "franck.soubes@inra.fr", url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
    RDAVIDWebService::setTimeOut(david, 90000)
    result <-
      addList(
        david,
        entrezids[[x]],
        idType = "ENTREZ_GENE_ID",
        listName = "testList",
        listType = "Gene"
      )
    
    selectedSpecie = (species)
    backgroundLocation = grep(selectedSpecie,RDAVIDWebService::getBackgroundListNames(david))
    specieLocation = grep(selectedSpecie, RDAVIDWebService::getSpecieNames(david))
    setCurrentSpecies(object = david, species = specieLocation)
    setCurrentBackgroundPosition(object = david, position = backgroundLocation)
    #getSpecieNames(david)
    setAnnotationCategories(david, c("GOTERM_MF_ALL"))
    as.data.frame(cbind(getFunctionalAnnotationChart(object=david, threshold=1, count=0L)))  %>%
      filter(Count>1) %>% arrange(desc(Count))  %>% select( Category:Count, List.Total:Pop.Total,PValue,everything())
  })
}









