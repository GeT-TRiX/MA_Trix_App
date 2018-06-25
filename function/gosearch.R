#require(goseq)
#require(GO.db)
library(dplyr)


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

probnamtoentrezvenn <- function(venngenes, mypack){
  
  entrezids <- venngenes %>%
    unlist() %>%
    as.character() %>%
    mget(x = .,envir = mypack,ifnotfound = NA) %>%
    unlist() %>%
    unique() %>%
    .[!is.na(.)]
  
}


entreztosymb <- function(myentz, mypack){
lapply(1:NROW(myentz), function(x)
  as.vector(unlist(mget(myentz[[x]], envir=mypack, ifnotfound=NA))))
}


davidquery <- function(entrezids, species, mycat) {
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
    #backgroundLocation = grep(selectedSpecie,RDAVIDWebService::getBackgroundListNames(david))
    specieLocation = grep(selectedSpecie, RDAVIDWebService::getSpecieNames(david))
    setCurrentSpecies(object = david, species = specieLocation)
    #setCurrentBackgroundPosition(object = david, position = backgroundLocation)
    #getSpecieNames(david)
    setAnnotationCategories(david, mycat) #c("GOTERM_MF_ALL", "GOTERM_CC_ALL", "GOTERM_BP_ALL")) # "KEGG_PATHWAY"
    mydav = as.data.frame(cbind(getFunctionalAnnotationChart(object=david, threshold=1, count=0L)))  %>%
      filter(Count>1) %>% arrange(desc(Count))  %>% select( Category:Count, List.Total:Pop.Total,X.,PValue,Genes,Fold.Enrichment, Bonferroni, Benjamini)
    colnames(mydav)[[7]] = "percent"
    return(mydav)
  })
}


davidqueryvenn <- function(entrezids, species){
  
  david <- DAVIDWebService$new(email = "franck.soubes@inra.fr", url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
  RDAVIDWebService::setTimeOut(david, 90000)
  
  addList(
    david,
    entrezids,
    idType = "ENTREZ_GENE_ID",
    listName = "myqueryvenn",
    listType = "Gene"
  )
  
  selectedSpecie = (species)
  #backgroundLocation = grep(selectedSpecie,RDAVIDWebService::getBackgroundListNames(david))
  specieLocation = grep(selectedSpecie, RDAVIDWebService::getSpecieNames(david))
  setCurrentSpecies(object = david, species = specieLocation)
  
  # get the cluster report for the upload
  getClusterReport(david, type = "Term")
  
}

  
# Functional Annotation Clustering: new!
# Due to the redundant nature of annotations, Functional Annotation Chart presents similar/relevant annotations repeatedly. 
# It dilutes the focus of the biology in the report.  To reduce the redundancy, the newly developed Functional Annotation Clustering report groups/displays similar annotations together which makes the biology clearer and more focused to be read vs. traditional chart report. 
# The grouping algorithm is based on the hypothesis that similar annotations should have similar gene members.  
# The Functional Annotation Clustering integrates the same techniques of  Kappa statistics to measure the degree of the common genes between two annotations, and  fuzzy heuristic clustering (used in Gene Functional Classification Tool ) to classify the groups of similar annotations according kappa values. 
# In this sense, the more common genes annotations share, the higher chance they will be grouped together.
# The p-values associated with each annotation terms inside each clusters are exactly the same meaning/values as p-values (Fisher Exact/EASE Score) shown in the regular chart report for the same terms.
# The Group Enrichment Score new! , the geometric mean (in -log scale) of member's p-values in a corresponding annotation cluster, is used to rank their biological significance. 
# Thus, the top ranked annotation groups most likely have consistent lower p-values for their annotation members.






