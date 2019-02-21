### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#' gosearch is a function that return a list of data frame containing the go ids for the different clusters
#'
#' @param hm01 data frame object
#' @param species character
#' @param ids package use to perform the enrichment
#' @param clusterlist list
#'
#' @return list of data frames
#' @export
#'

gosearch <- function(hm01, species, ids, clusterlist) {
  library(goseq)
  library(GO.db)

  for (i in 1:NROW(unique(hm01$cluster))) {
    genlist <- hm01[!duplicated(hm01$GeneName),]
    genlist <-genlist %>% dplyr::select(cluster, GeneName)   %>% filter(cluster == i)
    genfil = as.double(matrix(1, length(genlist$cluster)))
    names(genfil) = (genlist$GeneName)

    h <- function(w)
        if (any(grepl("constraints|library", w)))
          invokeRestart("muffleWarning")

    e <- function(y)
      if(any(grepl("Rplots.pdf", y)))
        invokeRestart("muffleWarning")

    pwf <- tryCatch({
      withCallingHandlers(nullp(genfil, species, ids ,plot.fit=FALSE), warning = h, error = e) %>% na.omit()
    }, warning = function(e) {
      warning("40 % of genes are misssing")
      return(NULL)
    })

    cat(length(row.names(pwf)))

    if (!is.null(pwf)) {
      enrichana <- goseq(pwf, species , ids, use_genes_without_cat = F)
      clusterlist[[i]] = filter(enrichana, numInCat > 1 ) %>%
        arrange(desc(numInCat))
    }
    else
      clusterlist[[i]] = NULL
  }
  return(clusterlist)
}

#' wclust is a function that return a tabular file containing the top n genes for the different clusters, the go ids associated to this cluster, the id's term and the definition of the term (Old function working with go seq package)
#'
#' @param clusterlist list of data frames
#' @param filename name of the output file
#' @param min GO ids that are not represented significally by default = 2
#' @param top top go ids to display
#'
#' @return txt file
#' @export

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
#' probnamtoentrez is a function that convert Gene symbols to entrez IDS
#'
#' @param hm01 data frame
#' @param mypack package specific to the genome
#'
#' @return lists of entrez IDS
#' @export
#'

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

#' probnamtoentrezvenn is a function that convert Gene symbols to entrez IDS
#'
#' @param venngenes lists of genes
#' @param mypack package specific to the genome
#'
#' @return lists of entrez IDS
#' @export
#'
probnamtoentrezvenn <- function(venngenes, mypack){

  entrezids <- venngenes %>%
    unlist() %>%
    as.character() %>%
    mget(x = .,envir = mypack, ifnotfound = NA) %>%
    unlist() %>%
    unique() %>%
    .[!is.na(.)]

}

#' entreztosymb is a function which aim is to convert entrez IDS to gene symbols
#'
#' @param myentz lists of genes
#' @param mypack package specific to the genome
#'
#' @return lists of gene symbols
#' @export
#'

entreztosymb <- function(myentz, mypack){
lapply(1:NROW(myentz), function(x)
  as.vector(unlist(mget(myentz[[x]], envir=mypack, ifnotfound=NA))))
}

#' davidquery is a function which aim is to querrying DWS and performing go term enrichment analysis
#'
#' @param entrezids list of entrez IDS
#' @param species character name of the species whose genes are enriched
#' @param mycat category of the enrichment analysis: MF, CC, BP or KEGG pathway
#'
#' @return list of data frames for each cluster containing
#' @export
#'

davidquery <- function(entrezids, species, mycat) {

  queryclust = lapply(1:NROW(entrezids), function(x) {
    david <- DAVIDWebService$new(email = "get-trix@genotoul.fr", url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
    RDAVIDWebService::setTimeOut(david, 90000)
    result <- addList(
        david,
        entrezids[[x]],
        idType = "ENTREZ_GENE_ID",
        listName = "queryhm",
        listType = "Gene"
    )

    selectedSpecie = (species)
    specieLocation = grep(selectedSpecie, RDAVIDWebService::getSpecieNames(david))
    print(RDAVIDWebService::getSpecieNames(david))
    setCurrentSpecies(object = david, species = specieLocation)
    setAnnotationCategories(david, mycat)
    mydav = as.data.frame(cbind(getFunctionalAnnotationChart(object=david, threshold=1, count=0L)))  %>%
      filter(Count>1) %>% arrange(desc(Count))  %>% dplyr::select( Category:Count, List.Total:Pop.Total,X.,PValue,Genes,Fold.Enrichment, Bonferroni, Benjamini)
    colnames(mydav)[[7]] = "percent"
    return(mydav)
  })
}


#' davidqueryvenn is a function which aim is to querrying DWS and performing Functional Annotation Clustering
#'
#' @param entrezids list of entrez IDS
#' @param species character name of the species whose genes are enriched
#'
#' @return david object
#' @export
#'

davidqueryvenn <- function(entrezids, species){

  david <- DAVIDWebService$new(email = "get-trix@genotoul.fr", url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
  RDAVIDWebService::setTimeOut(david, 90000)
  addList(
    david,
    entrezids,
    idType = "ENTREZ_GENE_ID",
    listName = "myqueryvenn",
    listType = "Gene"
  )

  selectedSpecie = (species)
  specieLocation = grep(selectedSpecie, RDAVIDWebService::getSpecieNames(david))
  setCurrentSpecies(object = david, species = specieLocation)
  getClusterReport(david, type = "Term")

}


#' mygotavres is a function which aim is to summarise the top 10 for each different cagetogies of the  the DAVID gene set enrichment analysis data table
#'
#' @param davtab data frame
#'
#' @return list of data frames
#' @export
#'

mygotabres <- function(davtab, enrichbased){
  print(enrichbased)
  lapply(seq(unique(davtab$Category)), function(x){
    return(davtab %>% select(Category, Term,Fold.Enrichment,Benjamini,Count,List.Total,Pop.Hits, PValue)%>%
             filter(Category == unique(davtab$Category)[[x]]) %>%{ if(enrichbased == "FoldE") top_n(10, Fold.Enrichment) %>% arrange(desc(Fold.Enrichment)) %>% tibble::rownames_to_column("Top")  else top_n(-10, PValue) %>% arrange(desc(-PValue))%>% tibble::rownames_to_column("Top") } # PValue 
             
    )})
}


