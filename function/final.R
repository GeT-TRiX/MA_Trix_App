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


heatmtoclust = function( hmp01_All, exprData, pval, height= 5){
  
  cut02 = cut(hmp01_All$rowDendrogram, h = height )
  
  
  HCgroupsLab = lapply(cut02$lower, function(x)
    labels(x))
  

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


#https://stackoverflow.com/questions/9063889/how-to-round-a-data-frame-in-r-that-contains-some-character-variables


round_df <- function(df, digits) {
  
  df = lapply(1:NROW(df), function(x){
  nums <- vapply(df[[x]], is.numeric, FUN.VALUE = logical(1))
  df[[x]][,nums] <- round(df[[x]][,nums], digits = digits)
  })
  return(df)
}

## https://www.r-bloggers.com/correctly-reporting-p-values-in-summary-tables-reported-with-xtable/

fixp <- function(x, dig=3){
  #x <- as.data.frame(x)
  if(substr(names(x)[ncol(x)],1,2) != "Pr")
    warning("The name of the last column didn't start with Pr. This may indicate that p-values weren't in the last row, and thus, that this function is inappropriate.")
  x[,ncol(x)] <- round(x[,ncol(x)], dig)
  for(i in 1:nrow(x)){
    if(x[i,ncol(x)] == 0)
      x[i,ncol(x)] <- paste0("< .", paste0(rep(0,dig-1), collapse=""), "1")
  }
  
  x
}


#require(goseq)
#require(GO.db)
library(dplyr)


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


#' wclust is a function that return a tabular file containing the top n genes for the different clusters, the go ids associated to this cluster, the id's term and the definition of the term
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
    mget(x = .,envir = mypack,ifnotfound = NA) %>%
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


#' davidqueryvenn is a function which aim is to querrying DWS and performing Functional Annotation Clustering
#'
#' @param entrezids list of entrez IDS
#' @param species character name of the species whose genes are enriched
#'
#' @return david object
#' @export
#'

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



require(VennDiagram)

#Intersect, Union and Setdiff (https://stackoverflow.com/questions/23559371/how-to-get-the-list-of-items-in-venn-diagram-in-r)

#' Intersect is a function that takes a list as argument and return the identical elements between those lists 
#'
#' @param x list
#'
#' @return vector
#' @export
#' 
#' @examples
#' x <- c(sort(sample(1:20, 9)))
#' y <- c(sort(sample(3:23, 7)))
#' test = list()
#' test[[1]] = x
#' test[[2]] = y
#' Intersect(test) = 9,19

Intersect <- function (x) {  
  
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    intersect(x[[1]], x[[2]])
  } else if (length(x) > 2){
    intersect(x[[1]], Intersect(x[-1]))
  }
}

#' Union is a function that takes a list as argument and return an union of those lists
#'
#' @param x list
#'
#' @return vector
#' @export
#' 
#' @examples
#' x <- c(sort(sample(1:20, 9)))
#' y <- c(sort(sample(3:23, 7)))
#' test = list()
#' test[[1]] = x
#' test[[2]] = y
#' Union(test) = 1  2  4  9 11 14 16 17 19  3  6 10 12 21


Union <- function (x) {  
  

  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    union(x[[1]], x[[2]])
  } else if (length(x) > 2) {
    union(x[[1]], Union(x[-1]))
  }
}

#' Setdiff is a function that remove the union of the y's from the common x's, x and y are lists of characters.
#'
#' @param x list of characters
#' @param y list of characters
#'
#' @return
#' @export
#' 

Setdiff <- function (x, y) {
  
  xx <- Intersect(x)
  yy <- Union(y)
  setdiff(xx, yy)
}


#' Vennlist is a function which aim is to return a list of signficant genes for a treshold defined by the user
#'
#' @param adj dataframe subset of the alltoptable
#' @param fc dataframe subset of the alltoptable
#' @param regulation character for up both or down
#' @param cutoffpval numeric value
#' @param cutofffc numeric value
#'
#' @return list.s
#' 
#' @export



Vennlist <- function(adj,fc, regulation, cutoffpval, cutofffc){ ## ajout de foreach parallel
  
  if(is.null(adj)) 
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
#' @param adj dataframe subset of the alltoptable
#' @param cex vector giving the size for each area label (length = 1/3/7/15 based on set-number)
#' @param cutoffpval numeric value
#' @param cutofffc numeric value
#' @param statimet character 
#' @param meandup character 
#' @param pval data frame of the alltoptable
#'
#' @return final draw on the current device of the venn diagram
#' @export
#' 

Vennfinal <- function(myl,adj, cex=1, cutoffpval, cutofffc, statimet, meandup = F, pval){ 
  
  if(is.null(myl))
    return(NULL)
  
  
  if(meandup){
    myl = lapply(seq(length(myl)), function(x){pval %>% select(GeneName, ProbeName) %>% filter( ProbeName %in% myl[[x]]) %>% 
        distinct( GeneName)}) %>%
      as.matrix()
    myl = lapply(1:length(myl),FUN = function(i) as.character(myl[[i]]$GeneName))  #D14Ertd668e doublons pour LWT_MCD.LWT_CTRL (si genesymbol -1 perte d'info) avec une probe nom partagée avec LKO_MCD.LKO_CTRL  et une probe partagée
  
  }
  
  metuse = ifelse(statimet == "FDR","DEG BH ", "DEG RAW ")
  
  indexnull = which( sapply(myl ,length) == 0)
  myl <- myl[sapply(myl, length) > 0]
  final = length(myl)-1
  totgenes =  sum(sapply(myl,length))
  totprobes=  totalvenn(myl, adj)
  mynumb = paste("total probes:", totgenes ,"and total probe crossings:",totprobes, collapse = "")

  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  mytresh = paste0(metuse, cutoffpval, " and FC " , cutofffc)
  
  
  if(length(myl)==2){
     if (length(myl[[2]])> length(myl[[1]]))
       mynames = rev(colnames(adj))
     else
       mynames = colnames(adj)
  }
  else
    mynames = colnames(adj)

  
  
  if(length(indexnull)>0){
    if(length(myl)==5){
      print(colnames(adj[,-c(indexnull)]))
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1, cat.just= list(c(0.6,1) , c(0,0) , c(0,0) , c(1,1) , c(1,0)),
                   category.names = mynames[,-c(indexnull)],fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                   fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
    }
    else{
      print(colnames(adj[,-c(indexnull)]))
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,
                     category.names = mynames[,-c(indexnull)],fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                     fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
    }
  }
  else{
      if(length(myl)==5){
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,cat.just=  list(c(0.6,1) , c(0,0) , c(0,0) , c(1,1) , c(1,0)) ,
                     category.names = mynames,fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                     fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop4
      }
      else{

        g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,
                         category.names = mynames,fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                         fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
      }
  }
  
  final = grid.arrange(gTree(children=g), top="Venn Diagram", bottom= mytresh)
  
  
  
  return(final)
}


#' Vennsev is a function that is used for more than 5 intersections
#'
#' @param myl a list of genes for the different contrasts
#' @param adj dataframe subset of the alltoptable
#'
#' @return plot device
#' @export
#' 


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
#' @export
#' 

myventocsv <- function(myven, adj){
  
  
  max.length <- max(sapply(myven, length))
  myven %>%
    lapply(function(v){ c(v, rep("", max.length-length(v)))}) %>%
    setNames(names(adj)) %>%
    as.data.frame()

}

#' totalvenn is a function which aim is to return the total element of each interesections for the venn diagram
#'
#' @param vennlist a list of genes for the different contrasts
#' @param adj dataframe subset of the alltoptable
#'
#' @return numeric value
#' @export
#' 

totalvenn <- function(vennlist,adj){
  
  names(vennlist) = colnames(adj)
  elements <- 1:length(vennlist) %>% lapply(function(x)
      combn(names(vennlist), x, simplify = FALSE)) %>%
    unlist(recursive = F) %>% setNames(., sapply(., function(p)
      paste0(p, collapse = ""))) %>%
    lapply(function(i)
      Setdiff(vennlist[i], vennlist[setdiff(names(vennlist), i)])) %>% .[sapply(., length) > 0]
  
  n.elements <- sapply(elements, length)

  
  return(sum(n.elements))
}


#' setvglobalvenn is a function which aim is to return lists of each probes for the different set of intersections 
#'
#' @param vennlist a list of genes for the different contrasts
#' @param adj dataframe subset of the alltoptable
#'
#' @return list of probes
#' @export

setvglobalvenn <- function(vennlist,adj){
  
  names(vennlist) = colnames(adj)
  elements <- 1:length(vennlist) %>% lapply(function(x)
    combn(names(vennlist), x, simplify = FALSE)) %>%
    unlist(recursive = F) %>% setNames(., sapply(., function(p)
      paste0(p, collapse = ""))) %>%
    lapply(function(i)
      Setdiff(vennlist[i], vennlist[setdiff(names(vennlist), i)])) %>% .[sapply(., length) > 0]
  
  
  return(elements)
}



#' rowtoprob is a function that return the probe names for the corresponding indexes
#'
#' @param myven a list of index for the different contrasts
#' @param pval dataframe of the alltoptable
#' @param adj dataframe subset of the alltoptable
#'
#' @return list
#' @export


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



#' topngenes is a function to plot the top n genes for a defined intersection between comparison.s
#'
#' @param dfinter list of intersection.s
#' @param mycont character Vector 
#' @param inputtop numeric value
#' @param meandup character
#'
#' @return ggplot2 barplot
#' @export
#'

topngenes <- function(dfinter, mycont, inputtop, meandup = F) {
  
  
  
  if(!meandup)
    dfinter$GeneName = make.names(dfinter$GeneName, unique = T)

  
  mycont = gsub("-"," vs logFC_" ,mycont)
  colnames(dfinter)= lapply(colnames(dfinter),function(x){
    
    if(grepl("-",x))
      x = gsub("-"," vs logFC_" ,x)
    
    return(x)})
  

  
  reshp <-melt(
    dfinter[1:inputtop, ],
    id.vars = "GeneName",
    measure.vars = c (mycont),
    variable.name = "Comparisons",
    value.name = "logFC"
  )
  
  reshp <- droplevels(reshp)
  reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))
  
  # maxval = as.numeric(max(reshp$logFC))
  # minval = as.numeric(min(reshp$logFC))
  # print(c(minval-2,maxval+2))
  
  
  p <- ggplot(reshp, aes(
    x = GeneName,
    y = as.numeric(as.character(formatC(as.double(logFC), digits = 1, format = "f"))),
    fill = Comparisons
  )) +
    geom_bar(stat = "identity", position = "dodge") +
    
   # coord_cartesian(ylim=c(minval-2,maxval+2))+
    # scale_fill_discrete(
    #   name = "GeneName",
    #   breaks = c(seq(mycont)),
    #   labels = c(mycont) )+
    
    scale_fill_manual(values = c("red","blue",'purple',"green","black")) + 
    
    xlab("Gene Names") + ylab("Log Fold-Change") +
    #theme_classic() + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "white"),
      plot.title = element_text(size = 20, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0.5),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10) ,
      axis.text.x = element_text(
        size = 11,
        colour = "#808080",
        angle = 80,
        hjust = 1
      ),
      axis.text.y = element_text(size = 8, colour = "#808080"),
      legend.position="top"
    ) 
    
  #print(unique(sort(c(seq(as.numeric(maxval)),seq(as.numeric(minval))))))
  
  print(p)
  
  return( p)
  
}




