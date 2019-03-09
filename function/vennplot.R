### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0

#Intersect, Union and Setdiff (https://stackoverflow.com/questions/23559371/how-to-get-the-list-of-items-in-venn-diagram-in-r)

#' Intersect is a function that takes a list as argument and return the identical elements between those lists
#'
#' @param x A list of elements
#'
#' @return A vector
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
#' @param x A list of elements
#'
#' @return A vector
#'
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
#' @param x List of characters
#' @param y List of characters
#'
#' @return A vector
#' @export
#'

Setdiff <- function (x, y) {

  xx <- Intersect(x)
  yy <- Union(y)
  setdiff(xx, yy)
}



#' Vennlist is a function which aim is to return a list of signficant genes for a treshold defined by the user
#'
#' @param adj A dataframe subset of the alltoptable
#' @param fc A dataframe subset of the alltoptable
#' @param regulation A character for regulation ("up", "both" or "down")
#' @param cutoffpval Cut-off for absolute log2 fold-change; default = 1.0
#' @param cutofffc Cut-off for pvalue; default = 0.05
#'
#' @return A list of significant genes for each contrast
#'
#' @export

Vennlist <- function(adj,fc, regulation, cutoffpval, cutofffc){

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



#' Vennfinal is a function which aim is to return an object containing a venn diagram (old function with Venndiagram had been change with Jvenn)
#'
#' @param myl A list of genes for the different contrasts
#' @param adj A dataframe subset of the alltoptable
#' @param cex A vector giving the size for each area label (length = 1/3/7/15 based on set-number)
#' @param cutofffc Cut-off for absolute log2 fold-change
#' @param cutoffpval Cut-off for pvalue
#' @param statimet A character
#' @param meandup A character
#' @param pval A data frame of the restable
#' @param mycol A character vector of selected contrast(s)
#'
#' @return Draw on the current device a venn diagram
#' @export
#'

Vennfinal <- function(myl,adj, cex=1, cutoffpval, cutofffc, statimet, meandup = "probes", pval, mycol= ""){



  palette("default")
  if(meandup == "genes"){
    myl = lapply(seq(length(myl)), function(x){pval %>% select(GeneName, ProbeName) %>% filter( ProbeName %in% myl[[x]]) %>%
        distinct( GeneName)}) %>%as.matrix()

    myl = lapply(1:length(myl),FUN = function(i) as.character(myl[[i]]$GeneName))
  }
  metuse = ifelse(statimet == "FDR","DEG BH ", "DEG RAW ")

  indexnull = which( sapply(myl ,length) == 0)
  if(length(indexnull)>0) comp = colnames(adj[,-c(indexnull)]) else  comp = colnames(adj)
  myl <- myl[sapply(myl, length) > 0]
  final = length(myl)-1
  if(mycol =="") mycolven= 2:(2+final) else mycolven = mycol
  totgenes =  sum(sapply(myl,length))
  totprobes=  totalvenn(myl, comp)
  mynumb = paste("total ", meandup,  ":", totgenes ,"and total ", meandup,  "crossings :",totprobes, collapse = "")

  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  mytresh = paste0(metuse, cutoffpval, " and FC " , cutofffc)


  if(length(myl)==2){
     if (length(myl[[2]])> length(myl[[1]]))
       mynames = rev(colnames(adj))
     else
       mynames = comp
  }
  else
    mynames = comp

  if(length(indexnull)>0){
    if(length(myl)==5){
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1, cat.just= list(c(0.6,1) , c(0,0) , c(0,0) , c(1,1) , c(1,0)),
                       category.names = mynames,fill = list(mycolven) , alpha = 0.3, sub=mynumb, cex=1,
                       fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
    }
    else{
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,
                       category.names = mynames,fill = mycolven, alpha = 0.3, sub=mynumb, cex=1,
                       fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
    }
  }
  else{
      if(length(myl)==5){
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,cat.just=  list(c(0.6,1) , c(0,0) , c(0,0) , c(1,1) , c(1,0)) ,
                     category.names = mynames,fill = mycolven  , alpha = 0.3, sub=mynumb, cex=1,
                     fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop4
      }
      else{
        g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,
                         category.names = mynames,fill = mycolven, alpha = 0.3, sub=mynumb, cex=1,
                         fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
      }
  }

  final = grid.arrange(gTree(children=g), top="Venn Diagram", bottom= mytresh)



  return(final)
}


#' myventocsv is a function that create a csv file of the signficant genes for the different contrasts.
#'
#' @param myven A list of significant genes for each contrasts
#' @param adj A dataframe
#'
#' @return A dataframe containing all the significant genes for each contrast(s)
#'
#' @export
#'

myventocsv <- function(myven, adj){
  max.length <- max(sapply(myven, length))
  myven %>% lapply(function(v){ c(v, rep("", max.length-length(v)))}) %>% setNames(names(adj)) %>% as.data.frame()
}


#' mysetventocsv is a function that create a csv file of the signficant genes for the different contrasts.
#'
#' @param myven A list of significant genes for each contrasts
#'
#' @return A dataframe containing all the significant genes for each contrast(s)
#'
#' @export
#'


mysetventocsv <- function(myven){
  max.length <- max(sapply(myven, length))
  myven %>%lapply(function(v){ c(v, rep("", max.length-length(v)))}) %>% as.data.frame()
}


#' totalvenn is a function which aim is to return the total element of each interesections for the venn diagram
#'
#' @param vennlist A list of genes for the different contrasts
#' @param adj A dataframe subset of the alltoptable
#'
#' @return A numeric value
#' @export
#'

totalvenn <- function(vennlist,adj){

  names(vennlist) = adj
  elements <- 1:length(vennlist) %>% lapply(function(x)
      combn(names(vennlist), x, simplify = FALSE)) %>%
    unlist(recursive = F) %>% setNames(., sapply(., function(p)
      paste0(p, collapse = ""))) %>%
    lapply(function(i)Setdiff(vennlist[i], vennlist[setdiff(names(vennlist), i)])) %>%
    .[sapply(., length) > 0]

  n.elements <- sapply(elements, length)


  return(sum(n.elements))
}

#' setvglobalvenn is a function which aim is to return lists of each probes for the different sets of the venn diagram
#'
#' @param vennlist A list of genes for the different contrasts
#' @param adj A dataframe subset of the alltoptable
#'
#' @return A list of probes/transcripts for all the different sets with the name associated
#' @export

setvglobalvenn <- function(vennlist,adj, dll = F ){

  names(vennlist) = colnames(adj)
  elements <- 1:length(vennlist) %>% lapply(function(x)
    combn(names(vennlist), x, simplify = FALSE)) %>%
    unlist(recursive = F) %>% setNames(., sapply(., function(p){
      if(dll)
        paste0(p, collapse = "vs")
      else paste0(p, collapse = "") })) %>%
    lapply(function(i)
      Setdiff(vennlist[i], vennlist[setdiff(names(vennlist), i)])) %>% .[sapply(., length) > 0]

  return(elements)
}

#' rowtoprob is a function that return the probenames/transcripts for the corresponding indexes
#'
#' @param myven A list of index for the different contrasts
#' @param pval A dataframe of the restable
#' @param adj A subset dataframe of the restable
#'
#' @return A list of transcripts and genes
#' @export

rowtoprob <- function(myven,pval,adj) {

  names(myven) = colnames(adj)
  probesel = lapply(
    names(myven),
    FUN = function(x)
      return( pval %>%filter( rownames(.)%in% myven[[x]]) %>%
                select(colnames(pval[1])) %>%unlist() %>%
                as.character())
  )

  genesel = lapply(names(myven),FUN = function(x)
    return( pval %>%filter(rownames(.)%in% myven[[x]]) %>%
                select(GeneName) %>%unlist() %>%as.character())
  )

  return(list(probesel, genesel))
}

#' filterjvenn is a function which takes as input a list of genes or probes obtained by selecting a set in the venn diagram and return
#' the association of this list with the logFC
#'
#'
#' @param jvennlist A vector of genes generated from the jvenn
#' @param selcontjv A character vector of the selected contrast(s)
#' @param restab A dataframe corresponding to the statistical table
#' @param idcol A character value (transcripts/probes)
#' @param usersel A character value (display genes or probes with the venn diagram)
#' @param venngeneslist A list of transcripts/probes generated from the Vennlist function which aims is to remove probes:transcripts that are not significant
#'
#' @return a list of genes associated with the logFC
#'
#' @export
#'

filterjvenn <- function(jvennlist, selcontjv, restab, idcol,  usersel, venngeneslist = NULL){
  

  ifelse (usersel == "genes",outputjvenntab <- restab %>%
    filter(GeneName %in% jvennlist) %>%
    filter(.[[1]]  %in% venngeneslist) %>%
    select( GeneName, paste0("logFC_",  selcontjv)) %>%
    mutate_if(is.numeric, funs(format(., digits = 3))), outputjvenntab <- restab %>%
    filter(.[[1]] %in% jvennlist) %>%
    select(idcol, GeneName, paste0("logFC_",  selcontjv)) %>%
    mutate_if(is.numeric, funs(format(., digits = 3))))

  return(outputjvenntab)
}



#' toJvenn is a function which aims to convert a dataframe object to json format
#'
#' @param myven A list of genes generates by the Vennlist function
#' @param adj A pvalue subset dataframe of the restable
#'
#' @return A json object
#'
#' @export
#'


toJvenn <- function(myven, adj){


  names(myven) = colnames(adj)
  name   <- rep(names(myven), sapply(myven, FUN=function(x)return(length(x))))
  names(myven) <- NULL
  data <- sapply(myven, FUN=function(x)return(x)) %>% unlist()
  restab  <- data.frame(name,data)

  return(restab %>% group_by(name) %>%
           summarise(data = list(as.character(data))) %>%
           jsonlite::toJSON())

}


#' topngenes is a function which aims is to plot the top n genes for the selected contrat(s)
#'
#' @param dfinter A dataframe which combines (unique ids, genes and logFC)
#' @param mycont A character Vector of the selected comparisons
#' @param inputtop A numeric value
#' @param meandup A character value to get to the level of unique ids or genes
#'
#' @return A ggplot barplot object
#'
#' @export
#'


topngenes <- function(dfinter, mycont, inputtop, meandup = "probes")  {

  if(any(grepl("probes|transcripts", meandup)))
    dfinter$GeneName = make.names(dfinter$GeneName, unique = T)


  mycont = gsub("-"," vs logFC_" ,mycont)
  colnames(dfinter)= lapply(colnames(dfinter),function(x){

    if(grepl("-",x))
      x = gsub("-"," vs logFC_" , x)
    return(x)})


  reshp <-melt(dfinter[1:inputtop, ],
  id.vars = "GeneName",measure.vars = c (mycont),
  variable.name = "Comparisons",value.name = "logFC") %>% na.omit()
  reshp <- droplevels(reshp)
  reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))

  p <- ggplot(reshp, aes(
    x = GeneName,
    y = as.numeric(as.character(formatC(as.double(logFC), digits = 1, format = "f"))),
    fill = Comparisons
  )) +
    geom_bar(stat = "identity", position = "dodge") +


    scale_fill_manual(values = c("red","blue",'purple',"green","black")) +

    xlab("Gene Names") + ylab("Log Fold-Change") +

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

  print(p)
  return( p)

}
