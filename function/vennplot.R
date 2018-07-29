

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

Vennfinal <- function(myl,adj, cex=1, cutoffpval, cutofffc, statimet, meandup = "probes", pval, mycol= ""){ 
  
  #if(is.null(myl))
    #return(NULL)
  
  palette("default")
  if(meandup == "genes"){
    myl = lapply(seq(length(myl)), function(x){pval %>% select(GeneName, ProbeName) %>% filter( ProbeName %in% myl[[x]]) %>% 
        distinct( GeneName)}) %>%as.matrix()
    
    myl = lapply(1:length(myl),FUN = function(i) as.character(myl[[i]]$GeneName))  #D14Ertd668e doublons pour LWT_MCD.LWT_CTRL (si genesymbol -1 perte d'info) avec une probe nom partagée avec LKO_MCD.LKO_CTRL  et une probe partagée
  }
  metuse = ifelse(statimet == "FDR","DEG BH ", "DEG RAW ")
  
  indexnull = which( sapply(myl ,length) == 0)
  if(length(indexnull)>0) test = colnames(adj[,-c(indexnull)]) else  test = colnames(adj)
  myl <- myl[sapply(myl, length) > 0]
  final = length(myl)-1
  if(mycol =="") mycolven= 2:(2+final) else mycolven = mycol
  totgenes =  sum(sapply(myl,length))
  totprobes=  totalvenn(myl, test)
  mynumb = paste("total ", meandup,  ":", totgenes ,"and total ", meandup,  "crossings :",totprobes, collapse = "")

  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  mytresh = paste0(metuse, cutoffpval, " and FC " , cutofffc)
  
  
  if(length(myl)==2){
     if (length(myl[[2]])> length(myl[[1]]))
       mynames = rev(colnames(adj))
     else
       mynames = test
  }
  else
    mynames = test

  if(length(indexnull)>0){
    if(length(myl)==5){
      print(colnames(adj[,-c(indexnull)]))
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1, cat.just= list(c(0.6,1) , c(0,0) , c(0,0) , c(1,1) , c(1,0)),
                       category.names = mynames,fill = list(mycolven) , alpha = 0.3, sub=mynumb, cex=1, 
                       fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
    }
    else{
      print(colnames(adj[,-c(indexnull)]))
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


mysetventocsv <- function(myven){

  max.length <- max(sapply(myven, length))
  myven %>%lapply(function(v){ c(v, rep("", max.length-length(v)))}) %>% as.data.frame()
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

#' setvglobalvenn is a function which aim is to return lists of each probes for the different set of intersections 
#'
#' @param vennlist a list of genes for the different contrasts
#' @param adj dataframe subset of the alltoptable
#'
#' @return list of probes
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

#' rowtoprob is a function that return the probe names for the corresponding indexes
#'
#' @param myven a list of index for the different contrasts
#' @param pval dataframe of the alltoptable
#' @param adj dataframe subset of the alltoptable
#'
#' @return list
#' @export

rowtoprob <- function(myven,pval,adj) {
  
  names(myven) = colnames(adj)
  probesel = lapply(
    names(myven),
    FUN = function(x) 
      return( pval %>%filter( rownames(.)%in% myven[[x]]) %>%
                select(ProbeName) %>%unlist() %>%
                as.character())
  )
  
  genesel = lapply(
    names(myven),
    FUN = function(x) 
    return( pval %>%filter(rownames(.)%in% myven[[x]]) %>%
                select(GeneName) %>%unlist() %>%as.character())
  )

  return(list(probesel, genesel))
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

topngenes <- function(dfinter, mycont, inputtop, meandup = "probes", mean = F) {
  
  
  if(meandup == "probes")
    dfinter$GeneName = make.names(dfinter$GeneName, unique = T)
  
  if(mean == T){
    
    logval <- "logFC_" %>%
      grepl(colnames(dfinter))%>%
      which(.==T)
    
    for (i in mycont) {
      dfinter[[i]] = as.numeric(as.character(dfinter[[i]]))
    }
    
    dfinter <- dfinter[,-1] %>% as.data.table() %>% .[,lapply(.SD,mean),"GeneName"] 
    dfinter = as.data.frame(dfinter)
    
    }
    
  
  
  
  mycont = gsub("-"," vs logFC_" ,mycont)
  colnames(dfinter)= lapply(colnames(dfinter),function(x){
    
    if(grepl("-",x))
      x = gsub("-"," vs logFC_" ,x)
    
    return(x)})
  

  reshp <-melt(dfinter[1:inputtop, ],
    id.vars = "GeneName",measure.vars = c (mycont),
    variable.name = "Comparisons",value.name = "logFC"
  )
  
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

