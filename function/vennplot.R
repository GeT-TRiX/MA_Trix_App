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

Vennfinal <- function(myl,adj, cex=1, cutoffpval, cutofffc, statimet){ 
  if(is.null(myl))
    return(NULL)
  
  
  metuse = ifelse(statimet == "FDR","DEG BH ", "DEG RAW ")
  
  indexnull = which( sapply(myl ,length) == 0)
  myl <- myl[sapply(myl, length) > 0]
  final = length(myl)-1
  totgenes = sum(sapply(myl,length))
  mynumb = paste("total genes", totgenes , collapse = ":")
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  #mytresh = paste0("DEG BH ", cutoffpval, " and FC " , cutofffc)
  mytresh = paste0(metuse, cutoffpval, " and FC " , cutofffc)
  if(length(indexnull)>0){
    if(length(myl)==5)
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1, cat.just= list(c(0.6,1) , c(0,0) , c(0,0) , c(1,1) , c(1,0)),
                   category.names = colnames(adj[,-c(indexnull)]),fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                   fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
    else
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,
                     category.names = colnames(adj[,-c(indexnull)]),fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                     fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
  }
  else{
      if(length(myl)==5)
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,cat.just=  list(c(0.6,1) , c(0,0) , c(0,0) , c(1,1) , c(1,0)) ,
                     category.names = colnames(adj),fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                     fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
      else
        g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,
                         category.names = colnames(adj),fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                         fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
  }
  
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

myventocsv <- function(myven, adj){
  
  
  max.length <- max(sapply(myven, length))
  myven %>%
    lapply(function(v){ c(v, rep("", max.length-length(v)))}) %>%
    setNames(names(adj)) %>%
    as.data.frame()

}

setvglobalvenn <- function(vennlist,adj){
  
  names(vennlist) = colnames(adj)
  elements <- 1:length(vennlist) %>% lapply(function(x)
      combn(names(vennlist), x, simplify = FALSE)) %>%
    unlist(recursive = F) %>% setNames(., sapply(., function(p)
      paste0(p, collapse = ""))) %>%
    lapply(function(i)
      Setdiff(vennlist[i], vennlist[setdiff(names(vennlist), i)])) %>% .[sapply(., length) > 0]
  
  
  # global <- unlist(lapply(1:length(vennlist),
  #                         function(x) combn(names(vennlist), x, simplify = FALSE)),
  #                  recursive = FALSE)
  # names(global) <- sapply(global, function(p) paste0(p, collapse = ""))
  # elements <- lapply(global, function(i) Setdiff(vennlist[i], vennlist[setdiff(names(vennlist), i)]))
  # elements <- elements[sapply(elements, length) > 0]
  
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



topngenes <- function(dfinter, mycont, inputtop, meandup = F) {
  
  
  if(!meandup)
    dfinter$GeneName = make.names(dfinter$GeneName, unique = T)
  else{
    logval <- "logFC_" %>%
      grepl(colnames(dfinter))%>%
      which(.==T)
    
    for (i in mycont) {
      dfinter[[i]] = as.numeric(as.character(dfinter[[i]]))
    }
    
    dfinter <- dfinter[,-1] %>% as.data.table() %>% .[,lapply(.SD,mean),"GeneName"] 
    dfinter = as.data.frame(dfinter)

  }

  
  mycont = gsub("-"," vs " ,mycont)
  colnames(dfinter)= lapply(colnames(dfinter),function(x){
    
    if(grepl("-",x))
      x = gsub("-"," vs " ,x)
    
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
        size = 8,
        colour = "#000000",
        angle = 80,
        hjust = 1
      ),
      axis.text.y = element_text(size = 8, colour = "#000000"),
      legend.position="top"
    ) 
    
  #print(unique(sort(c(seq(as.numeric(maxval)),seq(as.numeric(minval))))))
  
  print(p)
  
  return( p)
  
}







# topngenes <- function(dfinter, mycont, inputtop) {
#   dfinter$GeneName = make.names(dfinter$GeneName, unique = T)
#   
#   reshp <-
#     melt(
#       dfinter[1:inputtop, ],
#       id.vars = "GeneName",
#       measure.vars = c (mycont),
#       variable.name = "Source",
#       value.name = "logFC"
#     )
#   reshp <- droplevels(reshp)
#   reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))
#   
#   
#   
#   p <- ggplot(reshp, aes(
#     x = GeneName,
#     y = as.numeric(as.character(formatC(as.double(logFC), digits = 1, format = "f"))),
#     fill = factor(Source)
#   )) +
#     geom_bar(stat = "identity", position = "dodge") +
#     scale_fill_discrete(
#       name = "GeneName",
#       breaks = c(1, 2),
#       labels = c(mycont)
#     ) +
#     scale_fill_manual(values = c("red","blue")) + 
#     
#     
#     xlab("Gene Name") + ylab("Log Fold-Change") +
#     theme(
#       panel.grid.major = element_blank(),
#       panel.grid.minor = element_blank(),
#       panel.background = element_blank(),
#       axis.line = element_line(colour = "white"),
#       plot.title = element_text(size = 20, hjust = 0.5),
#       plot.caption = element_text(size = 10, hjust = 0.5),
#       axis.title.x = element_text(size = 10),
#       axis.title.y = element_text(size = 10) ,
#       axis.text.x = element_text(
#         size = 8,
#         colour = "#888888",
#         angle = 80,
#         hjust = 1
#       ),
#       axis.text.y = element_text(size = 8, colour = "#888888")
#     )
#   
#   
#   print(p)
# 
#   return(p)
# }


