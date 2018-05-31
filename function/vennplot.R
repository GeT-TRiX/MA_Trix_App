require(VennDiagram)
#library(venn)


#' Vennlist is a function which aim is to return a list of signficant genes for a treshold pvalue of 5%
#'
#' @param pval a data frame
#' @param adj a data frame with the contrast selected
#' @param fc a data frame with the constrast selected
#' @param regulation a character
#'
#' @return \myl a list
#' 

Vennlist <- function(pval,adj,fc, regulation, cutoffpval, cutofffc){
  
  
  if(is.null(pval)) 
    return(NULL)
  
  reguser = ifelse(regulation == "up", T, F)
  reguserboth = ifelse(regulation == "both", T, F)
  myl=list()
  if(reguser && !reguserboth){
    for(i in 1:ncol(adj)){
        myl[[i]] = which(adj[[i]] < cutoffpval & fc[[i]] > log2(cutofffc))
    }
  }
  else if(!reguser && !reguserboth){
    for(i in 1:ncol(adj)){
      myl[[i]] = which(adj[[i]] < cutoffpval & fc[[i]] < -log2(cutofffc))
    }
  }
  else{
    for(i in 1:ncol(adj)){
      myl[[i]] = which(adj[[i]] < cutoffpval  )
    }
  }
  
  return(myl)
}


#' Vennfinal is a function which aim is to return an object containing a venn diagram 
#' 
#' @param myl a list of genes for the different contrasts
#' @param adj a data frame 
#' @param cex a vector giving the size for each area label 
#'
#' @return \final draw on the current device
#' 

Vennfinal <- function(myl,adj, cex=1, cutoffpval){
  
  if(is.null(myl))
    return(NULL)
  
  
  indexnull = which( sapply(myl ,length) == 0)
  myl <- myl[sapply(myl, length) > 0]
  final = length(myl)-1
  test = sum(sapply(myl,length))
  mynumb = paste("total genes", test , collapse = ":")
  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  mytresh = paste0("DEG BH ", cutoffpval)
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
  test = sum(sapply(myl,length))
  mynumb = paste("total genes", test , collapse = ":")
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
    setNames(colnames(adj)) %>%
    as.data.frame()

}




# pval <- read.csv2("data/All_topTableAll.csv")
# adj = pval[,grep("^adj.P.Val", names(pval), value=TRUE)]
# fc = pval[,grep("^logFC", names(pval), value=TRUE)]
# View(fc)
# View(pval)
# library(dplyr)
# myven = Vennlist(pval, adj[1:5], fc[1:5], "test")
# print(myven)

# test = myventocsv(myven,adj[1:5])
# View(test)

# View(myven)
# df <- data.frame(matrix(unlist(myven), ncol = 5))
# colnames(df) = colnames(adj)
# View(df)
# write.csv(myven, file = "MyData.csv")
# 


# max.length <- max(sapply(myven, length))
# l <- lapply(myven, function(v) { c(v, rep("", max.length-length(v)))})
# test = do.call(cbind, l)
# colnames(test) = colnames(adj)
# write.table(test, "cnbd.csv",
#             na = "",
#             row.names = F,
#             col.names = T,
#             append = TRUE,
#             sep = ";")



# 
#myventocsv(myven,adj)

# myven
# adj[1:5]
# 
# 
# Vennfinal(myven,adj[1:5])
#g = venn(myven, ilabels= F, zcolor ="style", sname = colnames(adj), cexil = 0.5, size = 5, cexsn = 0.5)

# final = grid.arrange(gTree(children=g), top="Venn Diagram", bottom="DEG BH 0.05")
# help(venn)
# 
# evenn(data("Data_Lists"), annot = T, display = Yndisplay, ud  =T, Profils = T)
# print(myven[[1]])
# 
# final= list(A=myven[[1]],B=myven[[2]],C=myven[[4]],D= myven[[5]])
# plot(euler(final))
# 
# help(euler)
# combo <- c(A = 2, B = 2, C = 2, "A&B" = 1, "A&C" = 1, "B&C" = 1)
# fit1 <- euler(combo)
# final = euler(list(A = c("a", "ab", "ac", "abc"),
#            B = c("b", "ab", "bc", "abc"),
#            C = c("c", "ac", "bc", "abc")))
# 
# 
# w <- compute.Venn(Venn(final))
# gp <- VennThemes(w)
# plot(w, types= squares)
# 
# test = Vennerable::Venn(myven)
# 
# plot(test)
# 
# 
# plot(final)
# 
# Vennfinal(myven,adj)
# data("Data_Lists")
