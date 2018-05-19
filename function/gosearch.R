require(goseq)
require(GO.db)
library(dplyr)


gosearch <- function(hm01, species, ids, min,max) {
  clusterlist= list()
  for (i in 1:NROW(unique(hm01$cluster))) {
    genlist <- hm01[!duplicated(hm01[2]), ]
    genlist <-
      genlist %>% select(cluster, GeneName)   %>% filter(cluster == i)
    final = as.double(matrix(1, length(genlist$cluster)))
    names(final) = (genlist$GeneName)
    pwf <- final %>% nullp(species, ids) %>% na.omit()
    finalons <- goseq(pwf, species , ids, use_genes_without_cat = T)
    clusterlist[[i]] = filter(finalons, numInCat > min &  numInCat <max) %>%
      arrange(desc(numInCat))
    
  }
  return(clusterlist)
}


wclust <- function(clusterlist, filename,min, top)  {
  cat("Clustering", file=filename)
  sink(filename)
  sink()
  con <- file(filename, "w")
  for (i in 1:NROW(clusterlist)){
    if(!i==1)
      cat("--------------------------------------\n",file=con)
    cat(paste("cluster:", i),  file=con)
    if(!i==1)
      cat("\n--------------------------------------\n",file=con)
    cat("\n--------------------------------------\n",file=con)
    if(!length(clusterlist[[1]][[i]]) == 0) {
      for(go in top:min) {
        cat(paste("GOID:",as.character(GOID(clusterlist[[i]][[1]][[go]]))),file=con)
        cat("\n",file=con)
        cat(paste("Term:",as.character(Term(clusterlist[[i]][[1]][[go]]))),file=con)
        cat("\n",file=con)
        cat(paste("Definition:",as.character(Definition(clusterlist[[i]][[1]][[go]]))),file=con)
        cat("\n",file=con)
        cat("--------------------------------------\n",file=con)
      }
      cat("\n",file=con)
    }
  }
  
  close(con)
}
