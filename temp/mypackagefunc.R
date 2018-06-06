test <- sessionInfo()
sessionInfo()
library(dplyr)
library(ggplot2)


mypack = cbind(lapply(names(test$otherPkgs), function(x){
  return(paste(test$otherPkgs[[x]]$Package, test$otherPkgs[[x]]$Version))}), 
  lapply(names(test$otherPkgs), function(x)return(test$otherPkgs[[x]]$Title))) %>%
  as.data.frame() 
colnames(mypack)= c("Package","Summary")




View(mypack)
names(test$otherPkgs)
test$otherPkgs[["ggplot2"]]
