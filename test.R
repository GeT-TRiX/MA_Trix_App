library(dplyr)
test = c("blue","red",'Darkcyan')
mycol = lapply(test, FUN= function(x)return(col2rgb(x))) %>% as.data.frame()
mycol
length(mycol)
test = col2rgb(c(test)) %>% as.data.frame() %>% lapply(.,function(x)return(x)) 
test
(test[[1]])


