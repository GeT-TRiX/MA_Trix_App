test = c("blue","red",'Darkcyan',"caca")
mycol = lapply(test, FUN= function(x)return(col2rgb(x))) %>% as.data.frame()
length(mycol)
test = col2rgb(c(mycol = 1:3)) %>% as.data.frame() %>% lapply(.,function(x)return(x)) 
(test[[1]])


