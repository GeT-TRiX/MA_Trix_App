library(dplyr)
monitor <- "monitor.log"
monitor=read.table(file=monitor,header=T) %>% as.data.frame() %>% 
dplyr::filter(., grepl("[0-9]",app))%>% arrange(.,app)
write.csv(FILE,file="monitor.csv")

