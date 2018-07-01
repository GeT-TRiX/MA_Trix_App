library(dplyr)
test <- read.xlsx2("MA0191_go.xlsx")


mygotabres <- function(davtab){
lapply(seq(unique(davtab$Category)), function(x){
  return(davtab %>% select(Category, Term,Fold.Enrichment,Benjamini)%>%
           filter(Category == unique(davtab$Category)[[x]]) %>%
           top_n(10, Fold.Enrichment) %>% arrange(desc(Fold.Enrichment))
  )})
}


