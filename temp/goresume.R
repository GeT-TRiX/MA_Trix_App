library(xlsx)
read.xlsx2(file= "MA0706_go.xlsx")[[1]]
library(readxl)
gotab <- read.xlsx2("MA0191_go.xlsx", sheetIndex = 1)
View(gotab)
library(dplyr)
colnames(gotab)
unique(gotab$Category)
View(gotab)


resumgotab <- function(gotab){
 lapply(seq(NROW(unique(gotab$Category))), function(x){
  myl = unique(gotab$Category)
  return(gotab %>% filter(Category == myl[[x]]) %>% 
           select(Category, Term, Fold.Enrichment, Benjamini) %>% 
           top_n(10, Fold.Enrichment) %>% arrange(desc(Fold.Enrichment)))})
}

test = resumgotab(gotab)
View(test[[2]])


View(gotab)
final <- gotab %>% select(Category,Term, Fold.Enrichment, Benjamini) %>% top_n(10, Fold.Enrichment) 
View(final)

kegg = gotab %>% filter(Category == "KEGG_PATHWAY") %>% 
  select(Category, Term, Fold.Enrichment, Benjamini) %>% 
  dplyr::arrange(desc(Fold.Enrichment))

dplyr::
View(kegg)
??dplyr::desc
