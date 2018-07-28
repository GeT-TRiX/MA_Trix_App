library(xlsx)
read.xlsx2(file= "MA0706_go.xlsx")[[2]]
library(readxl)
gotab <- read.xlsx2("MA0191_go.xlsx", sheetIndex = 2)
View(gotab)
library(dplyr)
colnames(gotab)
unique(gotab$Category)
View(gotab)
View(gotab[[1]])
colnames(gotab)


sapply(gotab, function(x)return(x))

resumgotab <- function(gotab){
 lapply(seq(NROW(unique(gotab$Category))), function(x){
  myl = unique(gotab$Category)
  return(gotab %>% filter(Category == myl[[x]]) %>% 
           select(Category, Term, Fold.Enrichment, Benjamini,Count,List.Total) %>% 
           top_n(10, Fold.Enrichment) %>% arrange(desc(Fold.Enrichment)))})
}

test = resumgotab(gotab)
View(test)
View(test[[3]])
as.numeric(as.character(test[[3]]$Count))/as.numeric(as.character(test[[3]]$List.Total))*100
test[[2]]$Count
test[[2]]$List.Total
3/106



reumdiff = lapply(1:length(test),function(x)return(sapply(length(test[[x]]$Count), function(y){
  return(as.numeric(as.character(test[[x]]$Count))/as.numeric(as.character(test[[x]]$List.Total))*100)}))%>%  mutate(test[[x]],percent = .)) 

View(reumdiff[[1]])





reumdiff
View(test[[4]])
test[[1]]$wewe= reumdiff[[1]]
View(test)
for(i in 1:length(test))
  print(i)
  


lapply(1:length(test),function(x){
  return(test[[x]]$Count)})


length(test[[1]]$Count)

sapply(1:length(test), function(x)return(x))

length(test)
View(gotab)
final <- gotab %>% select(Category,Term, Fold.Enrichment, Benjamini) %>% top_n(10, Fold.Enrichment) 
View(final)

kegg = gotab %>% filter(Category == "KEGG_PATHWAY") %>% 
  select(Category, Term, Fold.Enrichment, Benjamini) %>% 
  dplyr::arrange(desc(Fold.Enrichment))

dplyr::
View(kegg)
??dplyr::desc
