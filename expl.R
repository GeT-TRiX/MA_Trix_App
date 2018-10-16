library(dplyr)
test = read.csv2("data/All_topTableAll.csv")
View(test)
names(test)
splitoptable <- test %>% select(., GeneName,adj.P.Val_.LWT_CARBO.LWT_CTRL...LKO_CARBO.LKO_CTRL. , logFC_.LWT_CARBO.LWT_CTRL...LKO_CARBO.LKO_CTRL.)



test = grep(pattern = "Cyp", splitoptable$GeneName) %>% slice(splitoptable,.) %>% select(GeneName) %>% unlist() %>% as.character()
test
typeof(test)
class(test)

splitoptable$GeneName[test]
slice(splitoptable, test)

grep(pattern = "Cyp", splitoptable$GeneName) %>% filter(splitoptable,  .)

View(splitoptable$GeneName[29])

test <- ""
ifelse(test =="", "ok","pasok")

exists(test)
is.empty.model(test)
is.na(test)
is.null(test)
print(is.null(test))
