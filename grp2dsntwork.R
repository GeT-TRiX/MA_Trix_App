
library(data.table)

all = fread(
  "/home/franck1337/toast/TOXA_HEGU_MA0659_All_WorkingSet.csv",
  data.table = F,
  check.names = F,
  header = T,
  sep = ";",
  dec = ","
) #benchmark fread memory speed
)
View(all)
all

pdata = fread(
  "/home/franck1337/toast/All_Wa_topTableAll.csv",
  data.table = F,
  check.names = F,
  header = T,
  sep = ";",
  dec = ","
) #benchmark fread memory speed
)
View(pdata)
View(all)
csvord = as.data.frame(colnames(all[,-1]))
colnames(csvord)[1] = "X"
csvord$Grp = gsub("[^A-Z|_|^a-z]", "", csvord$X )

View(csvord)

colnames(csvord)

View(csvord)
colnames(csvord[[2]])[1] = "X"
csvord[[2]]$Grp = gsub("[^A-Z|_|^a-z]", "", csvord[[2]]$X )


