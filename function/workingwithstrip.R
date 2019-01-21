wkset = read.csv2("data/TOXA_TELL_MA0020_AllChip_WorkingSet.csv")
pdata = read.csv2("data/TOXA_TELL_MA0020_AllChip_pData.csv")

library(ggplot2)
grps= pdata$Grp


geneName=wkset[1,"X"]
geneName

selGenesID= c(seq(1,50,by=3))

dietNames=cbind.data.frame(dietID=1:length(levels(grps)),Diet=levels(grps))
dietNames

dietNamesCol=cbind.data.frame(dietNames,groupsColors=palette()[2:(nrow(dietNames)+1)])
dietNamesCol
View(wkset)

source("function/ggstrip_groups.r")
ggp=ggstrip_groups2(grps=grps , wSet=wkset,SubGrpsID=1:nrow(dietNamesCol), SubIndivID=1:length(grp), probesID=selGenesID, dietNamesCol=dietNamesCol)
