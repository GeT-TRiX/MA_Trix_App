#####################
## Packages imported
#####################

source("environnement/global.R")

library(png)
library(BiocInstaller)
library(limma)
library(dplyr)
library(shiny)
library(foreach)
library(gplots)
library(Biobase)
library(readr)

#####################
## Files importation 
#####################

length(musmuscu)

musmuscu <- read.csv2("data/TOXA_HEGU_MA0191 _AllChip_WorkingSet.csv")
colnames(musmuscu)
mygrep = list(musmuscu, musmuscu , musmuscu)

for (i in 1:length(musmuscu))
  print(colnames(mygrep[[i]]))


colnames(musmuscu)[2:length(musmuscu)] = "test"
names(musmuscu) = gsub(pattern = "^", replacement = "", x = names(your_data))

colnames(testons[[2]])

colnames(test5 == "V1")
class(test5)

colnames(musmuscu[1])

header <- read.table("data/TOXA_HEGU_MA0191 _AllChip_WorkingSet.csv",header = T , nrow =1)
indata = fread("data/TOXA_HEGU_MA0191 _AllChip_WorkingSet.csv", check.names = F, sep =';', header=T, dec=",")
setnames(indata, colnames(header))
colnames(indata[1])

View(pval)
benchmark(
)


benchmark(
test2 = fread("data/All_topTableAll.csv"),
test2= as.data.frame(test2)
)


testons = fread("data/TOXA_HEGU_MA0191 _AllChip_pData.csv",data.table = F)
levels(str(testons$Grp))


chartofa = function(testons){
  
testons[] <- lapply( testons, factor)
col_names <- names(testons)
testons[col_names] <- lapply(testons[col_names] , factor)
return(testons)
}



levels(testons$Grp)

levels(gr$Grp)


gr = read.csv2("data/TOXA_HEGU_MA0191 _AllChip_pData.csv")

class(gr$X)
typeof(testons$V1)


benchmark(
  testons= read.csv2("data/All_topTableAll.csv")
)

factor(testons$Grp)
colnames(testons[2])



benchmark(
pval <- read.csv2("data/All_topTableAll.csv"))

benchmark(
read.table(
  "data/All_topTableAll.csv",
  sep = ";" ,
  dec = ",",
  header = T,
  check.names = F # good col names
))

View(test)



dt1 <- fread("V1 V2 V3
             x b;c;d 1
             y d;ef  2
             z d;ef  3")

splitcol2rows_mget <- function(dtInput, col2split, sep){
  dtInput <- dtInput[, .(tmp.add.col = unlist(strsplit(get(col2split),sep,T))), by=names(dtInput)]
  
  dtInput[, c(col2split):=NULL];
  setnames(dtInput, 'tmp.add.col', col2split); 
  return(dtInput);
}

split = splitcol2rows_mget(dt1, col2split = 'V2' , sep= ";")
split = as.data.frame(split)
colnames(split[1])

files <- list.files("data/", "\\.csv$", full.names = TRUE)

data <- lapply(files, fread)
data <- rbindlist(data, fill = T)


colnames <- strsplit(readLines(textConnection("data/All_topTableAll.csv"), n=1), ";")[[1]]
colnames[1] <- "rownames"
setnames(DT <- fread("data/All_topTableAll.csv", skip=1, header=FALSE), colnames)


View(musmuscu)
data  = subset(musmuscu ,is.na ,select = "LWT_Ctrl2")

test3 = fread("data/TOXA_HEGU_MA0191 _AllChip_WorkingSet.csv")
View(test3)

names(pval) = sapply(strsplit(names(pval), "^adj.P.Val*|^adj.P.Val*"), `[[`, 1)
pval <- read.csv2("data/All_topTableAll.csv")
View(test)


groupss <- read.csv2("data/TOXA_HEGU_MA0191 _AllChip_pData.csv", sep= ";" , dec = ",",header= T)


sort(levels(groupss$Grp))

palette(c("#000000", "#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442", "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray", "burlywood1","darkkhaki", "#CC0000" ));


cl=palette(mypal)

print(cl)
col=cl[(1:length(sort(levels(groupss$Grp))))]
print(col)



gpcol=num2cols(as.numeric(groupss$Grp))

print(gpcol)
num2cols(as.numeric(groupss$Grp))
print(as.numeric(groups$Grp))



View(musmuscu)
View(pval)
View(groupss)
length(colnames(musmuscu))
length(colnames(pval))
length(colnames(groupss))
listed = list(musmuscu,pval,groupss,ncol(musmuscu))
print(listed[[
  4
]])
colnames(pval)
colnames(groupss)



View(groupss)
toast<- levels(groupss$Grp)
typeof(toast)
class(levels(groupss$Grp))
mytoast <- as.list(levels(groupss$Grp))
levels(groupss$Grp)
new <- as.data.frame(groupss)
test <- filter(as.data.frame(new), Grp == c("LKO_MCD ", "LKO_CTRL" ))

test <- subset(new$Grp, rownames == c("LKO_MCD " ))


new %>% filter(row.names(new) %in% c("LKO_CTRL"))




#' transform a dataframe containing factor for different levels function is not optimal right now
#'
#' @param dataframe 
#'
#' @return
#' @export
#'
#' @examples
#' 

transform <- function(dataframe,toast){
  
myl = list()
cpt = 1
for (i in toast) {
  command <- paste0(i, "<-subset(dataframe, Grp=='", i, "')")
  test = eval(parse(text=command))
  X = test$X
  Grp = test$Grp
  myl[[cpt]] = data.frame(X ,Grp)
  cpt = cpt+1
  dyn_grp <- Reduce(function(x, y) merge(x, y, all=TRUE), myl, accumulate=FALSE)
}

return(dyn_grp)
}

toast <- levels(dataframe$Grp)[1:3]
mydata <- transform(groupss,toast)
thisisit <- select(musmuscu, as.character(factor(mydata$X)))
View(mydata)
View(test)


selected = c("LKO_MCD ", "LKO_CTRL" )
selected = levels(groupss$Grp)


#groupss[match(as.character(groupss$Grp), selected, nomatch = T), ]
test <- groupss[groupss$Grp %in% selected,]
print(test)
thisisit <- select(musmuscu, as.character(factor(test$X)))
View(thisisit)


levels(test$Grp)
uniquegrp = unique(test$Grp)
btestos <- droplevels(test)
print(levels(btestos$Grp))

for(i in factor(mydata$X)){
  print(i)
}


typeof(levels(mydata$X))
typeof(factor(mydata$X))



print(as.character(factor(mydata$X)))

View(thisisit)
ncol(thisisit)



test <- fct_c(myl[[1]],myl[[2]])
print(test)
View(test)

df_list <- myl

test <- Reduce(function(x, y) merge(x, y, all=TRUE), myl, accumulate=FALSE)
View(test)

View(test)
print(as.integer(factor(test$X)))

toast <- select(musmuscu ,as.integer(factor(test$X)))


factor(groupss$X)
levels(groupss$Grp)
View(groupss)

levels(colnames(musmuscu))
levels(groupss$X)
levels(groupss$Grp)
factor(groupss$Grp)



length(listed(colnames(groupss)))

print(colnames(listed[[3]][2]))
View(listed)
print(colnames(pval[10]))

test = list()
for (i in 1:length(listed)){
  if( colnames(listed[[i]][2]) == "Grp")
  {
    test[[2]] = listed[[i]]
  }
  else if( colnames(listed[[i]][10]) == "Amean")
  {
    test[[3]] = listed[[i]]
    
  }
  else{
    test[[1]] = listed[[i]]
  }
}

View(test[[3]])
View(listed)


for (i in 1:length(listed)){
  # if (length(colnames(listed[[i])) < length(colnames(listed[[i+1]])))
  # {
  #   print("ok")
  # }
  print(length(colnames(listed[[i]])))
}

j=0
while (j != 3){
  print(j)
  for(i in 1:length(listed))
  {
    final = listed[[i]] %>% 
     ncol() %>%
     append(test)
    print(i)
    
  }
  j = j+1
  print(j)
}


pis =list()
x <- 1:5
test= append(x, pis)
test[[4]]

typeof(pis)


typeof(colnames(musmuscu))
list = c("test")
typeof(list)
#####################
## Working directories
#####################

wd_path="~/stage/data"
#plot(musmuscu$X,musmuscu$LWT_Ctrl2)

#####################
## Source
#####################

#source("plotHeatmaps.r")
source("function/compat.R")
source("function/formating.R")
source("environnement/global.R")

#####################
## Constants
#####################



cutoff = 0.05
ngenes = nrow(pval)
tresh2ways = list(c(0.01,0.05),2)
tresh2ways[[1]][2]
prefix = "Test"
suffix = "Toast"

#mycols = names(musmuscu[grep("^LWT_MCD|^LWT_Ctrl|X|^LKO",names(musmuscu))])

mycols = names(musmuscu[grep("X",names(musmuscu))])

#  palette(c("black", "blue", "cyan", "magenta",   "darkgray", "darkgoldenrod", "violet",  "orange", "lightgreen","lightblue", "darkorchid", "darkred","darkslateblue", "darkslategray", "maroon", "burlywood1" , "darkolivegreen"));
palette(c("#000000", "#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442", "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray", "burlywood1","darkkhaki", "#CC0000" ));



#####################
## Data reshape
#####################


#data = pval[,c("adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.","adj.P.Val_LKO_CTRL.LWT_CTRL")]
adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|^adj.P.Val_LKO_CTRL.LWT_CTRL", names(pval), value=TRUE)]
View(adj)


formating = function( adj, musmuscu,pval){

  passingval = adj %>%
    apply(2,FUN = function(x){return(x < 0.05)}) %>%
    apply(1,sum) 

  passingval = which( passingval > 0)
  cat("Il y a",length(passingval),"gène significatifs")
  
  row.names(musmuscu) = musmuscu$X
  musmuscu <- data.matrix(musmuscu[,-1])
  
  newlist = list(passingval, musmuscu )
  return(newlist)
}


View(adj[,-1])
colnames(adj[,-1])

for (elem in colnames(adj[,-1])){
  print(elem)
  print(formating(elem,musmuscu ,pval = 0.05))
}

adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.", names(pval), value=TRUE)]
treated = formating(adj,musmuscu,pval= 0.05)

View(treated[[1]])

#####################
## Heatmap
#####################

#### method de corr = 1 - corr afin que des valeurs négatives ne soit pas autant corréler que pour des valeures proches de 1
#### 1- (-1) --> 2
#### 1-0.999 


source("function/compat.R")
source("environnement/global.R")
source("function/compat.R")
groupss = read.csv2("data/TOXA_HEGU_MA0191 _AllChip_pData.csv")
source("function/plotHeatmaps.r")

treatedd = treated[[2]]
rownames(treated[[2]])
row.names(treatedd) = pval$GeneName
View(treatedd)
print(row.names(treated[[2]]))
genename = row.names(treatedd)
print(genename[num])


View(pval)
View(musmuscu)
num = treated[[1]]
print(treated[[1]])
print(treated[[1]])

testos = c("green","red","orange","blue")
x11()
hmp01_All= plotHeatmaps(treated[[2]],treated[[1]],groupss$Grp,workingPath=wd_path,prefix,suffix, mypal = testos,
                        showcol = F, showrow = T,genename=pval$GeneName)

cutHeatmaps(hmp01_All)



#hmp01_All= plotHeatmaps(treated[[2]],treated[[1]],test$Grp,workingPath=wd_path,prefix,suffix,k=3) ## how it should be on shiny app
## Firt remove the columns that does not correspond to the selected columns
## Then do the same but this times for the rows that are not equal to the right columns supress up there. !!!!!


######################
## function          #
######################




##################################################### bad function
# signpval <- function (ngenes,data,pval){
# 
# rpval <- list()
# index <- list()
# for(i in 1:ngenes){
#   if(data[i] < 0.05){
#     
#     index[i] <- i   
#     }
#   }
#   index <- as.numeric(unlist(index))
#   
#   
#   })
#   return(X)
# }

# attestation <- signpval(ngenes,data,pval)
# print(attestation)



grp1 = adj[,c('adj.P.Val_LWT_MCD.LWT_CTRL' ,'adj.P.Val_LKO_MCD.LKO_CTRL')]
adj[,c('adj.P.Val_LKO_CTRL.LWT_CTRL','adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.')]
View(adj)
colnames(adj)

cutoff = 0.9
testFunc = function(x){x<cutoff}
adj$adj.P.Val_LKO_CTRL.LWT_CTRL
grp1 = adj[,c('adj.P.Val_LKO_CTRL.LWT_CTRL')] %>%
  apply(1,FUN = function(x){return(x < pval)}) %>%
  data.frame()


grp1 = adj[,c('adj.P.Val_LKO_CTRL.LWT_CTRL')]


grp1 = adj[,c('adj.P.Val_LKO_CTRL.LWT_CTRL')]


toast = lapply(adj, function(y) testFunc(y[('adj.P.Val_LKO_CTRL.LWT_CTRL')]))
colnames(adj)

View(toast)

pv = 0.05



evaluatesign = function(adj,elem,pv){

  grp1 = adj[,c(elem)] %>%
  sapply( FUN = function(x){return(x < pv)}) %>%
  data.frame() %>%
  filter(. == T) %>%
  nrow()
  
  return(grp1)
}

View(pval)
adj = pval[,grep("X|^adj.P.Val", names(pval), value=TRUE)]

View(adj)

evaluatesign(adj,'adj.P.Val_LKO_CTRL.LWT_CTRL')

pvalue = c(0.01,0.05)

#benchmark(


dtsign = data.frame(matrix(ncol=2,nrow = length(adj[,-1])))
y <- c("pvalue(0.01)","pvalue(0.05)")
colnames(dtsign) <- y

const <- (length(colnames(adj[,-1]))+1)
print(const)
i <- 1
for (pv in pvalue){
  for(elem in colnames(adj[,-1])){
    
    if(i %% const == 0 ){
      i = 1
    }
    if(pv == 0.05)
    {
      print(i)
      print( evaluatesign(adj,elem,pv))
      print("ok")
      dtsign$`pvalue(0.05)`[i] = evaluatesign(adj,elem,pv)
      i = i+1
    }
    else{
      print(i)
      print("ko")
      dtsign$`pvalue(0.01)`[i] = evaluatesign(adj,elem,pv)
       print(evaluatesign(adj,elem,pv))
       i = i+1
    }
  }
}
View(dtsign)

library(data.table)
test = fread("data/All_topTableAll.csv") ## faster ????
View(test)
View(pval)
createdfsign = function(adj) {
  
  dtsign = data.frame(matrix(ncol = 2, nrow = length(adj[, -1])))
  y <- c("pvalue(0.01)", "pvalue(0.05)")
  colnames(dtsign) <- y
  rownames(dtsign) <- colnames(adj[, -1])
  pvalue = c(0.01, 0.05)
  i <- 1
  for (pv in pvalue) {
    for (elem in colnames(adj[, -1])) {
      if (i %% const == 0) {
        i = 1
      }
      if (pv == 0.05)
      {
        dtsign$`pvalue(0.05)`[i] = evaluatesignpar(adj, elem, pv)
        i = i + 1
      }
      else{
        dtsign$`pvalue(0.01)`[i] = evaluatesignpar(adj, elem, pv)
        i = i + 1
      }
    }
  }
  return(dtsign)
}
benchmark(
toasted = createdfsign(adj)
)

print(toasted)

benchmark(
cbind.data.frame("FDR<1%"=colSums(pval[,26:30]<ptv[1]),"FDR<5%"=colSums(pval[,26:30]<ptv[2]))
)


adj = pval[,grep("X|^adj.P.Val", names(pval), value=TRUE)]
logfc = pval[,grep("X|^logFC", names(pval), value=TRUE)]
logfc = apply(logfc[,-1],2, FUN= function(x) return(2**abs(x)))

dplyr::select(pval, grep('X',names(pval)))


View(adj)
adjusted = formating(adj,0.05)
typeof(adjusted)
View(adjusted)
View(pval)
print(as.list(adjusted))



View(pval)
adjusteeed = formatingbis(adj,0.05,pval)
myfc = c(1.2,2,4,6,10)
test = pval %>%
  select(grep('^logFC',names(pval))) %>%
  apply(2,FUN= function(x) return(2**abs(x))) %>%
  as.data.frame()%>%
  cbind("X"= pval[,1],.)


adju <- as.data.frame(adjusteeed)


newdf <- test[test$X %in% row.names(adju),]
View(newdf)

j=1
for (fc in myfc){
  print(colSums((newdf[,-1]>fc)))
  #test[j]= cbind.data.frame(colSums(newdf[,-1]>fc))
  j= j+1
}

cbind.data.frame(colSums(adj[, -1] < pval & 2 ** abs(logfc[, -1]) > fc))


pval <- read.csv2("data/All_topTableAll.csv")

itsok = myfinalfc(pval,0.05)
View(itsok)

View(test)
View(pval)

cbind.data.frame(colSums(adj[,-1]<0.05 &  2**abs(logfc[,-1])>1.2))


apply(test,2 , FUN=function(x) return(names(x)))

isis = colnames(test)
lapply(isis,function(x){
  return(test[x])
})
View(test)
colnames(test)

print(tt)

View(titst)
nrow(titst)
toust = colSums(test<1| test>10)
print(toust)

pval <- read.csv2("data/All_topTableAll.csv")


ptv=c(.01,.05)
toast = cbind.data.frame("FDR<1%"=colSums(adj[,-1]<ptv[1]),"FDR<5%"=colSums(adj[,-1]<ptv[2]))
print(toast)

help("colSums")

3.898/0.208



View(toasted)

sapply(adj[,-1] ,FUN = function(x){return(x)})


library(doParallel)

significant = grp1 %>%
  filter(. == T)

View(significant)
cat("nous obtenons",nrow(significant),"gènes significatifs contre",nrow(pval))

View(significant)
View(musmuscu)

group = significant %>% 
  merge(musmuscu, by=c("X")) %>%
  filter(. == T) %>%
  select(mycols) # trouver la méthode pour chercher avec un chiffre 
  

group = dplyr::as_tibble(group)
group = data.matrix(group)


########################
# TEST
########################

filenames <- list.files(path=getwd(),
                        pattern=".*csv")


##Create list of data frame names without the ".csv" part 
names <-substr(filenames,1,3)

myList <- lapply(list, read.csv2)
View(myList[[1]])
View(myList)
###Load all files

View(pval)
myval = decTestTRiX(pval)

colnames(logfc)
adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|^adj.P.Val_LKO_CTRL.LWT_CTRL", names(pval), value=TRUE)]
logfc = pval[,grep("X|^", names(pval), value=TRUE)]
View(logfc)
finaled = cbind.data.frame(adj[,-1] < 0.05 & 2 ** abs(logfc[,-1]) > 1)


library(dplyr)
logfc = pval[,grep("X|^logFC_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|^logFC_LKO_CTRL.LWT_CTRL", names(pval), value=TRUE)]
logfc = apply(logfc,2, FUN = function(x){return(2 ** abs(x))})
logfc = apply(logfc,2,FUN = function(x){return(x >= 1)})
logfc = apply(logfc,1,sum) 
test = as.data.frame(which(logfc>0))

colnames(test)= '.'



final = decTestTRiX(adj[,-1],logfc[,-1], DEGcutoff = 0.05, FC=1)



passingval = adj %>%
  apply(2,FUN = function(x){return(x < 0.05)}) %>%
  apply(1,sum)

finish = which(passingval>0)
View(finish)


library(dplyr)


adj = pval[,grep("X|^adj.P.Val", names(pval), value=TRUE)]
passingval = adj %>%
  apply(2,FUN = function(x){return(x < 0.05)}) %>%
  apply(1,sum)

passingval = which(passingval > 0)


View(logfc)
logfc = pval[,grep("X|^logFC", names(pval), value=TRUE)]
logfc = logfc[,-1] %>%
  apply(2, FUN = function(x){return(2 ** abs(x))}) %>%
  apply(2,FUN = function(x){return(x > 1)}) %>%
  apply(1,sum)

logfc = which(logfc >0)
nrow(adj)
View(logfc)
View(adj)
final = decTestTRiX(adj,logfc, DEGcutoff = 0.05, FC=4)
length(final)

contrast = 1:ncol(adj)
print(contrast)
pList = adj[, contrast]
View(pList)

DEp = pList <= 0.05
View(DEp)

DEFC = 2 ^ abs(logfc[, contrast]) >= 1
View(DEFC)

toast = as.data.frame(which(passingval >0))

colnames(toast) ="."

final = dplyr::inner_join(test,toast,by=".")


final = subset(test %in% toast)

passingval = adj %>%
  apply(2,FUN = function(x){return(x < pval)}) %>%
  apply(1,sum)



ptv=c(1.2,10)
ptv=c(0.01,0.05)
cbind.data.frame("logfc>1.2"=colSums(logfc>ptv[1]),"logfc>10"=colSums(logfc>ptv[2]))

cbind.data.frame("adj"=colSums(adj<ptv[1]),"adj"=colSums(adj<ptv[2]))

logfc = apply(logfc,2,FUN = function(x){return(x > 10)})
logfc= as.data.frame(logfc)

View(pval)


passingval = which( passingval > 0) 


passingfc = logfc[,-1] %>%
  apply(2, FUN = function(x){return(2 ** abs(x))}) %>%
  apply(2,FUN = function(x){return(x > 10)}) %>%
  apply(1,sum) 

passingfc = which( passingfc > 0) 
length(passingfc)


formating = function( adj,logfc, pval,fc){
  
  
  passingval = adj %>%
    apply(2,FUN = function(x){return(x < pval)}) %>%
    apply(1,sum) 
  
  passingval = which( passingval > 0) 
  
  View(passingval)
  View(logfc)
  
  logfc = logfc[,-1] %>%
    mutate( 2 ** abs(logfc[,-1]))%>%
    View()
  
  
  View(logfc)
  
  passingfc = logfc %>%
    apply(2, FUN = function(x){return(2 ** abs(x))})
    apply(2,FUN = function(x){return(x > fc)}) %>%
    apply(1,sum) 
  
  View(passingfc)
  
  passingfc = which( passingfc > 0) 
  
  View(passingfc)
  
  final =dplyr::inner_join(passingfc,passingval, by =".")

  
  cat("Il y a",length(passingval),"gène significatifs")
  
  return(passingval)
  
}

formating(adj,logfc,0.05,1000)


colnames(pval)


musmuscu = read.csv2("data/TOXA_HEGU_MA0191 _AllChip_WorkingSet.csv")
groups = read.csv2("data/TOXA_HEGU_MA0191 _AllChip_pData.csv")
View(groups$Grp)
unique(groups$Grp)


View(musmuscu)
library(FactoMineR)
library(factoextra)
musmuscu <- musmuscu[,-1]
musmuscu <- scale(musmuscu)
View(musmuscu)
x11()
par(mfrow = c(1,2))
my_res = PCA(musmuscu, graph = T)
x11()
plot(my_res)
biocLite("impute")
library(impute)
library(factoextra)

myt = transpose(musmuscu)
row.names(myt)

help(PCA)
PCAres=PCA(t(musmuscu[,-1]),scale.unit=F,graph=F)
PCAres

mypca$call

View(musmuscu)
mypca = res.pca(musmuscu)
myplot = PCAplot(mypca,rep = F)

myl = list(colnames(musmuscu))


p <- fviz_pca_ind(mypca, label= "all", habillage = groups$Grp, addEllipses=TRUE, ellipse.level=0.8, repel = T, axes = c(1, 2))
  p + scale_color_brewer(palette="Dark2")
  p + theme_minimal()
  p + labs(title = "Variances - PCA")

print(p)

help(fviz_pca)

    colgrp=NULL;
    hab="ind";
    colhab=NULL

print(PCAres)
x11()
PCAres
names.arg=1:nrow(PCA$eig)
PCAres$var$cos2



x11()
test = barplot(names.arg=1:nrow(PCAres$eig),PCAres$eig[,2],main="ACP sur les individus",ylab="% variance", xlab="axes")

plot(PCAres,title="ACP sur les individus",new.plot=F,habillage=hab,col.hab=colhab)

p <- fviz_eig(PCAres, addlabels=TRUE, hjust = -0.3, barfill="white", barcolor ="darkblue", linecolor ="red")
      +theme_minimal()
      + labs(title = "Variances - PCA", x = "Principal Components", y = "% of variances")


print(eboulis(mypca))


plot(PCAres,title="ACP sur les individus",new.plot=F,habillage=hab,col.hab=colhab)


plotIndiv(PCAres,comp=1:2,ind.names=T,group=Y, style="graphics", legend=T, scale=F)


PCA = prcomp(musmuscu[,-1],scale = T)
x11()
biplot(PCA,scale=0)
       mtext("Biplot", line = 3, col = "black", font = 2, cex = 1.2)

