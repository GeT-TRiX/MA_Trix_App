#####################
## Packages imported
#####################

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
colnames(musmuscu)[2:length(musmuscu)] = "test"
names(musmuscu) = gsub(pattern = "^", replacement = "", x = names(your_data))


View(musmuscu)
data  = subset(musmuscu ,is.na ,select = "LWT_Ctrl2")


names(pval) = sapply(strsplit(names(pval), "^adj.P.Val*|^adj.P.Val*"), `[[`, 1)
pval <- read.csv2("data/All_topTableAll.csv")
groups <- read.csv2("data/TOXA_HEGU_MA0191 _AllChip_pData.csv", sep= ";" , dec = ",",header= T)


palette(c("#000000", "#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442", "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray", "burlywood1","darkkhaki", "#CC0000" ));






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
source("compat.R")
source("formating.R")

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


treated = formating(adj,musmuscu,pval= 0.05)

View(treated[[1]])

#####################
## Heatmap
#####################

#### method de corr = 1 - corr afin que des valeurs négatives ne soit pas autant corréler que pour des valeures proches de 1
#### 1- (-1) --> 2
#### 1-0.999 

source("compat.R")
source("global.R")
x11()
hmp01_All= plotHeatmaps(treated[[2]],treated[[1]],groupss$Grp,workingPath=wd_path,prefix,suffix,k=3)


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
    
    if(i %% const ==0 ){
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


i <- 1
for (pv in pvalue){
  for(elem in colnames(adj[,-1])){
    if(i %% const == 0 ){
      i = 1
    }
    if(pv == 0.05)
    {
      dtsign$`pvalue(0.05)`[i] = evaluatesignpar(adj,elem,pv)
      i = i+1
    }
    else{
      dtsign$`pvalue(0.01)`[i] = evaluatesignpar(adj,elem,pv)
      i = i+1
    }
  }
  View(dtsign)
}



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
