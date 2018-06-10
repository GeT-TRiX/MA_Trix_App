library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)

dfinter = read.csv2(file = "venns-filtered.csv")[, -1]
dfinter = read.csv2(file = "venncsv.csv")[, -1]
dfinter = read.csv2(file = "threevenn.csv")[, -1]

df  =lapply(dfinter,as.character)
data.frame(lapply(dfinter,as.numeric))
sapply(dfinter,as.integer)
dat <- as.data.frame(sapply(dfinter, as.numeric)stringsAsFactors=FALSE)
View(dat)


colnames(mycont)
colnames(dfinter) = gsub("\\."," vs " ,dfinter)
colnames(dfinter)
colnames(dfinter)= lapply(colnames(dfinter),function(x)
{
  if(grep(".",x))
    x = gsub("\\."," vs " ,x)

  return(x)})
print(colnames(dfinter))


test = c("logFC_LWT_MCD-LWT_CTRL" , "logFC_LKO_MCD-LKO_CTRL")
length(test)
seq(test)

test = c("ProbeName","GeneName","logFC_LWT_MCD-LWT_CTRL","logFC_LKO_MCD-LKO_CTRL"
)

colnames(dfinter)= lapply(colnames(dfinter),function(x)
  return(if(grep(".",x))
    x = gsub("\\."," vs " ,x)))
print(colnames(dfinter))


typeof(colnames(dfinter))
class(colnames(dfinter))
typeof(unlist(test))
class(unlist(test))
test = unlist(test)

print(test)
grepl("-",test)

colnames(dfinter) = gsub("-", " vs ", test)
print(colnames(dfinter))

test= lapply(test,function(x){
  if(grepl("-",x))
    x = gsub("-"," vs " ,x)
  return(x)})


test = gsub("-", " vs ",test)
colnames(dfinter)

test



colnames(dfinter) = gsub("\\."," vs " ,dfinter)


mycont = colnames(dfinter[3:5])
print(mycont)


topngenes <- function(dfinter, mycont, inputtop, meandup) {
  
  
  if(!meandup)
    dfinter$GeneName = make.names(dfinter$GeneName, unique = T)
  else
    
  
  
  mycont = gsub("\\."," vs " ,mycont)
  
  colnames(dfinter)= lapply(colnames(dfinter),function(x)
    return(if(grep(".",x))
      x = gsub("\\."," vs " ,x)))

  
  reshp <-melt(
      dfinter[1:inputtop, ],
      id.vars = "GeneName",
      measure.vars = c (mycont),
      variable.name = "Comparisons",
      value.name = "logFC"
    )
  reshp <- droplevels(reshp)
  
  maxval = as.numeric(max(reshp$logFC))
  minval = as.numeric(min(reshp$logFC))
  print(maxval)
  print(minval)
  
  
  reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))
  
  p <- ggplot(reshp, aes(
    x = GeneName,
    y = as.numeric(as.character(formatC(as.double(logFC), digits = 1, format = "f"))),
    fill = Comparisons
  )) +
    geom_bar(stat = "identity", position = "dodge") +
    
    
    #scale_y_continuous(limits=c(-15,0))+
    
    # scale_fill_discrete(
    #   name = "Comparisons",
    #   breaks = c(seq(mycont)),
    #   labels = c(mycont) )+

     
    scale_fill_manual(values = c("red","blue",'purple',"green","black")) + 
  

    xlab("Gene Name") + ylab("Log Fold-Change") +
    #theme_classic() + 
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "white"),
      plot.title = element_text(size = 20, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0.5),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10) ,
      axis.text.x = element_text(
        size = 8,
        colour = "#888888",
        angle = 80,
        hjust = 1
      ),
      axis.text.y = element_text(size = 8, colour = "#888888"),
      legend.position="top"
    ) 
  
  
  print(p+ scale_y_continuous(limits = c(-2, 0)))
  print(as.numeric(maxval)-1)
  print(as.numeric(minval)+1)
  
  return(p+ scale_y_continuous(breaks = c(seq(maxval),seq(minval))))

  }
library(data.table)

topngenes(dfinter, mycont, 60)
View(dfinter)
colnames(dfinter)



is.integer(X$logFC_LKO_CTRL.LWT_CTRL)
class(X$logFC_LKO_CTRL.LWT_CTRL)
test=c(1,2,3)

as.numeric(as.character(X$logFC_LWT_MCD.LWT_CTRL))
View(dfinter)

is.numeric(X$logFC_LKO_MCD.LKO_CTRL)
numeric_cols <- which(sapply(X, is.numeric))
print(which(sapply(X, is.numeric)))

DT[, lapply(.SD, sum), by = x, .SDcols = numeric_cols]
as.numeric(levels(X$logFC_LKO_CTRL.LWT_CTRL))[X$logFC_LKO_CTRL.LWT_CTRL]
as.numeric(levels(dfinter[[c(3:5)]]))[dfinter[[c(3:5)]]] 
dfinter[[3]]

test <- grepl("logFC_", colnames(dfinter))
which(test == T)


test <- grepl("logFC_", colnames(dfinter)) %>%
  which(. == T)

test <- "logFC_" %>%
  grepl(colnames(dfinter))%>%
  which(.==T)
test  
for ( i in test)
  print(i)

for (i in test)  
  dfinter[[i]] = as.numeric(levels(dfinter[[i]]))[dfinter[[i]]]

class(dfinter$logFC_LKO_MCD.LKO_CTRL)
View(dfinter$logFC_LWT_MCD.LWT_CTRL)

keys <- colnames(dfinter)[!grepl('logFC_|ProbeName',colnames(dfinter))]
print(keys)

X <- as.data.table(dfinter)
typeof(X$logFC_LWT_MCD.LWT_CTRL)
colnames(X)


test <- "logFC_" %>%
  grepl(colnames(dfinter))%>%
  which(.==T)

for (i in test)  
  dfinter[[i]] = as.numeric(levels(dfinter[[i]]))[dfinter[[i]]]
X <- as.data.table(dfinter)
dfinter <- dfinter[,-1] %>% as.data.table() %>% .[,lapply(.SD,mean),"GeneName"]
class(dfinter)
reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))


View(test)
length(test$mm)
length(dfinter$logFC_LWT_MCD.LWT_CTRL)
View(dfinter)

X[,lapply(.SD,mean),keys]


dfinter <- as.data.table(dfinter)
dfinter[,-1][,lapply(.SD,mean),"GeneName"])


test <- "logFC_" %>%
  grepl(colnames(dfinter))%>%
  which(.==T)

dfinter <- dfinter[,-1] %>% as.data.table() %>% .[,lapply(.SD,mean),"GeneName"]
View(dfinter)

mycont = gsub("."," vs " ,mycont)
colnames(dfinter)
mycont

colnames(dfinter)= lapply(colnames(dfinter),function(x){
  print(x)  
  if(grepl(".",x))
    x = gsub("."," vs " ,x)
  
  return(x)})

colnames(dfin)
reshp <-melt(
  dfinter[1:inputtop, ],
  id.vars = "GeneName",
  measure.vars = c (mycont),
  variable.name = "Comparisons",
  value.name = "logFC"
)
reshp <- droplevels(reshp)

dfinter = read.csv2(file = "threevenn.csv")[, -1]
adj = pval[,grep("X|^adj.P.Val_.LWT_MCD.LWT_CTRL...LKO_MCD.LKO_CTRL.|^adj.P.Val_LKO_CTRL.LWT_CTRL", names(pval), value=TRUE)]
print(class(adj$X))
levels(dfinter[[3]])
cont = colnames(dfinter[,-1])
cont

colnames(dfinter)= lapply(colnames(dfinter),function(x)
{
  if(grep(".",x))
    x = gsub("\\."," vs " ,x)
  
  return(x)})
print(colnames(dfinter))
View(dfinter)
colnames(dfinter) = cont
dfinter= as.data.frame(dfinter)

test <- "logFC_" %>%
  grepl(colnames(dfinter))%>%
  which(.==T)

for (i in test)  
  dfinter[[i]] = as.numeric(as.character(dfinter[[i]]))[dfinter[[i]]]
View(dfinter)
typeof(dfinter$logFC_LWT_MCD.LWT_CTRL)
class(dfinter$logFC_LWT_MCD.LWT_CTRL)
X <- as.data.table(dfinter)
dfinter <- dfinter[,-1] %>% as.data.table() %>% .[,lapply(.SD,mean),"GeneName"]
class(dfinter)
reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))
dfinter= as.data.frame(dfinter)

#####################################

dfinter = read.csv2(file = "threevenn.csv")[, -1]

print(class(dfinter[[2]]))

test <- "logFC_" %>%
  grepl(colnames(dfinter))%>%
  which(.==T)
print(class(dfin))
as.numeric(as.character(dfinter[[3]]))
dfinter = as.data.table(dfinter)
for (i in test)  {
  prin(i)
  dfinter[[i]] = as.numeric(as.character(dfinter[[i]]))[dfinter[[i]]]
  print(dfinter[[i]]) 
}
#dfinter <- dfinter[,-1] %>% as.data.table() %>% .[,lapply(.SD,mean),"GeneName"]
dfinter <- dfinter[,-1] %>% .[,lapply(.SD,mean),"GeneName"]
#reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))
dfinter= as.data.frame(dfinter)



mycont = colnames(dfinter[3:5])
print(mycont)

mycont = gsub("\\."," vs " ,colnames(dfinter[3:5]))


colnames(dfinter)= lapply(colnames(dfinter),function(x){
  
  if(grepl(".",x))
    x = gsub("\\."," vs " ,x)
  
  return(x)})


reshp <-melt(
  dfinter[1:50, ],
  id.vars = "GeneName",
  measure.vars = c (mycont),
  variable.name = "Comparisons",
  value.name = "logFC"
)

reshp <- droplevels(reshp)
reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))

maxval = as.numeric(max(reshp$logFC))
minval = as.numeric(min(reshp$logFC))
print(c(minval-2,maxval+2))


p <- ggplot(reshp, aes(
  x = GeneName,
  y = as.numeric(as.character(formatC(as.double(logFC), digits = 1, format = "f"))),
  fill = Comparisons
)) +
  geom_bar(stat = "identity", position = "dodge") +
  
  # coord_cartesian(ylim=c(minval-2,maxval+2))+
  # scale_fill_discrete(
  #   name = "GeneName",
  #   breaks = c(seq(mycont)),
  #   labels = c(mycont) )+
  
  scale_fill_manual(values = c("red","blue",'purple',"green","black")) + 
  
  xlab("Gene Names") + ylab("Log Fold-Change") +
  #theme_classic() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "white"),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.caption = element_text(size = 10, hjust = 0.5),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10) ,
    axis.text.x = element_text(
      size = 8,
      colour = "#000000",
      angle = 80,
      hjust = 1
    ),
    axis.text.y = element_text(size = 8, colour = "#000000"),
    legend.position="top"
  ) 

print(unique(sort(c(seq(as.numeric(maxval)),seq(as.numeric(minval))))))

print(p)

return( p)



