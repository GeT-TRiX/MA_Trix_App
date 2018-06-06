library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)

dfinter = read.csv2(file = "venns-filtered.csv")[, -1]
dfinter = read.csv2(file = "venncsv.csv")[, -1]
dfinter = read.csv2(file = "threevenn.csv")[, -1]

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


topngenes <- function(dfinter, mycont, inputtop) {
  dfinter$GeneName = make.names(dfinter$GeneName, unique = T)
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


topngenes(dfinter, mycont, 60)
seq(-20)
seq(-1)

unique(sort(c(seq(3.1803),seq( -2.3166))))


seq(-5,5,by=1)


seq(-8,-.5+2,by=1)











