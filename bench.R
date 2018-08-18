library(data.table)
# Demo speedup
n=5e5
DT = data.table( a=sample(1:1000,n,replace=TRUE),
                 b=sample(1:1000,n,replace=TRUE),
                 c=rnorm(n),
                 d=sample(c("foo","bar","baz","qux","quux"),n,replace=TRUE),
                 e=rnorm(n),
                 f=sample(1:1000,n,replace=TRUE) )
DT[2,b:=NA_integer_]
DT[4,c:=NA_real_]
DT[3,d:=NA_character_]
DT[5,d:=""]
DT[2,e:=+Inf]
DT[3,e:=-Inf]


write.table(DT,"test500000.csv",sep=",",row.names=FALSE,quote=FALSE)



library(data.table)
library(rbenchmark)
library(readr)
library(dplyr)
library(ggplot2)
library(reshape)
csvlist = c("test10000.csv","test100000.csv","test500000.csv","test1000000.csv")


freadfiles= lapply(csvlist,FUN = function(x)return(benchmark(fread(x)))) 
freadtot = lapply(seq(length(freadfiles)),FUN = function(x)return(freadfiles[[x]]$elapsed/freadfiles[[x]]$replications))
freadtot

df <- as.data.frame(unlist(freadtot)) %>% setNames(., "times")
df$packages <- ( "fread")
df$csv <- (gsub("./data/",x= unlist(csvlist), replacement = ""))


system.time(read.csv("test500000.csv"))


csvfiles=lapply(csvlist,FUN = function(x)return(benchmark(read.csv(x))))
csvtot = lapply(seq(length(csvfiles)),FUN = function(x)return(csvfiles[[x]]$elapsed/csvfiles[[x]]$replications))
csvtot

df1 <- as.data.frame(unlist(csvtot)) %>% setNames(., "times")
df1$packages <- ( "base")
df1$csv <- (gsub("./data/",x= unlist(csvlist), replacement = ""))


readrfiles =lapply(csvlist,FUN = function(x)return(benchmark(read_csv2(x))))
readrtot =lapply(seq(length(readrfiles)),FUN = function(x)return(readrfiles[[x]]$elapsed/readrfiles[[x]]$replications))
readrtot

df2 <- as.data.frame(unlist(readrtot)) %>% setNames(., "times")
df2$packages <- ( "readr")
df2$csv <- (gsub("./data/",x= unlist(csvlist), replacement = ""))

benchdf <- read.csv2("bench.csv")
levels(benchdf)
benchdf$fread <- unlist(freadtot)
benchdf$base <- unlist(csvtot)
benchdf$readr <- unlist(readrtot)
#benchdf <- bind_rows(df,df1,df2)


mdf <- melt(benchdf, id="Size")  # convert to long format
View(mdf)
mdf$`execution time (ms)` = mdf$`execution time (ms)`*1000
colnames(mdf)[1] <- "csv size (lines)"
colnames(mdf)[3] <- "execution time (ms)"
colnames(mdf)

ggplot(mdf, aes(x=`csv size (lines)`, y=`execution time (ms)`, colour=variable))  +
  geom_line() + 
  theme_bw() +
  geom_point(colour= "black", size=2)

