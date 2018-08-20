source("./function/vennplot.R")
library(dplyr)
colnames(pval)

pval <- read.csv2("data/All_topTableAll.csv")

filter(pval, ProbeName %in% "A_55_P1964332") %>% 
  select(logFC_LKO_CTRL.LWT_CTRL)



adj = pval[,grep("^adj.P.Val", names(pval), value=TRUE)]
fc = pval[,grep("^logFC", names(pval), value=TRUE)]
mylog = (fc[1:2])
print(myprob)
colnames(pval)
for (i in 1:ncol(adj)){
  print(i)
}

length(pval$ProbeName)
length(unique(pval$GeneName))

lapply(seq(ncol(adj)),function(x)
  return(x))


pval %>% filter(ProbeName %in% myprob) %>% select(ProbeName,GeneName, colnames(mylog))

test %>% mutate_if(is.numeric, funs(formatC(., format = "f")))

help(formatC)


View(fc)
row.names(adj) = pval$ProbeName
row.names(fc) = pval$ProbeName

colnames(pval[26:30])
sort((colnames(pval[26:30])))


xtest = sort(colnames(pval[26:29]))
xord = colnames(pval[26:30])
myord <- factor(xtest, levels = xord)
sort(myord)
paste(myord, collapse="")
test = c(0.005,2,3,4)

formatC(test, format  = "E")


View(pval[seq_along(NROW(pval))])
pval[seq(NCOL(pval)-1)]

rownames(pval)
library(dplyr)
library(gridExtra)

myven = Vennlist( adj, fc,"both",0.05,2)
myven <- myven[sapply(myven, sum) > 0]
sum(sapply(myven,length))
print(myven)
myvent = myven[1:2]
names(adj)
indexnull = which( sapply(myven ,length) == 0)
myven[-3]
print(indexnull)
test = gsub("^\\s+|\\s+$", "", unlist(strsplit(test, ",")))
myvenn
myven <- myven[sapply(myven, sum) > 0]
myven[lengths(myven) > 0L]
myven[sapply(myven)>0]
sum(sapply(myven,length))
adj[,-indexnull]
myven
myven = Vennlist( adj, fc,"both",0.05,1)
indexnull = which( sapply(myven ,length) == 0)
test <- adj[,-c(indexnull)]
names(test)
View(adj[,-indexnull])
indexnull
adj = pval[,grep("^adj.P.Val", names(pval), value=TRUE)]

myven <- myven[sapply(myven, length) > 0]
colnames(adj[,-c(indexnull)])
View(adj[,c(-3)])
colnames(adj)
colnames(adj[,-c(-3)])
myven = Vennlist( adj, fc,"both",0.01,10)
myven
source("./function/vennplot.R")
myven

myl = lapply(seq(length(myven)), function(x){pval %>% select(GeneName, ProbeName) %>% filter( ProbeName %in% myven[[x]]) %>% 
    distinct( GeneName)}) %>%
  as.matrix()
View(myl)
myl = lapply(1:length(myven),FUN = function(i) as.character(myven[[i]]$GeneName))  #


pval
Vennfinal(myven,adj, cex=1, 0.05, 1, statimet="FDR",pval,meandup = "genes")

snames(myven)
pval$rownames =rownames(pval)


names(myven) = colnames(adj)


final = lapply(
  names(myven),
  FUN = function(x) {
    test = pval[pval$rownames %in% myven[[x]], ]
    
    what <- test %>%
      select(ProbeName) %>%
      unlist() %>%
      as.character() 
  
    return(what)
  }
)

print(final)
names(final) = colnames(adj)
cl <- makeCluster(6)
stop(cl)
print(cl)
benchmark(setvglobalvenn(final,adj))

stopCluster(cl)

vennresbef = setvglobalvenn(final,adj)
print(vennresbef)

View(vennresbef)

testaxx = colnames(pval)[26:30]
print(testaxx)
testaxx= testaxx[-3]
print(testaxx)



typeof(testaxx)
class(testaxx)
who = paste(testaxx, collapse ="")

what  = paste0(testaxx[1], testaxx[2], testaxx[3], testaxx[4], sep="")
print(what)
print(who)
vennres[what]
names(vennres)

print(vennres)
View(vennres)


test = setvglobalvenn(final,adj)



names(test[10])
paste0()

max.length <- max(sapply(test, length))
print(max.length)

test = test %>%
  lapply(function(v){ c(v, rep("", max.length-length(v)))}) %>%
  setNames(names(test)) %>%
  as.data.frame()

colnames(test)
View(test)
myinter = paste0(mynames[3], sep="&")
print(myinter)
length(test[[myinter]])

for(i in 1:length(test[[myinter]]))
  print(test[[myinter]][[i]])


combs <-  unlist(lapply(1:length(myven), 
                function(j) combn(names(myven), j, simplify = FALSE)),
         recursive = FALSE)
names(combs) <- sapply(combs, function(i) paste0(i, collapse = ""))
str(combs)

elements <- 
  lapply(combs, function(i) Setdiff(myven[i], myven[setdiff(names(myven), i)]))

print(elements)
View(elements)
n.elements <- sapply(elements, length)
print(n.elements)


# set.seed(1)
# xx.1 <- list(A = sample(LETTERS, 15), 
#              B = sample(LETTERS, 15), 
#              C = sample(LETTERS, 15), 
#              D = sample(LETTERS, 15))
# print(xx.1)
# Intersect(xx.1)
# Setdiff(xx.1[c("C", "D")], xx.1[c("A", "B")])
# 
# print(xx.1)
# print(myven$adj.P.Val_LWT_MCD.LWT_CTRL)
# typeof(myven$adj.P.Val_LWT_MCD.LWT_CTRL)
# print(myven)source("./function/vennplot.R")
library(dplyr)


pval <- read.csv2("data/All_topTableAll.csv")
adj = pval[,grep("^adj.P.Val", names(pval), value=TRUE)]
fc = pval[,grep("^logFC", names(pval), value=TRUE)]
mylog = (fc[1:2])
print(myprob)
colnames(pval)
for (i in 1:ncol(adj)){
  print(i)
}

length(pval$ProbeName)
length(unique(pval$GeneName))

lapply(seq(ncol(adj)),function(x)
  return(x))


pval %>% filter(ProbeName %in% myprob) %>% select(ProbeName,GeneName, colnames(mylog))

test %>% mutate_if(is.numeric, funs(formatC(., format = "f")))

help(formatC)


View(fc)
row.names(adj) = pval$ProbeName
row.names(fc) = pval$ProbeName

colnames(pval[26:30])
sort((colnames(pval[26:30])))


xtest = sort(colnames(pval[26:29]))
xord = colnames(pval[26:30])
myord <- factor(xtest, levels = xord)
sort(myord)
paste(myord, collapse="")
test = c(0.005,2,3,4)

formatC(test, format  = "E")


View(pval[seq_along(NROW(pval))])
pval[seq(NCOL(pval)-1)]

rownames(pval)
library(dplyr)
library(gridExtra)

myven = Vennlist( adj, fc,"both",0.05,2)
myven <- myven[sapply(myven, sum) > 0]
sum(sapply(myven,length))
print(myven)
myvent = myven[1:2]
names(adj)
source("./function/vennplot.R")
Vennfinal(myvent,adj[1:2], cex=1, 0.05, 1, statimet="FDR",pval,meandup = "probes")


pval$rownames =rownames(pval)


names(myven) = colnames(adj)


final = lapply(
  names(myven),
  FUN = function(x) {
    test = pval[pval$rownames %in% myven[[x]], ]
    
    what <- test %>%
      select(ProbeName) %>%
      unlist() %>%
      as.character() 
    
    return(what)
  }
)

print(final)
names(final) = colnames(adj)
cl <- makeCluster(6)
stop(cl)
print(cl)
benchmark(setvglobalvenn(final,adj))

stopCluster(cl)

vennresbef = setvglobalvenn(final,adj)
print(vennresbef)

View(vennresbef)

testaxx = colnames(pval)[26:30]
print(testaxx)
testaxx= testaxx[-3]
print(testaxx)



typeof(testaxx)
class(testaxx)
who = paste(testaxx, collapse ="")

what  = paste0(testaxx[1], testaxx[2], testaxx[3], testaxx[4], sep="")
print(what)
print(who)
vennres[what]
names(vennres)

print(vennres)
View(vennres)


test = setvglobalvenn(final,adj)



names(test[10])
paste0()

max.length <- max(sapply(test, length))
print(max.length)

test = test %>%
  lapply(function(v){ c(v, rep("", max.length-length(v)))}) %>%
  setNames(names(test)) %>%
  as.data.frame()

colnames(test)
View(test)
myinter = paste0(mynames[3], sep="&")
print(myinter)
length(test[[myinter]])

for(i in 1:length(test[[myinter]]))
  print(test[[myinter]][[i]])


combs <-  unlist(lapply(1:length(myven), 
                        function(j) combn(names(myven), j, simplify = FALSE)),
                 recursive = FALSE)
names(combs) <- sapply(combs, function(i) paste0(i, collapse = ""))
str(combs)

elements <- 
  lapply(combs, function(i) Setdiff(myven[i], myven[setdiff(names(myven), i)]))

print(elements)
View(elements)
n.elements <- sapply(elements, length)
print(n.elements)


# set.seed(1)
# xx.1 <- list(A = sample(LETTERS, 15), 
#              B = sample(LETTERS, 15), 
#              C = sample(LETTERS, 15), 
#              D = sample(LETTERS, 15))
# print(xx.1)
# Intersect(xx.1)
# Setdiff(xx.1[c("C", "D")], xx.1[c("A", "B")])
# 
# print(xx.1)
# print(myven$adj.P.Val_LWT_MCD.LWT_CTRL)
# typeof(myven$adj.P.Val_LWT_MCD.LWT_CTRL)
# print(myven)
# class(xx.1)
# typeof(xx.1$A)
# Intersect(xx.1)
# #[1] "E" "L"
# Setdiff(xx.1[c("C", "D")], xx.1[c("A", "B")])
# 
# 
# 
# test = myventocsv(myven,adj[1:5])
# View(test)
# View(myven)
# df <- data.frame(matrix(unlist(myven), ncol = 5))
# colnames(df) = colnames(adj)
# View(df)
# write.csv(myven, file = "MyData.csv")
# 
# max.length <- max(sapply(myven, length))
# l <- lapply(myven, function(v) { c(v, rep("", max.length-length(v)))})
# test = do.call(cbind, l)
# colnames(test) = colnames(adj)
# write.table(test, "cnbd.csv",
#             na = "",
#             row.names = F,
#             col.names = T,
#             append = TRUE,
#             sep = ";")
# 
# myventocsv(myven,adj)
# myven
# adj[1:5]
# 
# 
# Vennfinal(myven,adj[1:5])
# g = venn(myven, ilabels= F, zcolor ="style", sname = colnames(adj), cexil = 0.5, size = 5, cexsn = 0.5)
# final = grid.arrange(gTree(children=g), top="Venn Diagram", bottom="DEG BH 0.05")
# help(venn)
# 
# evenn(data("Data_Lists"), annot = T, display = Yndisplay, ud  =T, Profils = T)
# print(myven[[1]])
# 
# final= list(A=myven[[1]],B=myven[[2]],C=myven[[4]],D= myven[[5]])
# plot(euler(final))
# 
# help(euler)
# combo <- c(A = 2, B = 2, C = 2, "A&B" = 1, "A&C" = 1, "B&C" = 1)
# fit1 <- euler(combo)
# final = euler(list(A = c("a", "ab", "ac", "abc"),
#            B = c("b", "ab", "bc", "abc"),
#            C = c("c", "ac", "bc", "abc")))
# 
# 
# w <- compute.Venn(Venn(final))
# gp <- VennThemes(w)
# plot(w, types= squares)
# 
# test = Vennerable::Venn(myven)
# 
# plot(test)
# 
# 
# plot(final)
# 
# Vennfinal(myven,adj)
# data("Data_Lists")


test = 'red,blue,yellow,green'
gsub("^\\s+|\\s+$", "", unlist(strsplit(test, ",")))

# class(xx.1)
# typeof(xx.1$A)
# Intersect(xx.1)
# #[1] "E" "L"
# Setdiff(xx.1[c("C", "D")], xx.1[c("A", "B")])
# 
# 
# 
# test = myventocsv(myven,adj[1:5])
# View(test)
# View(myven)
# df <- data.frame(matrix(unlist(myven), ncol = 5))
# colnames(df) = colnames(adj)
# View(df)
# write.csv(myven, file = "MyData.csv")
# 
# max.length <- max(sapply(myven, length))
# l <- lapply(myven, function(v) { c(v, rep("", max.length-length(v)))})
# test = do.call(cbind, l)
# colnames(test) = colnames(adj)
# write.table(test, "cnbd.csv",
#             na = "",
#             row.names = F,
#             col.names = T,
#             append = TRUE,
#             sep = ";")
# 
# myventocsv(myven,adj)
# myven
# adj[1:5]
# 
# 
# Vennfinal(myven,adj[1:5])
# g = venn(myven, ilabels= F, zcolor ="style", sname = colnames(adj), cexil = 0.5, size = 5, cexsn = 0.5)
# final = grid.arrange(gTree(children=g), top="Venn Diagram", bottom="DEG BH 0.05")
# help(venn)
# 
# evenn(data("Data_Lists"), annot = T, display = Yndisplay, ud  =T, Profils = T)
# print(myven[[1]])
# 
# final= list(A=myven[[1]],B=myven[[2]],C=myven[[4]],D= myven[[5]])
# plot(euler(final))
# 
# help(euler)
# combo <- c(A = 2, B = 2, C = 2, "A&B" = 1, "A&C" = 1, "B&C" = 1)
# fit1 <- euler(combo)
# final = euler(list(A = c("a", "ab", "ac", "abc"),
#            B = c("b", "ab", "bc", "abc"),
#            C = c("c", "ac", "bc", "abc")))
# 
# 
# w <- compute.Venn(Venn(final))
# gp <- VennThemes(w)
# plot(w, types= squares)
# 
# test = Vennerable::Venn(myven)
# 
# plot(test)
# 
# 
# plot(final)
# 
# Vennfinal(myven,adj)
# data("Data_Lists")


test = 'red,blue,yellow,green'
gsub("^\\s+|\\s+$", "", unlist(strsplit(test, ",")))
