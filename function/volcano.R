library(dplyr)
library(plotly)


toptable <- read.csv2("data/All_topTableAll.csv")

test <- toptable$GeneName[2663]
test


View(toptable)
colnames(toptable)
toptable %>%
  dplyr::select(GeneName,adj.P.Val_LWT_CARBO.LWT_CTRL )


test = select(toptable, adj.P.Val_LWT_CARBO.LWT_CTRL ) %>% top_n(.,n=10) %>% select(GeneName)
test$GeneName

head(toptable)
with(toptable, plot(logFC_LKO_CTRL.LWT_CTRL, -log10(adj.P.Val_LKO_CTRL.LWT_CTRL), pch=20, main="Volcano plot", xlim=c(-2.5,2)))
colnames(toptable)

with(subset(toptable, adj.P.Val_LKO_CTRL.LWT_CTRL<.05 ), points(logFC_LKO_CTRL.LWT_CTRL, -log10(adj.P.Val_LKO_CTRL.LWT_CTRL), pch=20, col="red"))
with(subset(toptable, abs(logFC_LKO_CTRL.LWT_CTRL)>1), points(logFC_LKO_CTRL.LWT_CTRL, -log10(adj.P.Val_LKO_CTRL.LWT_CTRL), pch=20, col="orange"))
with(subset(toptable, adj.P.Val_LKO_CTRL.LWT_CTRL<.05 & abs(logFC_LKO_CTRL.LWT_CTRL)>1), points(logFC_LKO_CTRL.LWT_CTRL, -log10(adj.P.Val_LKO_CTRL.LWT_CTRL), pch=20, col="green"))

# Label points with the textxy function from the calibrate plot
library(calibrate)
with(subset(toptable, adj.P.Val_LKO_CTRL.LWT_CTRL<.01 & abs(logFC_LKO_CTRL.LWT_CTRL)>2), textxy(logFC_LKO_CTRL.LWT_CTRL, -log10(adj.P.Val_LKO_CTRL.LWT_CTRL), labs=GeneName, cex=.8))
  dev.off()

  
  
toptable["group"] <- "NotSignificant"
View(toptable)  
toptable[which(toptable['adj.P.Val_LKO_CTRL.LWT_CTRL'] < 0.05 & abs(toptable['logFC_LKO_CTRL.LWT_CTRL']) < 1.5 ),"group"] <- "Significant"
toptable
toptable[which(toptable['adj.P.Val_LKO_CTRL.LWT_CTRL'] > 0.05 & abs(toptable['logFC_LKO_CTRL.LWT_CTRL']) > 1.5 ),"group"] <- "FoldChange"


toptable[which(toptable['adj.P.Val_LKO_CTRL.LWT_CTRL'] < 0.05 & abs(toptable['logFC_LKO_CTRL.LWT_CTRL']) > 1.5 ),"group"] <- "Significant&FoldChange"
top_peaks <- toptable[with(toptable, order("logFC_LKO_CTRL.LWT_CTRL", "adj.P.Val_LKO_CTRL.LWT_CTRL")),][1:5,]
top_peaks <- rbind(top_peaks, toptable[with(toptable, order("logFC_LKO_CTRL.LWT_CTRL", "adj.P.Val_LKO_CTRL.LWT_CTRL")),][1:5,])



a <- list()
for (i in seq_len(nrow(top_peaks))) {
  m <- top_peaks[i, ]
  a[[i]] <- list(
    x = m[["logFC_LKO_CTRL.LWT_CTRL"]],
    y = -log10(m[["adj.P.Val_LKO_CTRL.LWT_CTRL"]]),
    text = m[["GeneName"]],
    xref = "x",
    yref = "y",
    showarrow = TRUE,
    arrowhead = 0.5,
    ax = 20,
    ay = -40
  )
}
a
x = toptable["logFC_LKO_CTRL.LWT_CTRL"]
y = toptable["adj.P.Val_LKO_CTRL.LWT_CTRL"]
gene <- toptable["GeneName"]
p <- plot_ly(data = toptable, x =x , y = y , text = gene, mode = "markers") %>% 
  layout(title ="Volcano Plot") %>%
  layout(annotations = a)
p

colnames(toptable)
?EnhancedVolcano
rownames(toptable) = toptable$ProbeName
View(toptable)
library(dplyr)
toptable <- read.csv2("data/All_topTableAll.csv")
test = "logFC_LWT_CARBO.LWT_CTRL"
as.numeric(as.character(test))
as.numeric(as.character(factor(test)))
noquote(test)
gsub('"', '', test)
as.numeric(gsub("(^')|('$)", "", test))


mutate_values <- function(new_name, name1, name2){
  mtcars %>% 
    mutate(UQ(rlang::sym(new_name)) :=  UQ(rlang::sym(name1)) +  UQ(rlang::sym(name2)))
}

toptable %>% dplyr::select(GeneName,logFC_LWT_CARBO.LWT_CTRL) %>% mutate(logFC_LWT_CARBO.LWT_CTRL= abs(as.numeric(test))) %>%
  top_n(5)
class(toptable$Row)
test$GeneName
library(dplyr)
test <- toptable$GeneName[2663]

EnhancedVolcano(toptable, lab= toptable$GeneName, x ="logFC_LWT_CARBO.LWT_CTRL" , y = "adj.P.Val_LWT_CARBO.LWT_CTRL", 
                pCutoff = 0.05,
                FCcutoff = 3, topgenes = 10,
                transcriptPointSize = 1,
                transcriptLabSize = 3.0,DrawConnectors = T,
                title = "N061011 versus N61311",
                cutoffLineType = "twodash",
                cutoffLineCol = "black",
                cutoffLineWidth = 1,legend=c("NS","Log (base 2) fold-change","P value",
                                             "P value & Log (base 2) fold-change")
                )

