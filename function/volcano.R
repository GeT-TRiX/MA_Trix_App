library(dplyr)
library(plotly)


toptable <- read.csv2("data/All_topTableAll.csv")
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
EnhancedVolcano(toptable, lab= toptable$GeneName, x ="logFC_LWT_CARBO.LWT_CTRL" , y = "adj.P.Val_LWT_CARBO.LWT_CTRL",  pCutoff = 1,
                FCcutoff = 1, topgenes = 5,
                transcriptPointSize = 1,
                transcriptLabSize = 3.0,
                title = "N061011 versus N61311",
                cutoffLineType = "twodash",
                cutoffLineCol = "black",
                cutoffLineWidth = 1,legend=c("NS","Log (base 2) fold-change","P value",
                                             "P value & Log (base 2) fold-change")
                )

