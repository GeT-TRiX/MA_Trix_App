library(ggplot2)
library(reshape2)
library(plotly)
library(dplyr)

dfinter = read.csv2(file = "venns-filtered.csv")[, -1]
dfinter = read.csv2(file = "venncsv.csv")[, -1]
colnames(dfinter)
mycont = c("logFC_LWT_MCD.LWT_CTRL", "logFC_LKO_MCD.LKO_CTRL")
View(dfinter)


topngenes <- function(dfinter, mycont, inputtop) {
  dfinter$GeneName = make.names(dfinter$GeneName, unique = T)
  
  reshp <-
    melt(
      dfinter[1:inputtop, ],
      id.vars = "GeneName",
      measure.vars = c (mycont),
      variable.name = "Source",
      value.name = "logFC"
    )
  reshp <- droplevels(reshp)
  reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))
  
  
  
  p <- ggplot(reshp, aes(
    x = GeneName,
    y = as.numeric(as.character(formatC(as.double(logFC), digits = 1, format = "f"))),
    fill = factor(Source)
  )) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_fill_discrete(
      name = "GeneName",
      breaks = c(1, 2),
      labels = c("logFC_LWT_MCD.LWT_CTRL", "logFC_LKO_CTRL.LWT_CTRL")
    ) +
    scale_fill_manual(values = c("red","blue")) + 
  

    xlab("Gene Name") + ylab("Log Fold-Change") +
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
      axis.text.y = element_text(size = 8, colour = "#888888")
    )
  
  
  print(p)
  
}


topngenes(dfinter, mycont, 60)




dfinter$GeneName = make.names(dfinter$GeneName, unique = T)

reshp <-
  melt(
    dfinter[1:60, ],
    id.vars = "GeneName",
    measure.vars = c (mycont),
    variable.name = "Source",
    value.name = "logFC"
  )
reshp <- droplevels(reshp)
reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))

class(reshp$GeneName)
(formatC(reshp$logFC, digits = 5))

as.numeric(as.character(formatC(as.double(reshp$logFC), digits = 1, format = "f")))




















