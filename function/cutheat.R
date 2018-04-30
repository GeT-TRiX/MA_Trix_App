#' cutHeatmaps if a function that takes as input an heatmap object and depending on the cut height and the cluster
#' choosen render a ggplot object or an heatmap object
#'
#' @param hmp an heatmap object
#' @param height a numeric value to cut the dendogram
#' @param exprData a data frame with specific columns depending on the user's choices
#' @param DEGres a data frame corresponding to the xxx_topTableAll
#' @param groups a data frame of the choosen groups
#' @param cexcol a positive numbers, used as cex.axis in for the row or column axis labeling
#' @param cexrow a positive numbers, used as cex.axis in for the row or column axis labeling
#' @param labrow a character vectors with row and column labels to use
#' @param fileType a character to select the plot to display heatmap, boxplot or stripchart
#' @param scale a character indicating if the values should be centered and scaled in either the row direction or the column direction, or none
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#' @param col.hm a character vector
#' @param type a character to select the plot to display heatmap, boxplot or stripchart
#' @param las a numeric value
#' @param distfun function used to compute the distance (dissimilarity) between both rows and columns.
#' @param palette.col a character vector 
#' @param num an item of the heatmap object corresponding to a specific cluster choosen by the user 
#' @param ... 
#'
#' @return a ggplot object or heatmapply object
#' 

cutHeatmaps = function(hmp,height,exprData,DEGres,groups,cexcol = 1,cexrow = 1,labrow = T,
                       fileType = "png",scale = "row",meanGrp = F,
                       col.hm = maPalette(low = "green",high = "red",mid = "black",k = 75),
                       type = "None",las = 2,distfun = "cor",palette.col = NULL,num = 4,...)
{
  
  require(ggplot2)
  require(grid)
  require(gridExtra)
  
  plot.boxplot = ifelse(type == "Boxplot", T, F)
  plot.stripchart = ifelse(type == "LB" | type == "WB", T, F)
  hmp.plot = ifelse(type == "Heatmap", T, F)
  probes.boxplot = ifelse(type == "WB", T, F)
  
  
  if (!is.null(palette.col)) {
    cl = palette(palette.col)
    
    #		}else cl=palette(c("black", "blue", "cyan", "magenta",   "darkgray", "darkgoldenrod", "violet",  "orange", "lightgreen","lightblue", "darkorchid", "darkred","darkslateblue", "darkslategray", "maroon", "burlywood1" , "darkolivegreen"));
  } else
    cl =  palette(
      c(
        "#000000",
        "#0072c2",
        "#D55E00",
        "#999999",
        "#56B4E9",
        "#E69F00",
        "#CC79A7",
        "lightblue",
        "#F0E442",
        "lightgreen",
        "deepskyblue4",
        "darkred",
        "#009E73",
        "maroon3",
        "darkslategray",
        "burlywood1",
        "darkkhaki",
        "#CC0000"
      )
    )
  
  
  colid = colnames(exprData)
  
  exprData = exprData[labels(hmp$rowDendrogram), ]
  wdt = 900
  wdte = 12
  exprData0 = exprData
  groups0 = groups
  if (meanGrp) {
    cat("\n -> calculating groups means \n")
    #prefix=paste(prefix,"meanGrp",sep="_")
    exprData = t(apply(
      exprData,
      1,
      FUN = function(x) {
        tapply(x, groups, mean, na.rm = T)
      }
    ))
    groups = factor(levels(groups))
    colid = NA
    wdt = 600
    wdte = 8
  }
  
  if (distfun == "cor") {
    distfunTRIX = distcor
  } else {
    distfunTRIX = function(x, method = distfun) {
      dist(x, method = method)
    }
  }
  
  ###=======================
  ## decoupage de la heatmap
  ###=======================
  # decouper le dendro en 2 parties (upper et lower) a la hauteur desiree
  cut02 = cut(hmp$rowDendrogram, h = height)
  #upper est une version tronquee de l'arbre de depart
  #lower est une liste contenant les X sous-dendrogrammes generes par la coupure
  
  cat("\n -> size of", length(cut02$lower), "sub-dendrograms\n")
  print(sapply(cut02$lower, function(x)
    length(labels(x))))
  #Pour voir quels sont les effectifs de chaque sous-groupe de genes
  
  ###export toptable sous groupes hclust
  cat(" ->export result tables for each subgroup \n")
  gpcol = num2cols(as.numeric(groups))

  
  # idx sondes de chaque cluster
  HCgroupsLab = lapply(cut02$lower, function(x)
    labels(x))
  
  
  # expression de chaque cluster
  HCgroupsLabExrs = lapply(HCgroupsLab, function(x)
    exprData0[x, ])
  
  ## centrage reduction
  HCgroupsLabExrsCenterScale = lapply(HCgroupsLabExrs, function(y) {
    t(scale(t(y), center = T, scale = T))
  })
  
  ## exr moyen par groupe et pour chaque cluster
  grps = groups0
  
  
  HCgroupsLabExrsCenterScaleMean = lapply(HCgroupsLabExrsCenterScale, function(y) {
    t(apply(
      y,
      1,
      FUN = function(x) {
        tapply(x, grps, mean)
      }
    ))
  })
  
  
  if (plot.boxplot & !plot.stripchart) {
    ###=======================
    ## boxplot de la heatmap
    ###=======================
    
    cat(" ->Expression boxplot for each subgroup \n")
    
    library(Hmisc)
    library(reshape2)
    library(plotly)
    library(heatmaply)
    
    myplots <- list()
    
    for (i in 1:length(HCgroupsLabExrsCenterScaleMean)) {
      #print(length(HCgroupsLabExrsCenterScaleMean))
      
      local({
        i <- i
        
        dataCentS = HCgroupsLabExrsCenterScaleMean[[i]]
        isplotable = apply(simplify2array(dataCentS), 1:2, sum, na.rm = TRUE)
        nProbes = nrow(dataCentS)

        ## boxplotggplot2
        footnote <-
          paste("Average expression Z-score over replicates; ",
                nProbes,
                " probes",
                sep = "")
        test <- footnote
        dataCentSm = melt(dataCentS)
        colnames(dataCentSm) = c("Probe", "Group", "Expression")
        #footnote <- paste("Average expression Z-score over replicates; ",nProbes," probes",sep="")
        ggbplot = ggplot(dataCentSm, aes(x = Group, y = Expression)) +
          theme_bw() + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
          ) +
          theme(axis.line = element_line(colour = "#888888")) +
          #geom_violin(aes(fill=Group)) +
          scale_fill_manual(values = cl[(1:length(levels(groups))) + 1]) +
          geom_jitter(
            position = position_jitter(0.16) ,
            colour = "#888888",
            alpha = 0.4,
            shape = 16,
            size = 3
          ) +
          geom_boxplot(width = 0.1, fill = NA) +
          
          labs(title = paste("Cluster", i),
               subtitle = paste(caption = footnote)) +
          theme(
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.caption = element_text(size = 10, hjust = 0.5),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10) ,
            axis.text.x = element_text(
              size = 10,
              colour = "#888888",
              angle = 360,
              hjust = 0.5
            ),
            axis.text.y = element_text(size = 12, colour = "#888888")
          )
        
        if (nProbes > 2)
          myplots[[i]] <<-
          (ggbplot + geom_violin(aes(fill = Group)))
        else
          myplots[[i]] <<- (ggbplot)
        
      })
    }
    return(myplots[[as.numeric(num)]])
  }
  
  #############"
  ### ggplot2
  #############"
  
  if (plot.stripchart) {
    cat(" ->Plotting Expression as stripchart for each subgroup \n")
    #print(head(HCgroupsLabExrsCenterScale))
    
    numProbes = lapply(HCgroupsLabExrsCenterScale, nrow)
    myplotsstrip <- list()
    for (i in 1:length(HCgroupsLabExrsCenterScale)) {
      local({
        i <- i
        
        dataStacked = as.vector(HCgroupsLabExrsCenterScale[[i]])
        
        dataStackeddt = cbind.data.frame(
          Expression = dataStacked,
          factor.trace = rep(grps, each = nrow(HCgroupsLabExrsCenterScale[[i]])),
          probeName = rep(
            rownames(HCgroupsLabExrsCenterScale[[i]]),
            ncol(HCgroupsLabExrsCenterScale[[i]])
          ),
          indName = rep(
            colnames(HCgroupsLabExrsCenterScale[[i]]),
            each = nrow(HCgroupsLabExrsCenterScale[[i]])
          )
        )
        dataStackeddt$Grp = dataStackeddt$factor.trace
        nindiv = table(dataStackeddt$Grp) / numProbes[[i]]
        
        #	 		print(head(dataStackeddt))
        if (all(nindiv == nindiv[1])) {
          nindiv = as.character(nindiv[1])
          
        } else
          nindiv = paste(nindiv, collapse = ", ")
        
        footnote <-
          paste("mean expression Z-score +/- 95%CI; N=",
                nindiv,
                "; ",
                numProbes[[i]],
                " probes",
                sep = "")
        
        if (!probes.boxplot) {
          #	 print(footnote)
          ##=============
          ## plot stripchart
          ##
          
          
          #### jitter classique
          #		d=data.frame(Grp=rep(c('before','after'), 2000), Expression=rexp(4000, 1))
          #				ggstrip= ggplot(d, aes(x=Grp, y = Expression)) +
          #				geom_jitter()
          
          
          ggstrip = ggplot(dataStackeddt, aes(x = Grp, y = Expression)) +
            #				geom_jitter()
            theme_bw() + theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()
            ) +
            theme(axis.line = element_line(colour = "#888888")) +
            geom_violin() +
            #					geom_violin(aes(fill=Grp)) +
            ##					geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) +
            #					geom_sina() +  #  package ggforce
            #					geom_boxplot(width = 0.1) +
            geom_jitter(
              position = position_jitter(0.16) ,
              colour = "#888888",
              alpha = 0.05,
              shape = 16,
              size = 3
            ) +
            #					geom_sina(aes(color=Grp)) +
            #					geom_jitter(width=0.5, position=position_jitter(0.05), size=3, colour="#888888") +
            stat_summary(
              fun.data = mean_cl_normal,
              geom = "errorbar",
              colour = "red",
              width = 0.1,
              size = 1
            ) +
            stat_summary(
              fun.y = mean,
              geom = "point",
              color = "red",
              size = 2
            )  +
            #					labs(title=paste("Cluster",i), y="Expression Z-score")+#, x=factors.names[1], colour=factors.names[2]) +
            labs(
              title = paste("Cluster", i),
              y = "Expression Z-score",
              caption = footnote
            ) + #, x=factors.names[1], colour=factors.names[2]) +
            theme(
              plot.title = element_text(size = 20, hjust = 0.5),
              plot.caption = element_text(size = 10, hjust = 0.5),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10) ,
              axis.text.x = element_text(
                size = 10,
                colour = "#888888",
                angle = 360,
                hjust = 0.5
              ),
              axis.text.y = element_text(size = 12, colour = "#888888")
            )
          
          myplotsstrip[[i]] <<- (ggstrip)
        }
        
        else {
          ##### jitter classique avec boxplots par probe
          
          ggstrip = ggplot(data = dataStackeddt, aes(
            x = Grp,
            y = Expression,
            color = probeName
          )) +
            theme_bw() + theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()
            ) +
            theme(axis.line = element_line(colour = "#888888")) +
            geom_boxplot(outlier.shape = 1, notch = F) +
            #geom_jitter(position=position_dodge(0.6), width=0.8) +
            theme(legend.position = "none") +
            #					geom_jitter(width=0.5, position=position_dodge(0.05), size=3, colour="#888888") +
            stat_summary(
              fun.data = mean_cl_normal,
              geom = "errorbar",
              colour = "red",
              width = 0.1,
              size = 1
            ) +
            stat_summary(
              fun.y = mean,
              geom = "point",
              color = "red",
              size = 2
            )  +
            #					labs(title=paste("Cluster",i), y="Expression Z-score")+#, x=factors.names[1], colour=factors.names[2]) +
            labs(
              title = paste("Cluster", i),
              y = "Expression Z-score",
              caption = footnote
            ) + #, x=factors.names[1], colour=factors.names[2]) +
            theme(
              plot.title = element_text(size = 24, hjust = 0.5),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10) ,
              axis.text.x = element_text(
                size = 10,
                colour = "#888888",
                angle = 360,
                hjust = 0.5
              ),
              axis.text.y = element_text(size = 12, colour = "#888888")
            )
          myplotsstrip[[i]] <<- (ggstrip)
        }
        
      })
    }
    return(myplotsstrip[[as.numeric(num)]])
  }
  
  ####################"" END ggplot2
  
  
  ###=======================
  ## plot de la heatmap
  ###=======================
  
  
  if (hmp.plot) {
    cat(" ->plot heatmap for each subgroup \n")
    
    # for (i in 1:length(cut02$lower)) {
    #   if (length(labels(cut02$lower[[num]])) > 1) {
    #     rowIds = NA
        
        #    if(length(labrow)>1){ rowIds=labrow[labels(cut02$lower[[i]])]
        #    }else if(labrow==T) rowIds=DEGres$ResTable$GeneName[labels(cut02$lower[[i]])]
        #if(length(labrow)>1){ rowIds=labrow[labels(cut02$lower[[i]])]
        
        if (labrow == T)
          rowIds = DEGres$ResTable[labels(cut02$lower[[num]]), "GeneName"]
        
      # View(as.matrix(exprData[labels(cut02$lower[[num]]),]))
        useRasterTF = T
        #m02gp = heatmaply(
        heatmaply(
          as.matrix(exprData[labels(cut02$lower[[num]]),]),
          height=900,col = col.hm,distfun = distfunTRIX,hclustfun = hclustfun,
          scale = scale, Colv = hmp$colDendrogram
          )%>%
          layout(margin = list(l = 130, b = 100))

          
          
        #Rowv = str(cut02$lower[[num]]),
        # Colv = hmp$colDendrogram,
        # col = col.hm,
        # distfun = distfunTRIX,
        # hclustfun = hclustfun,
        # labRow = rowIds,
        # labCol = colid,
        # ColSideColors = gpcol,
        # cexCol = cexcol,
        # cexRow = cexrow,
        # scale = scale,
        # na.rm = T,
        # margins = c(8, 8),
        # useRaster = useRasterTF

        # mtext(
        #   side = 3,
        #   sort(levels(groups)),
        #   adj = 1,
        #   padj = seq(0, by = 1.4, length.out = length(levels(groups))),
        #   col = cl[(1:length(levels(groups)))],
        #   cex = 1,
        #   line = 3
        # )
        #return(hm02gp)
     #}
    return(hm02gp)
  # }
  }
}
