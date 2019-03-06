### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#' cutHeatmaps if a function that takes as input an heatmap object and depending on the cut height and the cluster
#' choosen render a ggplot object or an heatmap object
#'
#' @param hmp An heatmap object
#' @param height A numeric value to cut the dendogram
#' @param exprData A data frame with specific columns depending on the user's choices
#' @param groups A data frame of the choosen groups
#' @param cexcol A positive numbers, used as cex.axis in for the row or column axis labeling
#' @param cexrow A positive numbers, used as cex.axis in for the row or column axis labeling
#' @param labrow A character vectors with row and column labels to use
#' @param fileType A character to select the plot to display heatmap, boxplot or stripchart
#' @param meanGrp A boolean value to computes the mean for each groups; default = F
#' @param type A character to select the plot to display heatmap, boxplot or stripchart
#' @param las A numeric value
#' @param distfun Function used to compute the distance (dissimilarity) between both rows and columns.
#' @param palette.col A character vector of colors
#' @param num An item of the heatmap object corresponding to a specific cluster choosen by the user
#' @param genename A character vector of gene symbols
#' @param scales A character indicating if the values should be centered and scaled in either the row direction or the column direction, or none. The default is "none"
#' @param ...
#'
#' @return a ggplot object or heatmapply object
#'
#' @export



cutHeatmaps = function(hmp, height, exprData, groups, cexcol = 1, cexrow = 1, labrow = F, genename = NULL ,
                       fileType = "png",scales = "row",meanGrp = F,mypal = NULL,
                       type = "None",las = 2,distfun = "cor",palette.col = NULL,num = 4,...)
{


  require(ggplot2)
  require(grid)
  require(gridExtra)

  plot.boxplot = ifelse(type == "Boxplot", T, F)
  plot.stripchart = ifelse(type == "LB" | type == "WB", T, F)
  hmp.plot = ifelse(type == "Heatmap", T, F)
  probes.boxplot = ifelse(type == "WB", T, F)

  if(is.null(mypal))
    mypal = brewer.pal(8,"Dark2") %>%
    list(brewer.pal(10,"Paired")) %>%
    unlist()

  if(!is.null(palette.col)){
    palette(palette.col);
  }else  palette(mypal)

  cl=palette(mypal);
  colid = colnames(exprData)
  rownames(exprData) = rownames(genename)
  exprData = exprData[labels(hmp$rowDendrogram), ]
  wdt = 900
  wdte = 12
  exprData0 = exprData
  groups0 = groups
  if (meanGrp) {
    cat("\n -> calculating groups means \n")
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
  ## cut the heatmap
  ###=======================

  # Cut the dendogram in 2 part with the desired height

  cut02 = cut(hmp$rowDendrogram, h = height)
  cat("\n -> size of", length(cut02$lower), "sub-dendrograms\n")
  print(sapply(cut02$lower, function(x)
    length(labels(x))))
  #To check the sub group population of genes

  cat(" ->export result tables for each subgroup \n")
  gpcol=num2cols(as.numeric(groups))



    # Row names of each clsuter
    HCgroupsLab=lapply(cut02$lower,function(x)labels(x))

    # Expression of each cluster
    HCgroupsLabExrs=lapply(HCgroupsLab,function(x)exprData0[x,])

    ## scaling
#		HCgroupsLabExrsCenterScale <- ifelse(scales=="row",lapply(HCgroupsLabExrs,function(y){t(scale(t(y),center=T,scale=T))}),HCgroupsLabExrs)

    if(scales=="row"){
      HCgroupsLabExrsCenterScale <- lapply(HCgroupsLabExrs,function(y){t(scale(t(y),center=T,scale=T))})
    }else HCgroupsLabExrsCenterScale <- HCgroupsLabExrs


    # mean expression for each group and each cluster
    grps=groups0
    HCgroupsLabExrsCenterScaleMean = lapply(HCgroupsLabExrsCenterScale, function(y) {
    t(apply( y,1,
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
    myplots <- list()

    for (i in 1:length(HCgroupsLabExrsCenterScaleMean)) {
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
        ggbplot = ggplot(dataCentSm, aes(x = Group, y = Expression)) +
          theme_bw() + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
          ) +
          theme(axis.line = element_line(colour = "#888888")) +
          scale_fill_manual(values = cl[(1:length(levels(groups)))]) +
          geom_jitter(
            position = position_jitter(0.16) ,
            colour = "#888888",
            alpha = 0.4,
            shape = 16,
            size = 3
          ) +


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
          (ggbplot +
             geom_violin(aes(fill = Group),alpha = 0.3,width=0.3)+
             geom_boxplot(width = 0.1, aes(fill = Group),alpha = 0.3))
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

          ##=============
          ## plot stripchart
          ##
          #### jitter classique



          ggstrip = ggplot(dataStackeddt, aes(x = Grp, y = Expression)) +
            theme_bw() + theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()
            ) +
            theme(axis.line = element_line(colour = "#888888")) +
            geom_violin() +
            geom_jitter(
              position = position_jitter(0.16) ,
              colour = "#888888",
              alpha = 0.05,
              shape = 16,
              size = 3
            ) +

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
            labs(
              title = paste("Cluster", i),
              y = "Expression Z-score",
              caption = footnote
            ) +
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
            theme(legend.position = "none") +
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
            labs(
              title = paste("Cluster", i),
              y = "Expression Z-score",
              caption = footnote
            ) +
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


        if(hmp.plot){
           cat(" ->plot heatmap for each subgroup \n")
          for(i in 1:length(cut02$lower)){
            if(length(labels(cut02$lower[[i]]))>1){
            rowIds=NA;


            useRasterTF=T;
            hm02gp=heatmap(exprData[labels(cut02$lower[[1]]),], Rowv=str(cut02$lower[[1]]),
            Colv=hmp$colDendrogram,
            distfun=distfunTRIX,
            hclustfun=hclustfun,
            labRow =rowIds,
            labCol=colid,
            ColSideColors=gpcol,
            cexCol=cexcol,
            cexRow=cexrow,
            scale=scales,
            na.rm=T,
            margins=c(8,8),
            useRaster=useRasterTF)
  }
  }
    return(hm02gp)
}
}
