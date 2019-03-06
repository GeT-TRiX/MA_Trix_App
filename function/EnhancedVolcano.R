# Author: Kevin Blighe
# Title: Publication-ready volcano plots with enhanced colouring and labelign
# Github: https://github.com/kevinblighe/EnhancedVolcano
# Modified by Franck Soubès (add 74-85;  modify 98-107, add 3 parameters displayLab, findfamily, topgenes)

#' Volcano plots represent a useful way to visualise the results of differential expression analyses. Here, we present a highly-configurable function that
#'produces publication-ready volcano plots. EnhancedVolcano will attempt to fit as many transcript names in the plot window as possible,
#' thus avoiding 'clogging' up the plot with labels that could not otherwise have been read.
#'
#' @param toptable  Requires at least the following: column for transcript names (can be rownames); a column for log2 fold changes; a column for nominal or adjusted p-value. REQUIRED.
#' @param lab A column name in toptable containing transcript names. Can be rownames(toptable). REQUIRED.
#' @param x A column name in toptable containing log2 fold changes. REQUIRED.
#' @param y A column name in toptable containing nominal or adjusted p-values. REQUIRED.
#' @param selectLab  A vector containing a subset of lab. DEFAULT = NULL. OPTIONAL.
#' @param displaylab A comma-separated values corresponding to the displayed genes. DEFAULT = NULL. OPTIONAL.
#' @param findfamily A character to parse familiy of genes. DEFAULT = NULL. OPTIONAL.
#' @param topgenes A numeric value to display the top n genes based on the regulation.DEFAULT = NULL. OPTIONAL.
#' @param regulationvolc A character for the regulation ("up", "down", "both") .DEFAULT = NULL. OPTIONAL.
#' @param xlim Limits of the x-axis. DEFAULT = c(min(toptable[,x], na.rm=TRUE),max(toptable[,x], na.rm=TRUE)). OPTIONAL.
#' @param ylim Limits of the y-axis. DEFAULT = c(0, max(-log10(toptable[,y]), na.rm=TRUE) + 5). OPTIONAL.
#' @param xlab Label for x-axis. DEFAULT = bquote(Log[2]~ "fold change"). OPTIONAL.
#' @param ylab Label for y-axis. DEFAULT = bquote(-Log[10]~ P)).OPTIONAL.
#' @param axisLabSize Size of x- and y-axis labels. DEFAULT = 18. OPTIONAL.
#' @param pCutoff Cut-off for statistical significance. A horizontal line will be drawn at -log10(pCutoff). DEFAULT = 10e-6. OPTIONAL.
#' @param pLabellingCutoff Labelling cut-off for statistical significance. DEFAULT = pCutoff. OPTIONAL
#' @param FCcutoff Cut-off for absolute log2 fold-change. Vertical lines will be drawn at the negative and positive values of log2FCcutoff. DEFAULT =1.0. OPTIONAL.
#' @param title Plot title. DEFAULT = 'Volcano plot'. OPTIONAL.
#' @param titleLabSize Plot subtitle. DEFAULT = 'Bioconductor package, EnhancedVolcano'. OPTIONAL.
#' @param transcriptPointSize Size of plotted points for each transcript. DEFAULT = 0.8. OPTIONAL.
#' @param transcriptLabSize Size of labels for each transcript. DEFAULT = 3.0. OPTIONAL.
#' @param col Colour shading for plotted points, corresponding to< abs(FCcutoff) && > pCutoff, > abs(FCcutoff), < pCutoff,> abs(FCcutoff) && < pCutoff. DEFAULT = c("grey30", "forestgreen","royalblue", "red2"). OPTIONAL.
#' @param colAlpha Alpha for purposes of controlling colour transparency oftranscript points. DEFAULT = 1/2. OPTIONAL.
#' @param legend Plot legend text. DEFAULT = c("NS", "Log2 FC", "P","P & Log2 FC"). OPTIONAL.
#' @param legendPosition Position of legend ("top", "bottom", "left","right"). DEFAULT = "top". OPTIONAL.
#' @param legendLabSize Size of plot legend text. DEFAULT = 14. OPTIONAL.
#' @param legendIconSize Size of plot legend icons / symbols. DEFAULT = 4.0.OPTIONAL.
#' @param DrawConnectors Logical, indicating whether or not to connect plotlabels to their corresponding points by line connectors. DEFAULT = FALSE.OPTIONAL.
#' @param widthConnectors Line width of connectors. DEFAULT = 0.5. OPTIONAL.
#' @param colConnectors Line colour of connectors. DEFAULT = 'grey10'. OPTIONAL.
#' @param cutoffLineType Line type for FCcutoff and pCutoff ("blank","solid", "dashed", "dotted", "dotdash", "longdash", "twodash").DEFAULT = "longdash". OPTIONAL.
#' @param cutoffLineCol Line colour for FCcutoff and pCutoff. DEFAULT ="black". OPTIONAL.
#' @param cutoffLineWidth Line width for FCcutoff and pCutoff. DEFAULT = 0.4. OPTIONAL.
#'
#' @author Kevin Blighe <kevin@clinicalbioinformatics.co.uk>
#'
#' @return A list of two elements
#' @export
#'
#' @examples

EnhancedVolcano <- function(
    toptable,
    lab,
    x,
    y,
    selectLab = NULL,
    displaylab = NULL,
    findfamily = NULL,
    topgenes = NULL,
    regulationvolc = NULL,
    xlim = c(min(toptable[,x], na.rm=TRUE),
        max(toptable[,x], na.rm=TRUE)),
    ylim = c(0, max(-log10(toptable[,y]), na.rm=TRUE) + 5),
    xlab = bquote(~Log[2]~ "fold change"),
    ylab = bquote(~-Log[10]~italic(P)),
    axisLabSize = 16,
    pCutoff = 0.05,
    pLabellingCutoff = pCutoff,
    FCcutoff = 2.0,
    title = "",
    titleLabSize = 16,
    transcriptPointSize = 2.8,
    transcriptLabSize = 5.0,
    col = c("grey30", "forestgreen", "royalblue", "red2"),
    colAlpha = 1/2,
    legend = c("NS","Log2 FC","P","P & Log2 FC"),
    legendPosition = "top",
    legendLabSize = 10,
    legendIconSize = 3.0,
    DrawConnectors = FALSE,
    widthConnectors = 0.5,
    colConnectors = "black",
    cutoffLineType = "longdash",
    cutoffLineCol = "black",
    cutoffLineWidth = 0.4)

{
    if(!requireNamespace("ggplot2")) {
        stop("Please install ggplot2 first.", call.=FALSE)
    }

    if(!requireNamespace("ggrepel")) {
        stop("Please install ggrepel first.", call.=FALSE)
    }

    if(!is.numeric(toptable[,x])) {
        stop(paste(x[i], " is not numeric!", sep=""))
    }

    if(!is.numeric(toptable[,y])) {
        stop(paste(x[i], " is not numeric!", sep=""))
    }

    requireNamespace("ggplot2")
    requireNamespace("ggrepel")
    requireNamespace("dplyr")
    i <- xvals <- yvals <- Sig <- NULL

    toptable <- as.data.frame(toptable)
    toptable$GeneName <- sapply(toptable$GeneName, function(v) {
      if (is.character(v)) return(toupper(v))
      else return(v)
    })

    toptable$Sig <- "NS"
    toptable$Sig[(abs(toptable[,x]) > FCcutoff)] <- "FC"
    toptable$Sig[(toptable[,y]<pCutoff)] <- "P"
    toptable$Sig[(toptable[,y]<pCutoff) &
        (abs(toptable[,x])>FCcutoff)] <- "FC_P"
    toptable$Sig <- factor(toptable$Sig,
        levels=c("NS","FC","P","FC_P"))



    if(is.na(topgenes) && !is.na(displaylab) ){
      selectLab <- as.character(displaylab)
    }
    else if(is.na(topgenes) && is.na(displaylab) && !is.na(findfamily) )
      selectLab <- as.character(findfamily)
    else if(is.na(topgenes)&& is.na(displaylab)&& is.na(findfamily)){
      selectLab <- ""
    }
    else{
      if(regulationvolc == "both")
        toptable$abs <-  unlist(abs(toptable[x]))
      else if(regulationvolc == "up"){
        toptable$abs <- unlist((toptable[x]))
      }
      else{
        toptable$abs <- unlist((toptable[x]))
      }

      toptable$X <- rownames(toptable)
      myval <- toptable %>%   dplyr::filter(Sig =="FC_P") %>% dplyr::select(GeneName,X,abs)  %>%
      {if (regulationvolc == "down") top_n(.,-topgenes) else top_n(.,topgenes)}
      myvalueind <- myval$X
      selectLab <- as.character(myval$GeneName)

    }


    if (min(toptable[,y], na.rm=TRUE) == 0) {
        warning("One or more P values is 0. Converting to minimum possible value...", call. = FALSE)
        toptable[which(toptable[,y] == 0), y] <- .Machine$double.xmin
    }

    toptable$lab <-  sapply(toptable$GeneName, function(v) {
      if (is.character(v)) return(toupper(v))
      else return(v)
    })



    toptable$xvals <- toptable[,x]
    toptable$yvals <- toptable[,y]


   if (!is.null(selectLab)) {
    if(!is.na(topgenes) && is.na(displaylab)&& is.na(findfamily)){
    names.new <- rep("", length(toptable$lab))
    indices <- which(toptable$X %in% myvalueind)
    names.new[indices] <- as.character(toptable$GeneName[indices])
    toptable$lab <- names.new
    }
    else {
        names.new <- rep("", length(toptable$lab))
        indices <- which(toptable$GeneName %in% selectLab)
        names.new[indices] <- as.character(toptable$GeneName[indices])
        toptable$lab <- names.new
      }
    }

    subdata = subset(toptable,
                     toptable[,y]<pLabellingCutoff &
                       abs(toptable[,x])>FCcutoff)





    plot <- ggplot2::ggplot(toptable,
            ggplot2::aes(x=xvals, y=-log10(yvals))) +

        ggplot2::geom_point(ggplot2::aes(color=factor(Sig)),
            alpha=colAlpha, size=transcriptPointSize) +

        ggplot2::scale_color_manual(values=c(NS=col[1],
            FC=col[2],
            P=col[3],
            FC_P=col[4]),
            labels=c(NS=legend[1],
            FC=paste(legend[2], sep=""),
            P=paste(legend[3], sep=""),
            FC_P=paste(legend[4], sep=""))) +

        ggplot2::theme_bw(base_size=24) +

        ggplot2::theme(
            legend.background=ggplot2::element_rect(),
            plot.title=ggplot2::element_text(angle=0,
                size=titleLabSize,
                face="bold",
                vjust=1),

            panel.grid.major=ggplot2::element_blank(),
            panel.grid.minor=ggplot2::element_blank(),

            axis.text.x=ggplot2::element_text(angle=0,
                size=axisLabSize,
                vjust=1),
            axis.text.y=ggplot2::element_text(angle=0,
                size=axisLabSize,
                vjust=1),
            axis.title=ggplot2::element_text(size=axisLabSize),

            legend.position=legendPosition,
            legend.key=ggplot2::element_blank(),
            legend.key.size=ggplot2::unit(0.5, "cm"),
            legend.text=ggplot2::element_text(
                size=legendLabSize),
            title=ggplot2::element_text(
                size=legendLabSize),
            legend.title=ggplot2::element_blank()) +

        ggplot2::guides(colour = ggplot2::guide_legend(
            override.aes=list(size=legendIconSize))) +

        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +

        ggplot2::xlim(xlim[1], xlim[2]) +
        ggplot2::ylim(ylim[1], ylim[2]) +

        ggplot2::ggtitle(title) +

        ggplot2::geom_vline(xintercept=c(-FCcutoff, FCcutoff),
            linetype=cutoffLineType,
            colour=cutoffLineCol,
            size=cutoffLineWidth) +

        ggplot2::geom_hline(yintercept=-log10(pCutoff),
            linetype=cutoffLineType,
            colour=cutoffLineCol,
            size=cutoffLineWidth)



    if (DrawConnectors == TRUE) {
        plot <- plot + ggrepel::geom_text_repel(max.iter = 100,
            data=subdata ,
            ggplot2::aes(label=subset(toptable,
                toptable[,y]<pLabellingCutoff &
                    abs(toptable[,x])>FCcutoff)[,"lab"]),
                size = transcriptLabSize,
                segment.color = colConnectors,
                segment.size = widthConnectors,
                vjust = 1.0)
    } else if (DrawConnectors == FALSE && !is.null(selectLab)) {
        plot <- plot + ggplot2::geom_text(data=subdata,
            ggplot2::aes(label=subset(toptable,
                toptable[,y]<pLabellingCutoff &
                    abs(toptable[,x])>FCcutoff)[,"lab"]),
                size = transcriptLabSize,
		check_overlap = T,
                vjust = 1.0)
    } else if (DrawConnectors == FALSE && is.null(selectLab)) {
        plot <- plot + ggplot2::geom_text(data=subdata,
            ggplot2::aes(label=subset(toptable,
                toptable[,y]<pLabellingCutoff &
                    abs(toptable[,x])>FCcutoff)[,"lab"]),
                size = transcriptLabSize,
                check_overlap = F,
                vjust = 1.0)
    }

    if(!is.na(topgenes)) subdata <- filter(subdata, lab != "")
    mylist = list(plot, subdata)
    return(mylist)
}
