require(dplyr)
require(RColorBrewer)


#' distcor is a function that computes the distance correlation 
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

distcor<-function(x) {as.dist(1-cor(t(x),use="pairwise.complete.obs"))}


#' disteuc is a function that computes the euclidean correlation
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

disteucl<-function(x) {dist(x,method="euclidian")}


#' hclustfun is a function that performs a hiearchical cluster analysis with the method of Ward
#'
#' @param d 
#'
#' @return an object of class hclust
#' 
#' @export

hclustfun=function(d) {hclust(d,method="ward.D2")} 

require("Biobase")  
require("marray") 


#' num2cols is a function that gives a character vector of color names for a given numeric vector
#'
#' @param numVector a numeric vector
#' @param colp a character vector
#'
#' @return a matrix object 
#' 
#' @export

num2cols=function(numVector,colp=palette()){
  
  gpcol=as.data.frame(numVector)

  numCols=length(unique(gpcol[,1]))
  mycol <- sort(unique(gpcol$numVector))
  if(length(colp) <numCols) warning("number of color names < number of levels! Give a color palette with at least ",numCols," terms to 'col' argument")
  cols=as.data.frame(matrix(c(mycol,colp[1:(numCols)]),nrow=numCols)) ## couleurs pour les groupes
  myColFun=function(x){
    as.character(cols[cols[,1]==x[1],2])
  }
  apply(gpcol,1,FUN=myColFun)
  
}



############################################################################
##   plotHeatmaps() function                                              
############################################################################ 


#' plotHeatmaps is a function that create a false color image with a dendogram added to the left side and the top
#'
#' @param exprData a data frame with all the individuals selected
#' @param geneSet a data frame with the indexes corresponding to the sigificant genes
#' @param groups a data frame with the corresponding groups 
#' @param workingPath an access path
#' @param k a numeric value which aim is to defined the treshold value to cut the dendogram
#' @param fileType a character value which extension corresponds to different file types
#' @param cexcol  a positive numbers, used as cex.axis in for the row or column axis labeling
#' @param cexrow a positive numbers, used as cex.axis in for the row or column axis labeling
#' @param colOrder a positive numbers, used as cex.axis in for the row or column axis labeling
#' @param labrow a character vectors with row and column labels to use
#' @param na.color color to use for missing value 
#' @param scale a character indicating if the values should be centered and scaled in either the row direction or the column direction, or none
#' @param hclustGenes a function used to compute the hierarchical clustering when Rowv or Colv are not dendrograms
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#' @param plotRowSideColor a boolean value that colors or not the rows side
#' @param RowSideColor a character vector of length nrow containing the color names for a vertical side bar that may be used to annotate the rows of x.
#' @param Rowdistfun a function used to compute the distance for the rows
#' @param Coldistfun a function used to compute the distance for the columns
#' @param palette.col NULL
#' @param margins a numeric vector of length 2 containing the margins (see par(mar= *)) for column and row names, respectively.
#' @param my_palette a character vetor of length 17
#' @param mycex a numeric value which aim is to change the size of the legend
#' @param mypal a character vetor of length 17 
#' @param colid a character vectors with row and column labels to use; default NULL
#' @param showcol a boolean value used to hide or show the colnames 
#' @param showrow a boolean value used to hide or show the rownames
#' @param genename a data frame list corresponding to the gene names 
#'
#' @return an heatmap object
#' 
#' @export


plotHeatmaps=function(exprData,geneSet,groups,workingPath=getwd(),k=3,fileType="png",cexcol=1.5,cexrow=1.5,
                      colOrder=NULL,labrow=F,na.color="black",scale="row",hclustGenes=T,meanGrp=F,plotRowSideColor=T,#col.hm=greenred(75),
                      RowSideColor=c("gray25","gray75"), Rowdistfun="correlation",Coldistfun="correlation" ,palette.col=NULL, 
                      margins=c(8,8),my_palette=colorRampPalette(c("green", "black", "red"))(n = 75),mycex = 0.6,mypal=test,colid = NULL, showcol = T ,showrow =F, genename=NULL, notplot = F){
  
  #RowSideColor: color palette to be used for rowSide cluster colors
  # can also be gray.colors(k, start = 0.2, end = 0.9) to get k colors of gray scale
  

  if(is.null(showcol))
     showcol = F
  
  if(is.null(showrow))
    showrow = F

  if(showcol == T)
    colid = NA
  else
    colid = NULL
  
  if(showrow == F)
    rowIds = NA
  else
    rowIds = genename[geneSet]

  
  if(is.null(mypal))
    mypal = brewer.pal(8,"Dark2") %>%
      list(brewer.pal(10,"Paired")) %>%
      unlist()
    
    
    # mypal =c ("#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442",
    #          "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray",
    #          "burlywood1","darkkhaki", "#CC0000" )
  
  
  if(!is.null(palette.col)){
    palette(palette.col);
  }else  palette(mypal)


  if( any(rownames(exprData) != rownames(exprData)[order(as.numeric(rownames(exprData)))])) stop("Error: 'exprData' must have rownames in numerical ascending order!");
  if(length(RowSideColor)==1) RowSideColor=gray.colors(k, start = 0.2, end = 0.9)
  if(!Rowdistfun %in% c("correlation","euclidian")) stop("Rowdistfun must be one of 'cor' or 'euclidian'!")
  if(!Coldistfun %in% c("correlation","euclidian")) stop("Coldistfun must be one of 'cor' or 'euclidian'!")
  
  library(gplots)
  library(marray)
  cl=palette(mypal);
  
  exprData=exprData[geneSet,]

  ##-----------------------##
  ## Row dendrogram
  ##-----------------------##
  cat("\n -> Hierarchical clustering on genes... \n")
  if(Rowdistfun=="correlation"){	
    hc=hclustfun(distcor(exprData))
    subdist="dist method: 1-cor";
    distfunTRIX= distcor;
  } 
  if(Rowdistfun=="euclidian"){
    hc=hclustfun(disteucl(exprData))
    subdist="dist method: euclidian";
    distfunTRIX = disteucl;
  }
  
  rowv=as.dendrogram(hc)
  if(meanGrp){
    cat("\n -> calculating groups means... \n")
    suffix=paste("meanGrp",suffix,sep="_")
    exprData=t(apply(exprData,1,FUN=function(x){tapply(x,groups,mean,na.rm=T)}))
    cat("    Done \n")
    groups=factor(levels(groups),levels=levels(groups))

  }
  
  ##**********
  ## Rownames
  
  ##-----------------------##
  ## Col dendrogram
  ##-----------------------##
  
  gpcol=num2cols(as.numeric(groups))


  ##**********
  ## RowDendrogram
  
  # build dendrogram

  
  if(Coldistfun=="correlation")
    ColvOrd = exprData %>%
      t() %>%
      distcor()%>%
      hclustfun()%>%
      as.dendrogram()

  if(Coldistfun=="euclidian")
    ColvOrd = exprData %>%
    t() %>%
    disteucl()%>%
    hclustfun()%>%
    as.dendrogram()
  
  # re-order dendrogram
  
  if(!is.null(colOrder)){ # reorder col dendrogram
    if(length(colOrder)==ncol(exprData)){
      ord=colOrder	#vector of order values
    } else ord=1:ncol(exprData); # TRUE: create default 1:ncol
    ColvOrd=reorder(ColvOrd,ord,agglo.FUN = mean) # reorder by mean values
  }else {ColvOrd=ColvOrd} # no reordering
  
  
  #### heatmap  genes 
  
  #useRasterTF=F;
  
  ##-----------------------##
  ## plot Row dendrogram
  ##-----------------------##
  
  if(hclustGenes){
    cat("\n -> Plotting Dendrogram... \n")
    plot(hc,hang=-1,labels=FALSE,sub=paste("hclust method: ward2\n", subdist),xlab="",main="")
    hcgp=rect.hclust(hc,k=k,border="red")
    
    
    #hts=rev( tail( hc$height,15))
    #bp=barplot(hts,names.arg=1:length(hts))
    #text(x=bp,y=hts,label= formatC(hts,1,format="f"),pos=3,cex=0.8) 
    cat("    Done \n")
  }
  
  ##-----------------------##
  ## plotRowSideColor
  ##-----------------------##
  
  if(plotRowSideColor){
    if(!hclustGenes){
      #png(".tmp")
      plot(hc,hang=-1,labels=FALSE,xlab="",main="")
      hcgp=rect.hclust(hc,k=k,border="red")
      #file.remove(".tmp")
    }
    if(length(RowSideColor) == length(geneSet)){
      gpcolr=RowSideColor;
    }else {
      if(length(RowSideColor) != length(geneSet)){
        gphcc=as.data.frame(matrix( c(hc$labels[hc$order], rep(1:k,times=sapply(hcgp,length))),nrow=length(hc$labels)),stringsAsFactors=F)
        colnames(gphcc)=c("probe","cluster")
        gphccOrd=gphcc[order(as.numeric(gphcc[,1])),]
        hcgp=factor(paste("c",gphccOrd[,2],sep=""),levels=paste("c",rep(1:k),sep=""))
        gpcolr=num2cols(as.numeric(hcgp),c(rep(RowSideColor,20)[1:(k)]))
        print(RowSideColor)
      }else gpcolr=NULL
    }
  }else gpcolr=rep("white",nrow(exprData))
  
  
  ##-----------------------##
  ## plot Heatmap
  ##-----------------------##
  cat("\n -> Plotting HeatMap... \n")
  
  #print(gphcc)
  #View(exprData)

  #return(gphcc)
  par("mar")
  par(mar=c(5,5,1,1.10))
  hmp02 = heatmap.2(exprData,na.rm=T,dendrogram="both",labRow = rowIds,labCol=colid,scale=scale, RowSideColors=gpcolr, ColSideColors=gpcol,key=T,
                    keysize=1, symkey=T, trace="none",density.info="density",distfun=distfunTRIX, hclustfun=hclustfun,cexCol=cexcol,
                    Colv=ColvOrd,Rowv=rowv,na.color=na.color,cexRow=cexrow,useRaster=T,margins=margins,layout(lmat =rbind(4:3,2:1),lhei = c(0.05,1), lwid = c(0.1,1)),col=my_palette,key.par = list(cex=0.6))
  mtext(side=3,sort(levels(groups)),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[(1:length(levels(groups)))],cex=mycex,line=-1)
  if(notplot)
    dev.off()

  return(hmp02)
}
library(shiny)

# @ Author: Joe Cheng
# Also uses parallel, shinyjs, tools
# Create a long-running task, executed in a forked process. (Doesn't work on Windows)
# 
# The return value is a promise-like object with three
# methods:
# - completed(): FALSE initially, then TRUE if the task succeeds,
#   fails, or is cancelled. Reactive, so when the state changes
#   any reactive readers will invalidate.
# - result(): Use this to get the return value. While execution is
#   in progress, performs a req(FALSE). If task succeeded, returns
#   the return value. If failed, throws error. Reactive, so when
#   the state changes any reactive readers will invalidate.
# - cancel(): Call this to prematurely terminate the task.
create_forked_task <- function(expr) {
  makeReactiveBinding("state")
  state <- factor("running",
                  levels = c("running", "success", "error", "cancel"),
                  ordered = TRUE
  )
  
  result <- NULL
  
  # Launch the task in a forked process. This always returns
  # immediately, and we get back a handle we can use to monitor
  # or kill the job.
  task_handle <- parallel::mcparallel({
    force(expr)
  })
  
  # Poll every 100 milliseconds until the job completes
  o <- observe({
    res <- parallel::mccollect(task_handle, wait = FALSE)
    if (is.null(res)) {
      invalidateLater(100)
    } else {
      o$destroy()
      if (!is.list(res) || length(res) != 1 || !inherits(res[[1]], "try-error")) {
        state <<- "success"
        result <<- res[[1]]
      } else {
        state <<- "error"
        result <<- attr(res[[1]], "condition", exact = TRUE)
      }
    }
  })
  
  list(
    completed = function() {
      state != "running"
    },
    result = function() {
      if (state == "running") {
        # If running, abort the current context silently.
        # We've taken a reactive dependency on "state" so if
        # the state changes the context will invalidate.
        req(FALSE)
      } else if (state == "success") {
        return(result)
      } else if (state == "error") {
        stop(result)
      } else if (state == "cancel") {
        validate(need(FALSE, "The operation was cancelled"))
      }
    },
    cancel = function() {
      if (state == "running") {
        state <<- "cancel"
        o$destroy()
        tools::pskill(task_handle$pid, tools::SIGTERM)
        tools::pskill(-task_handle$pid, tools::SIGTERM)
        parallel::mccollect(task_handle, wait = FALSE)
      }
    }
  )
}#' cutHeatmaps if a function that takes as input an heatmap object and depending on the cut height and the cluster
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
#' @export

cutHeatmaps = function(hmp,height,exprData,DEGres,groups,cexcol = 1,cexrow = 1,labrow = T,
                       fileType = "png",scale = "row",meanGrp = F,mypal = NULL,
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
  
  if(is.null(mypal))
    mypal = brewer.pal(8,"Dark2") %>%
    list(brewer.pal(10,"Paired")) %>%
    unlist()
  
  if(!is.null(palette.col)){
    palette(palette.col);
  }else  palette(mypal)
  
  cl=palette(mypal);
  
  
  
  # if (!is.null(palette.col)) {
  #   cl = palette(palette.col)
    
    #		}else cl=palette(c("black", "blue", "cyan", "magenta",   "darkgray", "darkgoldenrod", "violet",  "orange", "lightgreen","lightblue", "darkorchid", "darkred","darkslateblue", "darkslategray", "maroon", "burlywood1" , "darkolivegreen"));
  # } else
  #   cl =  palette(
  #     c(
  #       "#000000",
  #       "#0072c2",
  #       "#D55E00",
  #       "#999999",
  #       "#56B4E9",
  #       "#E69F00",
  #       "#CC79A7",
  #       "lightblue",
  #       "#F0E442",
  #       "lightgreen",
  #       "deepskyblue4",
  #       "darkred",
  #       "#009E73",
  #       "maroon3",
  #       "darkslategray",
  #       "burlywood1",
  #       "darkkhaki",
  #       "#CC0000"
  #     )
  #   )
  
  
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
  print(height)
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
    #library(heatmaply)
    
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
         #scale_fill_manual(values = cl[(1:length(levels(groups))) + 1]) +
          scale_fill_manual(values = cl[(1:length(levels(groups)))]) +
          geom_jitter(
            position = position_jitter(0.16) ,
            colour = "#888888",
            alpha = 0.4,
            shape = 16,
            size = 3
          ) +
          #geom_boxplot(width = 0.1, fill = NA) +
          
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
#' decTestTRiX is a function 
#'
#' @param adj a data frame with the "adj.P.Val"
#' @param logfc a data frame with the "logFC"
#' @param pval a data frame with the "P.value"
#' @param DEGcutoff a numeric value for tresholding the data on the pvalue
#' @param FC a numeric value for tresholding the data on the FC value
#' @param cutoff_meth a character to choose the method appropriate, "FDR" in order to cut off with the "adj.P.Val" and "None" for the "P.value"; default is set to "FDR"
#' @param maxDE a numeric value that gives a maximal number of genes to display for all the differents contrasts
#' @param contrast a numeric value representing the length of the data frame
#'
#' @return \DEsel a matrix of double values containing the signficant genes
#' 
#' @export

decTestTRiX <- function(adj,logfc,pval, DEGcutoff = 0.05 ,FC = 1,cutoff_meth = "FDR",maxDE = NULL,contrast = 1:ncol(adj))

{
  ## select probes with cutoff_meth<= DEGcutoff and FoldChange > FC and nbr of selected probes < maxDE (if nb FC selected >maxDE)
  
  if (length(contrast) == 1)
    contrast = c(contrast, contrast)
  
  if (is.na(maxDE))
    maxDE = nrow(adj)
  
  #print(maxDE)
  
  if (cutoff_meth == "FDR") 
    pList = adj[, contrast]

  
  if(cutoff_meth=="None")
    pList= pval[,contrast]

  
  ## select on pvalue
  DEp = pList <= DEGcutoff
  
  ## select on FC
  DEFC = 2 ** abs(logfc[, contrast]) >= FC
  
  
  ## reduce selection to maxDE
  if (any(colSums(DEFC) > maxDE)) {
    
    # reduce the nbr of selecte probes to maxDE for each cont
    # cat("\n -> reduction of selected probes to",
    #     maxDE,
    #     "in each contrast\n")
    
    DEmax = pList
    
    for (i in 1:ncol(DEFC))
      
    {
      if (maxDE > sum(DEFC[, i])) {
        maxDEi = sum(DEFC[, i])
      } else
        maxDEi = maxDE
      
      ord = order(DEmax[, i])
      msi = max(DEmax[ord, i][DEFC[ord, i]][1:maxDEi])

      DEmax[, i] = DEmax[, i] <= msi

    }
    
    DEsel = DEp & DEFC & DEmax
    #print(colSums(DEsel, na.rm = T))
  }
  
  else{
    DEsel = DEp & DEFC
    #print(colSums(DEsel, na.rm = T))
  
  }

  
  DEsel = which(rowSums(DEsel, na.rm = T) > 0)
  #cat("Il y a",length(DEsel),"gène significatifs")
  elements= list(DEsel, length(DEsel))
  
  
  return(elements)
  
}
##### Formating function for microarray data #### 
##### Franck Soubès


#' formating is a function alpha version of the higher elaborate decTestTRiX function 
#'
#' @param adj
#' @param pval 
#'
#' @return
#' @export
#'
#' 
#' @export

formating = function( adj, pval){
  
  
  passingval = adj %>%
    apply(2,FUN = function(x){return(x < pval)}) %>%
    apply(1,sum) 
  
  passingval = which( passingval > 0)
  
  cat("Il y a",length(passingval),"gène significatifs")

  return(passingval)

}


#' transform a dataframe containing factor for different levels not optimal tho
#'
#' @param dataframe 
#'
#' @return
#' 
#' @export


transform <- function(dataframe,toast){
  
  myl = list()
  cpt = 1
  for (i in toast) {
    command <- paste0(i, "<-subset(dataframe, Grp=='", i, "')")
    test = eval(parse(text=command))
    X = test$X
    Grp = test$Grp
    myl[[cpt]] = data.frame(X ,Grp)
    cpt = cpt+1
    dyn_grp <- Reduce(function(x, y) merge(x, y, all=TRUE), myl, accumulate=FALSE)
  }
  
  return(dyn_grp)
}


### ANother method faster and stronger
### thisisit <- select(musmuscu, as.character(factor(mydata$X)))
# selected = c("LKO_MCD ", "LKO_CTRL" )
# selected = levels(groupss$Grp)
# 
# 
#groupss[match(as.character(groupss$Grp), selected, nomatch = T), ]
# test <- groupss[groupss$Grp %in% selected,]
# 
# 
# uniquegrp = unique(test$Grp)
# btestos <- droplevels(test)


#' This function returns an integer for the number of significant genes
#'
#' @param adj a data frame
#' @param elem a list
#' @param pv a list
#'
#' @return \grp1 of class data frame
#' 
#' @export

evaluatesign = function(adj,elem,pv){
  

  grp1 = adj[,c(elem)] %>%
    sapply( FUN = function(x){return(x < pv)}) %>%
    data.frame() %>%
    filter(. == T) %>%
    nrow()
  
  return(grp1)
}


#' This function returns an integer for the number of significant genes using parallelism
#' 
#' @param adj 
#' @param elem 
#' @param pv 
#'
#' @return \grp1 of class data.frame
#' 
#' @export




evaluatesignpar = function(adj,elem,pv) { ### for benchmarking 
  
  
  grp1 = foreach(i = iter(adj[elem], by = "col"), .combine = c) %dopar%  
    (sign= {i < pv}) %>%
    as.data.frame() %>%
    filter(. == T) %>%
    nrow()
  
 return(grp1) 
  
}


#' Create a data frame containing the number of signficant genes for different conditions pval and log fc
#'
#' @param adj a data frame containing the adjusted p-value
#'
#' @return \dtsign a data frame 
#' 
#' @export

createdfsign = function(adj) {


  dtsign = data.frame(matrix(ncol = 2, nrow = length(adj[, -1])))
  y <- c("FDR < 0.01", "FDR < 0.05")

  dtsign = data.frame(matrix(ncol <- 2, nrow <- length(adj[, -1])))
  y <- c("pvalue(0.01)", "pvalue(0.05)")

  colnames(dtsign) <- y
  rownames(dtsign) <- colnames(adj[, -1])
  pvalue = c(0.01, 0.05)
  
  i <- 1
  for (pv in pvalue) {
    for (elem in colnames(adj[, -1])) {
      
      if (i %% constmod == 0) {
        i <- 1
      }
      if (pv == 0.05)
      {

        dtsign$`FDR < 0.05`[i] = evaluatesignpar(adj, elem, pv)
        i = i + 1
      }
      else{
        
        dtsign$`pvalue(0.01)`[i] = evaluatesignpar(adj, elem, pv)
        i <- i + 1

      }
    }
  }
  return(dtsign)
}



#' This function returns a data frame of the element which are superior to a vector character 1.2,2,4,6 and 10 and for a defined pvalue
#'
#' @param alltop a data frame
#' @param pval a numeric pvalue
#'
#' @return \fcpval a data frame 
#' 
#' @export

myfinalfc = function(alltop, pval, testrix) {
  
  j = 1
  
  whatest  = ifelse(testrix == "FDR", T, F)
  if (whatest)
    adj = alltop[, grep("^X|^adj.P.Val", names(alltop), value = TRUE)]
  else
    adj = alltop[, grep("^X|^P.value", names(alltop), value = TRUE)]
    
  logfc = alltop[, grep("^X|^logFC", names(alltop), value = TRUE)]
  myfc = c(1, 1.2, 2, 4, 6, 10)
  fcpval = data.frame(matrix(ncol = length(myfc), nrow = length(adj[, -1])))
  mycolnames = c("FC>1.0", "FC >1.2" , "FC >2", "FC >4", "FC >6", "FC >10")

  
  for (fc in myfc) {
    fcpval[j] = cbind.data.frame(colSums(adj[,-1] < pval &
                                          2 ** abs(logfc[,-1]) > fc))
    j = j + 1
  }

  names(logfc) =  gsub(
    pattern = "^logFC_",
    replacement = "",
    x = names(logfc),
    perl =  TRUE
  )

  colnames(fcpval) = mycolnames
  rownames(fcpval) = colnames(logfc[, -1])
  
  return(fcpval)
}

#' This function returns a transformed data frame of character type to a data frame of factor type
#'
#' @param datach a data frames
#'
#' @return \datach a data frame 
#' 
#' @export


chartofa = function(datach){
  
  datach[] <- lapply( datach, factor)
  col_names <- names(datach)
  datach[col_names] <- lapply(datach[col_names] , factor)
  
  return(datach)
}


#' This function returns a data frame of the significant genes associated with the corresponding cluster index
#'
#' @param cut02 a heatmap object
#' @param ind a vector of integer
#' @param signws a matrix 
#' @param pval a data frame
#'
#' @return a data frame
#' @export
#'


heatmtoclust = function( hmp01_All, exprData, pval, height= 5){
  
  cut02 = cut(hmp01_All$rowDendrogram, h = height )
  
  
  HCgroupsLab = lapply(cut02$lower, function(x)
    labels(x))
  

  final = exprData[rev(hmp01_All$rowInd), hmp01_All$colInd]
  
  my_last= as.integer(lapply(seq(length(HCgroupsLab)), function(x)
  {return(tail(HCgroupsLab[[x]],1))}))
  
  mygen = as.integer(row.names(final))
  
  heatmclust = pval %>%
    dplyr::select (X,ProbeName,GeneName) %>%
    filter( X %in% mygen) %>%
    left_join(data.frame(X=mygen), . , by="X") %>%
    arrange(-row_number())
  
  i = 1
  for(row in 1:nrow(heatmclust)){
    if(heatmclust$X[row] == my_last[i] ){
      heatmclust$cluster[row] = i
      i = i+1
    }
    else
      heatmclust$cluster[row] = i
  }
  
  return(heatmclust)
  
}


#https://stackoverflow.com/questions/9063889/how-to-round-a-data-frame-in-r-that-contains-some-character-variables
round_df <- function(df, digits) {
  
  df = lapply(1:NROW(df), function(x){
  nums <- vapply(df[[x]], is.numeric, FUN.VALUE = logical(1))
  df[[x]][,nums] <- round(df[[x]][,nums], digits = digits)
  })
  return(df)
}

## https://www.r-bloggers.com/correctly-reporting-p-values-in-summary-tables-reported-with-xtable/
fixp <- function(x, dig=3){
  #x <- as.data.frame(x)
  if(substr(names(x)[ncol(x)],1,2) != "Pr")
    warning("The name of the last column didn't start with Pr. This may indicate that p-values weren't in the last row, and thus, that this function is inappropriate.")
  x[,ncol(x)] <- round(x[,ncol(x)], dig)
  for(i in 1:nrow(x)){
    if(x[i,ncol(x)] == 0)
      x[i,ncol(x)] <- paste0("< .", paste0(rep(0,dig-1), collapse=""), "1")
  }
  
  x
}



# 
# test <- sessionInfo()
# final <- cbind(unlist(lapply(names(test$otherPkgs),
#                       function(x)
#                         return(
#                           paste(test$otherPkgs[[x]]$Package, test$otherPkgs[[x]]$Version)
#                         ))),
#                unlist(lapply(names(test$otherPkgs), function(x)
#                  return(paste(test$otherPkgs[[x]]$Title)))))%>%
#   as.data.frame()
# colnames(final) = c('version', "definition")
# View(final)
# final$version
# 
# test = (cbind(unlist(final$version),unlist(final$definition))) %>% as.data.frame()
# colnames(test) = c('version', "definition")
# 
# 
# View(test)
# typeof(test$definition)
# typeof(pval$X)
#require(goseq)
#require(GO.db)
library(dplyr)


gosearch <- function(hm01, species, ids, clusterlist) {
  #clusterlist = NULL
  library(goseq)
  library(GO.db)
	
  for (i in 1:NROW(unique(hm01$cluster))) {
    genlist <- hm01[!duplicated(hm01$GeneName),]
    genlist <-genlist %>% dplyr::select(cluster, GeneName)   %>% filter(cluster == i)
    final = as.double(matrix(1, length(genlist$cluster)))
    names(final) = (genlist$GeneName)
    
    h <- function(w)
        if (any(grepl("constraints|library", w)))
          invokeRestart("muffleWarning")
    
    e <- function(y)
      if(any(grepl("Rplots.pdf", y)))
        invokeRestart("muffleWarning")

    pwf <- tryCatch({
      withCallingHandlers(nullp(final, species, ids ,plot.fit=FALSE), warning = h, error = e) %>% na.omit()
    }, warning = function(e) {
      warning("40 % of genes are misssing")
      #return(enrichment_empty())
      return(NULL)
    })
    
    #pwf <- nullp(final, species, ids,plot.fit=FALSE) %>% na.omit()
    cat(length(row.names(pwf)))
    
    if (!is.null(pwf)) {
      finalons <- goseq(pwf, species , ids, use_genes_without_cat = F)
      clusterlist[[i]] = filter(finalons, numInCat > 1 ) %>%
        arrange(desc(numInCat))
    }
    else
      clusterlist[[i]] = NULL
  }
  return(clusterlist)
}


wclust <- function(clusterlist, filename, min, top)  {
  cat("Clustering", file = filename)
  sink(filename)
  sink()
  con <- file(filename, "w")
  for (i in 1:NROW(clusterlist)) {
    if (!i == 1)
      cat("--------------------------------------\n", file = con)
    cat(paste("cluster:", i),  file = con)
    if (!i == 1)
      cat("\n--------------------------------------\n", file = con)
    cat("\n--------------------------------------\n", file = con)
    if (!length(clusterlist[[1]][[i]]) == 0) {
      for (go in top:min) {
        cat(paste("GOID:", as.character(GOID(
          clusterlist[[i]][[1]][[go]]
        ))), file = con)
        cat("\n", file = con)
        cat(paste("Term:", as.character(Term(
          clusterlist[[i]][[1]][[go]]
        ))), file = con)
        cat("\n", file = con)
        cat(paste("Definition:", as.character(Definition(
          clusterlist[[i]][[1]][[go]]
        ))), file = con)
        cat("\n", file = con)
        cat("--------------------------------------\n", file = con)
      }
      cat("\n", file = con)
    }
  }
  
  close(con)
}


probnamtoentrez <- function(hm01,  mypack) {
  
  lapply(1:NROW(unique((hm01$cluster))), function(x) {
    entrezids <- hm01 %>%
      filter(cluster == x) %>%
      dplyr::select(GeneName) %>%
      unlist() %>%
      as.character() %>%
      mget(x = .,envir = mypack,ifnotfound = NA) %>%
      unlist() %>%
      unique() %>%
      .[!is.na(.)]
    
    return(entrezids)
  })
}

probnamtoentrezvenn <- function(venngenes, mypack){
  
  entrezids <- venngenes %>%
    unlist() %>%
    as.character() %>%
    mget(x = .,envir = mypack,ifnotfound = NA) %>%
    unlist() %>%
    unique() %>%
    .[!is.na(.)]
  
}


entreztosymb <- function(myentz, mypack){
lapply(1:NROW(myentz), function(x)
  as.vector(unlist(mget(myentz[[x]], envir=mypack, ifnotfound=NA))))
}


davidquery <- function(entrezids, species, mycat) {
  test = lapply(1:NROW(entrezids), function(x) {
    david <- DAVIDWebService$new(email = "franck.soubes@inra.fr", url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
    RDAVIDWebService::setTimeOut(david, 90000)
    result <-
      addList(
        david,
        entrezids[[x]],
        idType = "ENTREZ_GENE_ID",
        listName = "testList",
        listType = "Gene"
      )
    
    selectedSpecie = (species)
    #backgroundLocation = grep(selectedSpecie,RDAVIDWebService::getBackgroundListNames(david))
    specieLocation = grep(selectedSpecie, RDAVIDWebService::getSpecieNames(david))
    setCurrentSpecies(object = david, species = specieLocation)
    #setCurrentBackgroundPosition(object = david, position = backgroundLocation)
    #getSpecieNames(david)
    setAnnotationCategories(david, mycat) #c("GOTERM_MF_ALL", "GOTERM_CC_ALL", "GOTERM_BP_ALL")) # "KEGG_PATHWAY"
    mydav = as.data.frame(cbind(getFunctionalAnnotationChart(object=david, threshold=1, count=0L)))  %>%
      filter(Count>1) %>% arrange(desc(Count))  %>% select( Category:Count, List.Total:Pop.Total,X.,PValue,Genes,Fold.Enrichment, Bonferroni, Benjamini)
    colnames(mydav)[[7]] = "percent"
    return(mydav)
  })
}


davidqueryvenn <- function(entrezids, species){
  
  david <- DAVIDWebService$new(email = "franck.soubes@inra.fr", url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
  RDAVIDWebService::setTimeOut(david, 90000)
  
  addList(
    david,
    entrezids,
    idType = "ENTREZ_GENE_ID",
    listName = "myqueryvenn",
    listType = "Gene"
  )
  
  selectedSpecie = (species)
  #backgroundLocation = grep(selectedSpecie,RDAVIDWebService::getBackgroundListNames(david))
  specieLocation = grep(selectedSpecie, RDAVIDWebService::getSpecieNames(david))
  setCurrentSpecies(object = david, species = specieLocation)
  
  # get the cluster report for the upload
  getClusterReport(david, type = "Term")
  
}

  
# Functional Annotation Clustering: new!
# Due to the redundant nature of annotations, Functional Annotation Chart presents similar/relevant annotations repeatedly. 
# It dilutes the focus of the biology in the report.  To reduce the redundancy, the newly developed Functional Annotation Clustering report groups/displays similar annotations together which makes the biology clearer and more focused to be read vs. traditional chart report. 
# The grouping algorithm is based on the hypothesis that similar annotations should have similar gene members.  
# The Functional Annotation Clustering integrates the same techniques of  Kappa statistics to measure the degree of the common genes between two annotations, and  fuzzy heuristic clustering (used in Gene Functional Classification Tool ) to classify the groups of similar annotations according kappa values. 
# In this sense, the more common genes annotations share, the higher chance they will be grouped together.
# The p-values associated with each annotation terms inside each clusters are exactly the same meaning/values as p-values (Fisher Exact/EASE Score) shown in the regular chart report for the same terms.
# The Group Enrichment Score new! , the geometric mean (in -log scale) of member's p-values in a corresponding annotation cluster, is used to rank their biological significance. 
# Thus, the top ranked annotation groups most likely have consistent lower p-values for their annotation members.







require(dplyr)
require(RColorBrewer)



## Add jackknife correlation good for false positives with the pval methods add citation ....


#' distcos is a function that computes the cosine similarity for a given matrix
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

distcos <- function(x){ #Jonathan Chang
  as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2))))) 
}



#' distcor is a function that computes the pearson correlation 
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

distcor<-function(x) {as.dist(1-cor(t(x),use="pairwise.complete.obs"))}


#' disteuc is a function that computes the euclidean distance matrices
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

disteucl<-function(x) {dist(x,method="euclidian")}


#' hclustfun is a function that performs a hiearchical cluster analysis with the method of Ward
#'
#' @param d 
#'
#' @return an object of class hclust
#' 
#' @export

hclustfun=function(d) {hclust(d,method="ward.D2")} 

#require("Biobase")  
require("marray") 


#' num2cols is a function that gives a character vector of color names for a given numeric vector
#'
#' @param numVector a numeric vector
#' @param colp a character vector
#'
#' @return a matrix object 
#' 
#' @export

num2cols=function(numVector,colp=palette()){
  
  gpcol=as.data.frame(numVector)
  numCols=length(unique(gpcol[,1]))
  mycol <- sort(unique(gpcol$numVector))
  if(length(colp) <numCols) warning("number of color names < number of levels! Give a color palette with at least ",numCols," terms to 'col' argument")
  cols=as.data.frame(matrix(c(mycol,colp[1:(numCols)]),nrow=numCols)) ## couleurs pour les groupes
  myColFun=function(x){
    as.character(cols[cols[,1]==x[1],2])
  }
  apply(gpcol,1,FUN=myColFun)
  
}



############################################################################
##   plotHeatmaps() function                                              
############################################################################ 


#' hmbis is an event reactive function that pre-computed hierarchical clustering on microarray data
#'
#' @param exprData  data frame with all the individuals selected
#' @param geneSet  data frame with the indexes corresponding to the sigificant genes
#' @param groups  data frame with the corresponding groups 
#' @param workingPath  access path
#' @param k  numeric value which aim is to defined the treshold value to cut the dendogram
#' @param fileType  character value which extension corresponds to different file types
#' @param colOrder  positive numbers, used as cex.axis in for the row or column axis labeling
#' @param na.color color used for missing values
#' @param hclustGenes  function used to compute the hierarchical clustering when Rowv or Colv are not dendrograms
#' @param meanGrp  boolean value to computes the mean for each groups; default = F
#' @param plotRowSideColor  boolean value that colors or not the rows side
#' @param RowSideColor  character vector of length nrow containing the color names for a vertical side bar that may be used to annotate the rows of x.
#' @param Rowdistfun  function used to compute the distance for the rows
#' @param Coldistfun  function used to compute the distance for the columns
#' @param palette.col NULL
#' @param notplot  boolean
#'
#' @return list of objects which aim is to being passed as argument in the plotHeatmaps function
#' 
#' @export
#' 


truncatedhat=function(exprData,geneSet,groups,workingPath=getwd(),k=3,fileType="png",
                      colOrder=NULL,na.color="black",hclustGenes=T,meanGrp=F,plotRowSideColor=T,mypal=NULL,
                      RowSideColor=c("gray25","gray75"), Rowdistfun="correlation",Coldistfun="correlation" ,palette.col=NULL , notplot = T,genename=pval){
  
  
  
  
  if( any(rownames(exprData) != rownames(exprData)[order(as.numeric(rownames(exprData)))])) stop("Error: 'exprData' must have rownames in numerical ascending order!");
  if(length(RowSideColor)==1) RowSideColor=gray.colors(k, start = 0.2, end = 0.9)
  if(!Rowdistfun %in% c("correlation","euclidian")) stop("Rowdistfun must be one of 'cor' or 'euclidian'!")
  if(!Coldistfun %in% c("correlation","euclidian")) stop("Coldistfun must be one of 'cor' or 'euclidian'!")
  
  #library(gplots)
  #library(marray)
  
  
  
  if(is.null(mypal))
    mypal = brewer.pal(8,"Dark2") %>%
      list(brewer.pal(10,"Paired")) %>%
      unlist()
  
  if(!is.null(palette.col)){
    palette(palette.col);
  }
  else  
    palette(mypal)
  
  cl=palette(mypal);
  
  
  exprData=exprData[geneSet,]
  
  ##-----------------------##
  ## Row dendrogram
  ##-----------------------##
  cat("\n -> Hierarchical clustering on genes... \n")
  if(Rowdistfun=="correlation"){	
    hc=hclustfun(distcor(exprData))
    subdist="dist method: 1-cor";
    distfunTRIX= distcor;
  } 
  if(Rowdistfun=="euclidian"){
    hc=hclustfun(disteucl(exprData))
    subdist="dist method: euclidian";
    distfunTRIX = disteucl;
  }
  
  rowv=as.dendrogram(hc)
  if(meanGrp){
    cat("\n -> calculating groups means... \n")
    exprData=t(apply(exprData,1,FUN=function(x){tapply(x,groups,mean,na.rm=T)}))
    cat("    Done \n")
    groups=factor(levels(groups),levels=levels(groups))
    
  }
  
  ##**********
  ## Rownames
  
  ##-----------------------##
  ## Col dendrogram
  ##-----------------------##
  
  gpcol=num2cols(as.numeric(groups))
  
  
  ##**********
  ## RowDendrogram
  
  # build dendrogram
  
  
  if(Coldistfun=="correlation")
    ColvOrd = exprData %>%
    t() %>%
    distcor()%>%
    hclustfun()%>%
    as.dendrogram()
  
  if(Coldistfun=="euclidian")
    ColvOrd = exprData %>%
    t() %>%
    disteucl()%>%
    hclustfun()%>%
    as.dendrogram()
  
  # re-order dendrogram
  
  if(!is.null(colOrder)){ # reorder col dendrogram
    if(length(colOrder)==ncol(exprData)){
      ord=colOrder	#vector of order values
    } else ord=1:ncol(exprData); # TRUE: create default 1:ncol
    ColvOrd=reorder(ColvOrd,ord,agglo.FUN = mean) # reorder by mean values
  }else {ColvOrd=ColvOrd} # no reordering
  
  
  #### heatmap  genes 
  
  #useRasterTF=F;
  
  ##-----------------------##
  ## plot Row dendrogram
  ##-----------------------##
  
  if(hclustGenes){
    cat("\n -> Plotting Dendrogram... \n")

    plot(hc,hang=-1,labels=FALSE,sub=paste("hclust method: ward2\n", subdist),xlab="",main="")
    hcgp=rect.hclust(hc,k=k,border="red")

    #hts=rev( tail( hc$height,15))
    # bp=barplot(hts,names.arg=1:length(hts))
    # text(x=bp,y=hts,label= formatC(hts,1,format="f"),pos=3,cex=0.8) 
    # dev.off()
    myheight = rev( tail( hc$height,15))
    
    cat("    Done \n")
  }
  
  ##-----------------------##
  ## plotRowSideColor
  ##-----------------------##
  
  if(plotRowSideColor){
    if(!hclustGenes){
      
      plot(hc,hang=-1,labels=FALSE,xlab="",main="")
      hcgp=rect.hclust(hc,k=k,border="red")

    }
    if(length(RowSideColor) == length(geneSet)){
      gpcolr=RowSideColor;
    }else {
      if(length(RowSideColor) != length(geneSet)){
        gphcc=as.data.frame(matrix( c(hc$labels[hc$order], rep(1:k,times=sapply(hcgp,length))),nrow=length(hc$labels)),stringsAsFactors=F)
        colnames(gphcc)=c("probe","cluster")
        gphccOrd=gphcc[order(as.numeric(gphcc[,1])),]
        hcgp=factor(paste("c",gphccOrd[,2],sep=""),levels=paste("c",rep(1:k),sep=""))
        gpcolr=num2cols(as.numeric(hcgp),c(rep(RowSideColor,20)[1:(k)]))
        print(RowSideColor)
      }else gpcolr=NULL
    }
  }else gpcolr=rep("white",nrow(exprData))
  
  rowIds = genename$GeneName[geneSet]
  
  
  #print(myheight[[k]])  
  objforheat = list(exprData,distfunTRIX,ColvOrd,rowv,gpcol,gpcolr,rowIds,myheight[[k]])
  
  
  return(objforheat)
}


#' plotHeatmaps is a function that creates a false color image with a dendogram added to the left side and the top
#'
#' @param exprData  data frame with all the individuals selected
#' @param geneSet  data frame with the indexes corresponding to the sigificant genes
#' @param groups  data frame with the corresponding groups 
#' @param workingPath an access path
#' @param k  numeric value which aim is to defined the treshold value to cut the dendogram
#' @param fileType  character value which extension corresponds to different file types
#' @param cexcol   positive numbers, used as cex.axis in for the row or column axis labeling
#' @param cexrow  positive numbers, used as cex.axis in for the row or column axis labeling
#' @param colOrder  positive numbers, used as cex.axis in for the row or column axis labeling
#' @param labrow  character vectors with row and column labels to use
#' @param na.color color to use for missing value 
#' @param scale  character indicating if the values should be centered and scaled in either the row direction or the column direction, or none
#' @param rowv  dendogram object
#' @param ColOrd  positive numbers, used as cex.axis in for the row or column axis labeling
#' @param gpcol  matrix with colors associated to each groups 
#' @param gpcolr  matrix with gray color depending on the clusters
#' @param distfunTRIX function that computes whether euclidian or pearson for Hierarchical Clustering
#' @param RowSideColor a character vector of length nrow containing the color names for a vertical side bar that may be used to annotate the rows of x.
#' @param palette.col NULL
#' @param margins  numeric vector of length 2 containing the margins (see par(mar= *)) for column and row names, respectively.
#' @param my_palette a character vetor of length 17
#' @param mycex  numeric value which aim is to change the size of the legend
#' @param mypal  character vetor of length 17 
#' @param colid  character vectors with row and column labels to use; default NULL
#' @param showcol  boolean value used to hide or show the colnames 
#' @param showrow  boolean value used to hide or show the rownames
#' @param genename  data frame list corresponding to the gene names 
#' @param notplot  boolean calling or not the dev.off method
#'
#' @return  data frame with the cluster and the corresponding genes 
#' 
#' @export

plotHeatmaps=function(exprData,groups,workingPath=getwd(),fileType="png",cexcol=1.5,cexrow=1.5,
                      colOrder=NULL,labrow=F,na.color="black",scale="row", rowv =list4, ColvOrd = list3,
                      gpcol =list5 , gpcolr =list6 , distfunTRIX = list2,
                      RowSideColor=c("gray25","gray75") ,palette.col=NULL, 
                      margins=c(8,8),my_palette=colorRampPalette(c("green", "black", "red"))(n = 75)
                      ,mycex = 0.6,mypal=test,colid = NULL, showcol = T ,showrow =F,
                       notplot = F, geneSet= list7,genename = csvf, height = list8,rastering= myras ){
  
  
  #RowSideColor: color palette to be used for rowSide cluster colors
  # can also be gray.colors(k, start = 0.2, end = 0.9) to get k colors of gray scale

  if(is.null(showcol))
    showcol = F
  
  if(is.null(showrow))
    showrow = F
  
  if(showcol == T)
    colid = NA
  else
    colid = NULL
  
  if(showrow == F)
    rowIds = NA
  else
    rowIds = geneSet
  
  
  
  # mypal =c ("#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442",
  #          "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray",
  #          "burlywood1","darkkhaki", "#CC0000" )
  
  
  if(is.null(mypal))
    mypal = brewer.pal(8,"Dark2") %>%
      list(brewer.pal(10,"Paired")) %>%
      unlist()
  
  if(!is.null(palette.col)){
    palette(palette.col);
  }else  palette(mypal)
  
  cl=palette(mypal);
  
  
  ##-----------------------##
  ## plot Heatmap
  ##-----------------------##
  
  cat("\n -> Plotting HeatMap... \n")
  par("mar")
  
  par(mar=c(5,5,1,1.10))
  
  #pdf(NULL)
  hmp02 = heatmap.2(exprData,na.rm=T,dendrogram="both",labRow = rowIds,labCol=colid,scale=scale, RowSideColors=gpcolr, ColSideColors=gpcol,key=T,
                    keysize=1, symkey=T, trace="none",density.info="density",distfun=distfunTRIX, hclustfun=hclustfun,cexCol=cexcol,
                    Colv=ColvOrd,Rowv=rowv,na.color=na.color,cexRow=cexrow,useRaster=rastering,margins=margins,layout(lmat =rbind(4:3,2:1),lhei = c(0.05,1), 
                                                                                                              lwid = c(0.1,1)),col=my_palette,key.par = list(cex=0.6))
  #mtext(side=3,sort(levels(groups)),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[(1:length(levels(groups)))],cex=mycex,line=-1)
  
  if(notplot)
    dev.off()
  
  cat("    Done \n")
  myfinalobj = list(heatmtoclust(hmp02,exprData,genename,height= height),hmp02)

  #return(heatmtoclust(hmp02,exprData,genename,height= height))

  return(myfinalobj)
}










require(FactoMineR)
require(factoextra)


#' res.pca is a function that computed a PCA of non-normalized data with the FactoMineR package
#'
#' @param workingset a data frame corresponding to the WorkingSet
#' @param scale a boolean; by default this value is set to False non-normalized data
#'
#' @return \PCAres a data frame with PCA attributes
#' 
#' @export

res.pca <- function(workingset, scale = F) {
  
  myt = transpose(workingset)
  row.names(myt) = colnames(workingset)
  
  PCAres = PCA(myt,
               scale.unit = F,
               graph = F)
  
  return(PCAres)
}


#' eboulis is a function which aim is to display the eigenvalues of the data with the package factoextra
#'
#' @param PCAresa a data frame with PCA attributes
#'
#' @return \p a factoextra object
#' 
#' @export

eboulis <- function(PCAres){
  
  p <- fviz_eig(PCAres, addlabels=TRUE, hjust = -0.3, barfill="white", barcolor ="darkblue", linecolor ="red")
  p + theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),panel.border = element_blank(),
            panel.background = element_blank())
  p + labs(title = "Variances - PCA", x = "Principal Components", y = "% of variances")

  return(p)
}

#' PCAplot is a function that return a factoextra object of PCA type
#'
#' @param PCAres a data frame with PCA attributes
#' @param myax a numeric vector of length 2 specifying the dimensions to be plotted
#' @param elips a boolean value to add ellipse to the data distribution  for the different groups; default = False
#' @param rep a boolean value to avoid overlaps between the label points
#' @param mylevel a data frame corresponding to the pData
#' @param mylabsize a numeric value representing the police size to display for the different labels
#' @param dispelip a numeric value representing the ellipsoid dispersion
#' @param labeled a character to display labels and/or points
#' @param pal a color object from the RcolorBrewer package
#'
#' @return
#' 
#' @export

PCAplot <- function(PCAres, myax = c(1,2), elips = T , rep = T , mylevel = groups$Grp,  mylabsize = 4, dispelip = 0.8 , labeled = 'all', pal = brewer.pal(8, "Dark2")){
  
  
  p <- fviz_mca_ind(PCAres, label= labeled , habillage = mylevel, addEllipses= elips ,
                    ellipse.level= 0.8, repel = rep, axes = myax, pointsize = 2 , labelsize = mylabsize)

  
  return(p + scale_color_manual(values=pal))
}





require(VennDiagram)

#Intersect, Union and Setdiff (https://stackoverflow.com/questions/23559371/how-to-get-the-list-of-items-in-venn-diagram-in-r)

Intersect <- function (x) {  
  # Multiple set version of intersect
  # x is a list
  
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    intersect(x[[1]], x[[2]])
  } else if (length(x) > 2){
    intersect(x[[1]], Intersect(x[-1]))
  }
}

Union <- function (x) {  
  # Multiple set version of union
  # x is a list
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    union(x[[1]], x[[2]])
  } else if (length(x) > 2) {
    union(x[[1]], Union(x[-1]))
  }
}

Setdiff <- function (x, y) {
  # Remove the union of the y's from the common x's. 
  # x and y are lists of characters.
  xx <- Intersect(x)
  yy <- Union(y)
  setdiff(xx, yy)
}



#' Vennlist is a function which aim is to return a list of signficant genes for a treshold pvalue of 5%
#'
#' @param pval a data frame
#' @param adj a data frame with the contrast selected
#' @param fc a data frame with the constrast selected
#' @param regulation a character
#'
#' @return \myl a list
#' 

Vennlist <- function(adj,fc, regulation, cutoffpval, cutofffc){ ## ajout de foreach parallel
  
  if(is.null(adj)) 
    return(NULL)
  
  
  
  reguser = ifelse(regulation == "up", T, F)
  reguserboth = ifelse(regulation == "both", T, F)
  lapply(1:ncol(adj), FUN = function(x){
    if(reguser && !reguserboth)
      return(as.character(which(adj[[x]] < cutoffpval & fc[[x]] > log2(cutofffc))))
    else if(!reguser && !reguserboth)
      return( as.character(which(adj[[x]] < cutoffpval & fc[[x]] < -log2(cutofffc))))
    else
      return(as.character(which(adj[[x]] < cutoffpval & abs(fc[[x]]) > log2(cutofffc))))
  })
  
  #slice(my_data, 1:6)

}



#' Vennfinal is a function which aim is to return an object containing a venn diagram 
#' 
#' @param myl a list of genes for the different contrasts
#' @param adj a data frame 
#' @param cex a vector giving the size for each area label 
#'
#' @return \final draw on the current device
#' 

Vennfinal <- function(myl,adj, cex=1, cutoffpval, cutofffc, statimet, meandup = F, pval){ 
  
  if(is.null(myl))
    return(NULL)
  
  
  if(meandup){
    myl = lapply(seq(length(myl)), function(x){pval %>% select(GeneName, ProbeName) %>% filter( ProbeName %in% myl[[x]]) %>% 
        distinct( GeneName)}) %>%
      as.matrix()
    myl = lapply(1:length(myl),FUN = function(i) as.character(myl[[i]]$GeneName))  #D14Ertd668e doublons pour LWT_MCD.LWT_CTRL (si genesymbol -1 perte d'info) avec une probe nom partagée avec LKO_MCD.LKO_CTRL  et une probe partagée
  
  }
  
  metuse = ifelse(statimet == "FDR","DEG BH ", "DEG RAW ")
  
  indexnull = which( sapply(myl ,length) == 0)
  myl <- myl[sapply(myl, length) > 0]
  final = length(myl)-1
  totgenes =  sum(sapply(myl,length))
  totprobes=  totalvenn(myl, adj)
  mynumb = paste("total probes:", totgenes ,"and total probe crossings:",totprobes, collapse = "")

  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  mytresh = paste0(metuse, cutoffpval, " and FC " , cutofffc)
  
  
  if(length(myl)==2){
     if (length(myl[[2]])> length(myl[[1]]))
       mynames = rev(colnames(adj))
     else
       mynames = colnames(adj)
  }
  else
    mynames = colnames(adj)

  
  
  if(length(indexnull)>0){
    if(length(myl)==5){
      print(colnames(adj[,-c(indexnull)]))
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1, cat.just= list(c(0.6,1) , c(0,0) , c(0,0) , c(1,1) , c(1,0)),
                   category.names = mynames[,-c(indexnull)],fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                   fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
    }
    else{
      print(colnames(adj[,-c(indexnull)]))
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,
                     category.names = mynames[,-c(indexnull)],fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                     fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
    }
  }
  else{
      if(length(myl)==5){
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,cat.just=  list(c(0.6,1) , c(0,0) , c(0,0) , c(1,1) , c(1,0)) ,
                     category.names = mynames,fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                     fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop4
      }
      else{

        g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,
                         category.names = mynames,fill = 2:(2+final), alpha = 0.3, sub=mynumb, cex=1, 
                         fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
      }
  }
  
  final = grid.arrange(gTree(children=g), top="Venn Diagram", bottom= mytresh)
  
  
  
  return(final)
}


Vennsev <- function(myl, adj){
  
  myl <- myl[sapply(myl, length) > 0]
  final = length(myl)-1
  tot = sum(sapply(myl,length))
  mynumb = paste("total genes", tot , collapse = ":")
  g = venn(myven, ilabels= F, zcolor ="style", sname = colnames(adj), cexil = 0.5, size = 5, cexsn = 0.5)
  return(g)
}

#' myventocsv is a function that create a csv file of the signficant genes for the different contrasts for a cutoff of 5%
#'
#' @param myven a list of genes for the different contrasts
#' @param adj a data frame
#'
#' @return
#' 

myventocsv <- function(myven, adj){
  
  
  max.length <- max(sapply(myven, length))
  myven %>%
    lapply(function(v){ c(v, rep("", max.length-length(v)))}) %>%
    setNames(names(adj)) %>%
    as.data.frame()

}

totalvenn <- function(vennlist,adj){
  
  names(vennlist) = colnames(adj)
  elements <- 1:length(vennlist) %>% lapply(function(x)
      combn(names(vennlist), x, simplify = FALSE)) %>%
    unlist(recursive = F) %>% setNames(., sapply(., function(p)
      paste0(p, collapse = ""))) %>%
    lapply(function(i)
      Setdiff(vennlist[i], vennlist[setdiff(names(vennlist), i)])) %>% .[sapply(., length) > 0]
  
  n.elements <- sapply(elements, length)

  
  return(sum(n.elements))
}


setvglobalvenn <- function(vennlist,adj){
  
  names(vennlist) = colnames(adj)
  elements <- 1:length(vennlist) %>% lapply(function(x)
    combn(names(vennlist), x, simplify = FALSE)) %>%
    unlist(recursive = F) %>% setNames(., sapply(., function(p)
      paste0(p, collapse = ""))) %>%
    lapply(function(i)
      Setdiff(vennlist[i], vennlist[setdiff(names(vennlist), i)])) %>% .[sapply(., length) > 0]
  
  
  return(elements)
}



rowtoprob <- function(myven,pval,adj) {
  
  pval$rownames = rownames(pval)
  names(myven) = colnames(adj)
  final = lapply(
    names(myven),
    FUN = function(x) {
      test = pval[pval$rownames %in% myven[[x]],]
      
      what <- test %>%
        select(ProbeName) %>%
        unlist() %>%
        as.character()
      
      return(what)
    }
  )
}



topngenes <- function(dfinter, mycont, inputtop, meandup = F) {
  
  
  if(!meandup)
    dfinter$GeneName = make.names(dfinter$GeneName, unique = T)

  
  mycont = gsub("-"," vs logFC_" ,mycont)
  colnames(dfinter)= lapply(colnames(dfinter),function(x){
    
    if(grepl("-",x))
      x = gsub("-"," vs logFC_" ,x)
    
    return(x)})
  

  
  reshp <-melt(
    dfinter[1:inputtop, ],
    id.vars = "GeneName",
    measure.vars = c (mycont),
    variable.name = "Comparisons",
    value.name = "logFC"
  )
  
  reshp <- droplevels(reshp)
  reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))
  
  # maxval = as.numeric(max(reshp$logFC))
  # minval = as.numeric(min(reshp$logFC))
  # print(c(minval-2,maxval+2))
  
  
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
        size = 11,
        colour = "#808080",
        angle = 80,
        hjust = 1
      ),
      axis.text.y = element_text(size = 8, colour = "#808080"),
      legend.position="top"
    ) 
    
  #print(unique(sort(c(seq(as.numeric(maxval)),seq(as.numeric(minval))))))
  
  print(p)
  
  return( p)
  
}




