### Author: Yannick Lippi and adapted to shiny by Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0



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

hclustfun=function(d,e = "ward.D2") {hclust(d,method=e)} 

#' disteuc is a function that computes the euclidean distance matrices
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

distman<-function(x) {dist(x,method="manhattan")}

#require("Biobase")  
#require("marray") 


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


truncatedhat=function(exprData,geneSet,groups,workingPath=getwd(),k=3,fileType="png",algo = "ward.D2",
                      colOrder=NULL,na.color="black",hclustGenes=T,meanGrp=F,plotRowSideColor=T,mypal=NULL,
                      RowSideColor=c("gray25","gray75"), Rowdistfun="correlation",Coldistfun="correlation" ,palette.col=NULL , notplot = T,genename=pval){
  
  
  
  
  if( any(rownames(exprData) != rownames(exprData)[order(as.numeric(rownames(exprData)))])) stop("Error: 'exprData' must have rownames in numerical ascending order!");
  if(length(RowSideColor)==1) RowSideColor=gray.colors(k, start = 0.2, end = 0.9)
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
  rownames(exprData) = rownames(genename)
  exprData=exprData[geneSet,]
  
  
  
  ##-----------------------##
  ## Row dendrogram
  ##-----------------------##
  cat("\n -> Hierarchical clustering on genes... \n")
  if(Rowdistfun=="correlation"){	
    hc=hclustfun(distcor(exprData),algo)
    subdist="dist method: 1-cor";
    distfunTRIX= distcor;
  } 
  if(Rowdistfun=="euclidian"){
    hc=hclustfun(disteucl(exprData),algo)
    subdist="dist method: euclidian";
    distfunTRIX = disteucl;
  }
  if(Rowdistfun=="manhattan"){
    hc=hclustfun(disteucl(exprData),algo)
    subdist="dist method: manhattan";
    distfunTRIX = distman;
  }
  if(Rowdistfun=="cosine"){
    hc=hclustfun(disteucl(exprData),algo)
    subdist="dist method: cosine";
    distfunTRIX = distcos;
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
  
  if(Coldistfun=="manhattan")
    ColvOrd = exprData %>%
    t() %>%
    distman()%>%
    hclustfun()%>%
    as.dendrogram()
  
  
  if(Coldistfun=="cosine")
    ColvOrd = exprData %>%
    t() %>%
    distcos()%>%
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
    myheight <- rev( tail( hc$height,15))
    
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
  hmp02 = heatmap.2(exprData,na.rm=T,dendrogram="both",labRow = rowIds,labCol=colid,scale=scale, RowSideColors=gpcolr, ColSideColors=gpcol,key=T,
                    keysize=1, symkey=T, trace="none",density.info="density",distfun=distfunTRIX, hclustfun=hclustfun,cexCol=cexcol,
                    Colv=ColvOrd,Rowv=rowv,na.color=na.color,cexRow=cexrow,useRaster=rastering,margins=margins,layout(lmat =rbind(4:3,2:1),lhei = c(0.05,1), 
                                                                                                              lwid = c(0.1,1)),col=my_palette,key.par = list(cex=0.6))
  mtext(side=3,sort(levels(groups)),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[(1:length(levels(groups)))],cex=mycex,line=-1)
  
  if(notplot)
    dev.off()
  
  cat("    Done \n")

  restoshiny = list(heatmtoclust(hmp02,exprData,genename,height= height),hmp02)
  return(restoshiny)
}










