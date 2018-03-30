distcor<-function(x) {as.dist(1-cor(t(x),use="pairwise.complete.obs"))}
disteucl<-function(x) {dist(x,method="euclidian")}
hclustfun=function(d) {hclust(d,method="ward.D2")} 

require("Biobase")  
require("marray") 

##jpeg(".tmp"))
#  palette(c("black", "blue", "cyan", "magenta",   "darkgray", "darkgoldenrod", "violet",  "orange", "lightgreen","lightblue", "darkorchid", "darkred","darkslateblue", "darkslategray", "maroon", "burlywood1" , "darkolivegreen"));

palette(c("#000000", "#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442", "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray", "burlywood1","darkkhaki", "#CC0000" ));

#dev.off()
###unlink(".tmp")(".tmp")(".tmp")

num2cols=function(numVector,colp=palette()){
  
  # gives a character vector of color names for a given numeric vector
  
  gpcol=as.data.frame(numVector)
  
  numCols=length(unique(gpcol[,1]))
  mycol <- sort(unique(gpcol$numVector))
  if(length(colp)-1<numCols) warning("number of color names < number of levels! Give a color palette with at least ",numCols," terms to 'col' argument")
  cols=as.data.frame(matrix(c(mycol,colp[2:(numCols+1)]),nrow=numCols))
  
  myColFun=function(x){
    as.character(cols[cols[,1]==x[1],2])
    
  }
  apply(gpcol,1,FUN=myColFun)
  
}


############################################################################
##   plotHeatmaps() function
############################################################################ 

plotHeatmaps=function(exprData,geneSet,groups,workingPath=getwd(),prefix,suffix,k=2,fileType="png",cexcol=0.9,cexrow=0.4,
                      colOrder=NULL,labrow=F,colid=NULL,na.color="black",scale="row",hclustGenes=T,meanGrp=F,plotRowSideColor=T,col.hm=greenred(75),
                      RowSideColor=c("gray25","gray75"), Rowdistfun="cor",Coldistfun="cor" ,palette.col=NULL, margins=c(8,8), ...){
  #RowSideColor: color palette to be used for rowSide cluster colors
  # can also be gray.colors(k, start = 0.2, end = 0.9) to get k colors of gray scale

  if(!is.null(palette.col)){
    palette(palette.col);
    #		}else palette(c("black", "blue", "cyan", "magenta",   "darkgray", "darkgoldenrod", "violet",  "orange", "lightgreen","lightblue", "darkorchid", "darkred","darkslateblue", "darkslategray", "maroon", "burlywood1" , "darkolivegreen"));
  }else   palette(c("#000000", "#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442", "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray", "burlywood1","darkkhaki", "#CC0000" ));

  
  if(fileType %in% c("emf","eps","svg") ) require("devEMF") 
  
  if( any(rownames(exprData) != rownames(exprData)[order(as.numeric(rownames(exprData)))])) stop("Error: 'exprData' must have rownames in numerical ascending order!");
  if(length(RowSideColor)==1) RowSideColor=gray.colors(k, start = 0.2, end = 0.9)
  if(!Rowdistfun %in% c("cor","euclidian")) stop("Rowdistfun must be one of 'cor' or 'euclidian'!")
  if(!Coldistfun %in% c("cor","euclidian")) stop("Coldistfun must be one of 'cor' or 'euclidian'!")
  
  library(gplots)
  library(marray)
  cl=palette();
    
  
  ftype=c("tiff","emf","png","eps","svg")
  if(!fileType%in%ftype) stop("Error: file type must be tiff, emf, png or eps");
  exprData=exprData[geneSet,]
  
  #colid=NULL;
  #	rowv=TRUE;
  wdt=900;
  wdte=12;
  kcex=0.4;
  
  ##-----------------------##
  ## Row dendrogram
  ##-----------------------##
  cat("\n -> Hierarchical clustering on genes... \n")
  if(Rowdistfun=="cor"){	
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
    wdt=600;
    wdte=8;
    kcex=0.4;
    colid=NA
  }
  
  ##**********
  ## Rownames
  rowIds=NA;
  if(length(labrow)>1){
    rowIds=labrow[geneSet]
  }else if(labrow) rowIds=NULL;
  
  ##-----------------------##
  ## Col dendrogram
  ##-----------------------##
  
  gpcol=num2cols(as.numeric(groups))
  print(gpcol)
  
  ##**********
  ## RowDendrogram
  
  # build dendrogram
  if(Coldistfun=="cor") ColvOrd=as.dendrogram(hclustfun(distcor(t(exprData))))
  if(Coldistfun=="euclidian") ColvOrd=as.dendrogram(hclustfun(disteucl(t(exprData))))
  
  # re-order dendrogram
  if(!is.null(colOrder)){ # reorder col dendrogram
    if(length(colOrder)==ncol(exprData)){
      ord=colOrder	#vector of order values
    } else ord=1:ncol(exprData); # TRUE: create default 1:ncol
    ColvOrd=reorder(ColvOrd,ord,agglo.FUN = mean) # reorder by mean values
  }else {ColvOrd=ColvOrd} # no reordering
  
  #	if(!is.null(colOrder)){ # reorder col dendrogram
  #	  if(length(colOrder)==ncol(exprData)){
  #	   ord=colOrder
  #	  } else ord=1:ncol(exprData);
  #		if(Coldistfun=="cor"){	
  #		  ColvOrd=reorder(as.dendrogram(hclustfun(distcor(t(exprData)))),ord,agglo.FUN = mean)
  #		} else ColvOrd=reorder(as.dendrogram(hclustfun(dist(t(exprData), method=distfun))),ord,agglo.FUN = mean)
  
  #	}else ColvOrd=T;
  
  #### heatmap  genes 
  useRasterTF=F;
  
  ##-----------------------##
  ## plot Row dendrogram
  ##-----------------------##
  
  if(hclustGenes){
    cat("\n -> Plotting Dendrogram... \n")
    #		hc02=as.hclust(hmp02$rowDendrogram)
    #png(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_hclustGenes.png",sep="")),width=800, height=400)
    plot(hc,hang=-1,labels=FALSE,sub=paste("hclust method: ward2\n", subdist),xlab="",main="")
    hcgp=rect.hclust(hc,k=k,border="red")

    
    #png(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_hclustGenes_heights.png",sep="")),width=800, height=400)
    hts=rev( tail( hc$height,15))
    bp=barplot(hts,names.arg=1:length(hts))
    text(x=bp,y=hts,label= formatC(hts,1,format="f"),pos=3,cex=0.8) 
    cat("    Done \n")
  }
  
  ##-----------------------##
  ## plotRowSideColor
  ##-----------------------##
  
  if(plotRowSideColor){
    if(!hclustGenes){
      png(".tmp")
      plot(hc,hang=-1,labels=FALSE,xlab="",main="")
      hcgp=rect.hclust(hc,k=k,border="red")
      file.remove(".tmp")
    }
    if(length(RowSideColor) == length(geneSet)){
      gpcolr=RowSideColor;
    }else {
      if(length(RowSideColor) != length(geneSet)){
        gphcc=as.data.frame(matrix( c(hc$labels[hc$order], rep(1:k,times=sapply(hcgp,length))),nrow=length(hc$labels)),stringsAsFactors=F)
        colnames(gphcc)=c("probe","cluster")
        gphccOrd=gphcc[order(as.numeric(gphcc[,1])),]
        hcgp=factor(paste("c",gphccOrd[,2],sep=""),levels=paste("c",rep(1:k),sep=""))
        gpcolr=num2cols(as.numeric(hcgp),c("black",rep(RowSideColor,20)[1:(k)]))			
      }else gpcolr=NULL
    }
  }else gpcolr=rep("white",nrow(exprData))
  
  
  ##-----------------------##
  ## plot Heatmap
  ##-----------------------##
  cat("\n -> Plotting HeatMap... \n")
  # if(fileType=="tiff")
  # 	tiff(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,".tiff",sep="")),width=wdt, height=900,compression="jpeg")
  # if(fileType=="png")
  # 	png(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,".png",sep="")),width=wdt, height=900)
  # if(fileType=="emf"){
  # 	emf(file = file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,".emf",sep="")), width = wdte, height = 12,
  # 	bg = "transparent", fg = "black", pointsize = 12, custom.lty = FALSE)
  # 	useRasterTF=F;
  # }
  # if(fileType=="eps"){
  # 	postscript(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,".eps",sep="")),  width = wdte, height = 12, horizontal=F,
  # 	bg = "transparent", pointsize = 12)
  # }
  # if(fileType=="svg"){
  # 	svg(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,".svg",sep="")),  width = wdte, height = 12,
  # 	bg = "transparent", pointsize = 12)
  # }
  
  
  
  #	png(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,".png",sep="")),width=wdt, height=900) 

  par("mar")
  par(mar=c(5,5,1,1.10))
  
  #par("mar")
  #par(mar=c(1,1,1,1))
  #par(mfrow=c(4,2))
  # hmp02=heatmap.2(exprData,na.rm=T, col=col.hm,dendrogram="both",labRow =rowIds,labCol=colid,scale=scale, ColSideColors=gpcol,RowSideColors=gpcolr, key=TRUE,
  # 	keysize=kcex, symkey=FALSE, trace="none",density.info="density",distfun=distfunTRIX, hclustfun=hclustfun,cexCol=cexcol,
  # 	Colv=ColvOrd,Rowv=rowv,na.color=na.color,cexRow=cexrow,useRaster=useRasterTF,margins=margins)
  #	mtext(side=3,levels(groups),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[2:(length(levels(groups))+1)],cex=1,line=-1)
  #	mtext(side=3,levels(groups),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[(1:length(levels(groups)))+1],cex=1,line=-1)
  hmp02 = heatmap.2(exprData,na.rm=T, col=col.hm,dendrogram="both",labRow =rowIds,labCol=colid,scale=scale, RowSideColors=gpcolr, ColSideColors=gpcol,key=T,
                    keysize=1, symkey=T, trace="none",density.info="density",distfun=distfunTRIX, hclustfun=hclustfun,cexCol=cexcol,
                    Colv=ColvOrd,Rowv=rowv,na.color=na.color,cexRow=cexrow,useRaster=useRasterTF,margins=margins,layout(lmat =rbind(4:3,2:1),lhei = c(0.05,1), lwid = c(0.1,1)))
  #	mtext(side=3,levels(groups),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[2:(length(levels(groups))+1)],cex=1,line=-1)
  mtext(side=3,levels(groups),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[(1:length(levels(groups)))+1],cex=0.8,line=-1)
  #dev.off()
  # ColSideColors=gpcol,
  #key.par = list(cex=0.5),
  #key.par=list(mar=c(4,4,4,10))
  #return(hmp02)
  # lhei = c(0.5,5), lwid = c(1,5)
  #key.par=list(mar=c(3,3,1,1),cex=0.5)
}
