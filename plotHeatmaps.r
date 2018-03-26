 distcor<-function(x) {as.dist(1-cor(t(x),use="pairwise.complete.obs"))}
 disteucl<-function(x) {dist(x,method="euclidian")}
 hclustfun=function(d) {hclust(d,method="ward.D2")} 
 
 require("Biobase")  
 require("marray") 

jpeg(".tmp")
#  palette(c("black", "blue", "cyan", "magenta",   "darkgray", "darkgoldenrod", "violet",  "orange", "lightgreen","lightblue", "darkorchid", "darkred","darkslateblue", "darkslategray", "maroon", "burlywood1" , "darkolivegreen"));
  palette(c("#000000", "#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442", "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray", "burlywood1","darkkhaki", "#CC0000" ));
#dev.off()
 unlink(".tmp")

 num2cols=function(numVector,colp=palette()){
   # gives a character vector of color names for a given numeric vector
      gpcol=as.data.frame(numVector)
      numCols=length(unique(gpcol[,1]))
      if(length(colp)-1<numCols) warning("number of color names < number of levels! Give a color palette with at least ",numCols," terms to 'col' argument")
      cols=as.data.frame(matrix(c(1:numCols,colp[2:(numCols+1)]),nrow=numCols));
      myColFun=function(x){
        as.character(cols[cols[,1]==x[1],2])
      }
      apply(gpcol,1,FUN=myColFun)
 }
 
############################################################################
##   plotHeatmaps() function
############################################################################ 
 
plotHeatmaps=function(exprData,geneSet,groups,workingPath=getwd(),prefix,suffix,k=2,fileType="png",cexcol=0.4,cexrow=0.2,
colOrder=NULL,labrow=F,colid=NULL,na.color="black",scale="row",hclustGenes=T,meanGrp=F,plotRowSideColor=T,col.hm=greenred(75),
RowSideColor=c("gray25","gray75"), Rowdistfun="cor",Coldistfun="cor", palette.col=NULL, margins=c(8,8), ...){
#RowSideColor: color palette to be used for rowSide cluster colors
# can also be gray.colors(k, start = 0.2, end = 0.9) to get k colors of gray scale
	jpeg(".tmp")
	if(!is.null(palette.col)){
			palette(palette.col);
#		}else palette(c("black", "blue", "cyan", "magenta",   "darkgray", "darkgoldenrod", "violet",  "orange", "lightgreen","lightblue", "darkorchid", "darkred","darkslateblue", "darkslategray", "maroon", "burlywood1" , "darkolivegreen"));
		}else   palette(c("#000000", "#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442", "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray", "burlywood1","darkkhaki", "#CC0000" ));

#	dev.off()
	unlink(".tmp")
	
	if(fileType %in% c("emf","eps","svg") ) require("devEMF") 

	if( any(rownames(exprData) != rownames(exprData)[order(as.numeric(rownames(exprData)))])) stop("Error: 'exprData' must have rownames in numerical ascending order!");
	if(length(RowSideColor)==1) RowSideColor=gray.colors(k, start = 0.2, end = 0.9)
	if(!Rowdistfun %in% c("cor","euclidian")) stop("Rowdistfun must be one of 'cor' or 'euclidian'!")
	if(!Coldistfun %in% c("cor","euclidian")) stop("Coldistfun must be one of 'cor' or 'euclidian'!")
	
	 library(gplots)
	  library(marray)
	   jpeg(".tmp")         
	 cl=palette();
	 #dev.off()         
		unlink(".tmp")        

	  ftype=c("tiff","emf","png","eps","svg")
	   if(!fileType%in%ftype) stop("Error: file type must be tiff, emf, png or eps");
	exprData=exprData[geneSet,]
	  
	#colid=NULL;
#	rowv=TRUE;
	wdt=900;
	wdte=12;
	kcex=0.5;
	
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
	kcex=0.5;
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
	useRasterTF=T;

##-----------------------##
## plot Row dendrogram
##-----------------------##
	 
	if(hclustGenes){
		cat("\n -> Plotting Dendrogram... \n")
#		hc02=as.hclust(hmp02$rowDendrogram)
		png(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_hclustGenes.png",sep="")),width=800, height=400)
		plot(hc,hang=-1,labels=FALSE,sub=paste("hclust method: ward2\n", subdist),xlab="",main="")
		hcgp=rect.hclust(hc,k=k,border="red")
		#dev.off() 

		png(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_hclustGenes_heights.png",sep="")),width=800, height=400)
		hts=rev( tail( hc$height,15))
		bp=barplot(hts,names.arg=1:length(hts))
		text(x=bp,y=hts,label= formatC(hts,1,format="f"),pos=3,cex=0.8) 
		#dev.off()
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
	#par(mar=par("mar")+c(0,0,-3,0))
	 #graphics.off()
   par("mar")
	 par(mar=c(4,4,8,0))
	
	#par("mar")
	#par(mar=c(1,1,1,1))
	#par(mfrow=c(4,2))
	# hmp02=heatmap.2(exprData,na.rm=T, col=col.hm,dendrogram="both",labRow =rowIds,labCol=colid,scale=scale, ColSideColors=gpcol,RowSideColors=gpcolr, key=TRUE,
	# 	keysize=kcex, symkey=FALSE, trace="none",density.info="density",distfun=distfunTRIX, hclustfun=hclustfun,cexCol=cexcol,
	# 	Colv=ColvOrd,Rowv=rowv,na.color=na.color,cexRow=cexrow,useRaster=useRasterTF,margins=margins)
	#	mtext(side=3,levels(groups),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[2:(length(levels(groups))+1)],cex=1,line=-1)
	#	mtext(side=3,levels(groups),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[(1:length(levels(groups)))+1],cex=1,line=-1)
	hmp02 = heatmap.2(exprData,na.rm=T, col=col.hm,dendrogram="both",labRow =rowIds,labCol=colid,scale=scale, ColSideColors=gpcol,RowSideColors=gpcolr, key=F,
		                keysize=kcex, symkey=F, trace="none",density.info="density",distfun=distfunTRIX, hclustfun=hclustfun,cexCol=cexcol,
		                Colv=ColvOrd,Rowv=rowv,na.color=na.color,cexRow=cexrow,useRaster=useRasterTF, lhei = c(4,4), lwid = c(4,4),margins=margins)
		#	mtext(side=3,levels(groups),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[2:(length(levels(groups))+1)],cex=1,line=-1)
		mtext(side=3,levels(groups),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[(1:length(levels(groups)))+1],cex=0.9,line=-1)
	#dev.off()
	 
	#return(hmp02)
}

##
###########################"
## cut heatmap
############################

cutHeatmaps= function(hmp,height,exprData,DEGres,workingPath=getwd(),prefix,suffix,groups,cexcol=1,cexrow=1,labrow=F,fileType="png",scale="row",
meanGrp=F,col.hm= maPalette(low="green",high="red",mid="black",k=75), las=2, hmp.plot=F,distfun="cor",plot.boxplot=T,plot.stripchart=F,palette.col=NULL, probes.boxplot=F, ...)
{
require(ggplot2)
require(grid)
require(gridExtra)


	jpeg(".tmp")
	if(!is.null(palette.col)){
			cl=palette(palette.col);
#		}else cl=palette(c("black", "blue", "cyan", "magenta",   "darkgray", "darkgoldenrod", "violet",  "orange", "lightgreen","lightblue", "darkorchid", "darkred","darkslateblue", "darkslategray", "maroon", "burlywood1" , "darkolivegreen"));
		}else cl=  palette(c("#000000", "#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442", "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray", "burlywood1","darkkhaki", "#CC0000" ));
		
	#dev.off()
	unlink(".tmp")

	if(!fileType%in%c("eps","emf","tiff","png"))  stop("Error: file type must be jpg, tiff, png or emf"); 
	if(fileType %in% c("emf","eps") ) require("devEMF") 

	colid=colnames(exprData);
	exprData=exprData[labels(hmp$rowDendrogram),]
	wdt=900;
	wdte=12;
	exprData0=exprData
	groups0=groups
	if(meanGrp){
		cat("\n -> calculating groups means \n")
		prefix=paste(prefix,"meanGrp",sep="_")
		exprData=t(apply(exprData,1,FUN=function(x){tapply(x,groups,mean,na.rm=T)}))
		groups=factor(levels(groups))
		colid=NA;
		wdt=600;
		wdte=8;
	}

	if(distfun=="cor"){	
		distfunTRIX= distcor
	} else {
		distfunTRIX = function(x, method=distfun){ dist(x, method=method)}
	}

	###=======================
	## decoupage de la heatmap
	###=======================
	 # decouper le dendro en 2 parties (upper et lower) a la hauteur desiree
	cut02=cut(hmp$rowDendrogram,h=height)
	#upper est une version tronquee de l'arbre de depart
	#lower est une liste contenant les X sous-dendrogrammes generes par la coupure

	cat("\n -> size of",length(cut02$lower),"sub-dendrograms\n")
	print(sapply(cut02$lower,function(x)length(labels(x))))
	#Pour voir quels sont les effectifs de chaque sous-groupe de genes

	###export toptable sous groupes hclust
	cat(" ->export result tables for each subgroup \n")
	for(i in 1:length(cut02$lower)){
	 write.csv2(DEGres$ResTable[labels(cut02$lower[[i]]),],file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_gp",i,".csv",sep="")))
	}


	gpcol=num2cols(as.numeric(groups))
#	if(!is.null(palette.col)){
#		gpcol=num2cols(as.numeric(groups),palette.col)
#	}else gpcol=num2cols(as.numeric(groups))


		# idx sondes de chaque cluster   
		HCgroupsLab=lapply(cut02$lower,function(x)labels(x))

		# expression de chaque cluster   
		HCgroupsLabExrs=lapply(HCgroupsLab,function(x)exprData0[x,])

		## centrage reduction
		HCgroupsLabExrsCenterScale= lapply(HCgroupsLabExrs,function(y){t(scale(t(y),center=T,scale=T))})

		## exr moyen par groupe et pour chaque cluster
		grps=groups0
		#print(grps)
		#print(HCgroupsLabExrsCenterScale)

		HCgroupsLabExrsCenterScaleMean=lapply(HCgroupsLabExrsCenterScale,function(y){t(apply(y,1,FUN=function(x){tapply(x,grps,mean)}))})

		
	if(plot.boxplot){
	###=======================
	## boxplot de la heatmap
	###=======================
	
		 cat(" ->Expression boxplot for each subgroup \n")

		library(Hmisc)

		for(i in 1:length(HCgroupsLabExrsCenterScaleMean)){
			dataCentS=HCgroupsLabExrsCenterScaleMean[[i]]
			nProbes=nrow(dataCentS)
#			png(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_gp",i,"_boxplots.png",sep="")),height=800,width=800)
#			par(mar=par("mar")+c(2,0,0,0))
#			boxplot(dataCentS,ylab="average z-score",col=cl[(1:length(levels(groups)))+1],las=las,main=paste("Cluster ",i,sep=""),xaxt="n")
#			axis(1,at=1:ncol(dataCentS),labels=colnames(dataCentS),cex.axis=cexcol,las=las)
#			dev.off()
			
			## boxplotggplot2
			dataCentSm=melt(dataCentS)
			colnames(dataCentSm)=c("Probe","Group","Expression")
			
				 			footnote <- paste("Average expression Z-score over replicates; ",nProbes," probes",sep="")

					ggbplot= ggplot(dataCentSm, aes(x=Group, y = Expression)) + 
					theme_bw()+ theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border = element_blank()) +
					theme(axis.line=element_line(colour="#888888"))+
					geom_violin(aes(fill=Group)) +
					scale_fill_manual(values=cl[(1:length(levels(groups)))+1] ) +
					geom_jitter(position=position_jitter(0.16) , colour="#888888", alpha=0.4, shape=16, size=3) + 
					geom_boxplot(width=0.1, fill=NA) +

					labs(title=paste("Cluster",i), y="Average Expression Z-score", caption=footnote)+#, x=factors.names[1], colour=factors.names[2]) + 
					theme(plot.title=element_text(size=24, hjust = 0.5), plot.caption=element_text(size=14, hjust = 0.5),
						axis.title.x=element_text(size=18), axis.title.y=element_text(size=18) , 
						axis.text.x=element_text(size=14, colour="#888888", angle=45, hjust=1),axis.text.y=element_text(size=12, colour="#888888")) 
			
					png(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_gp",i,"_boxplots.png",sep="")),height=800,width=800)
					plot(ggbplot)
				#dev.off()

		
		
		}
	}
	
	 #############"
	 ### ggplot2
	 #############"
	 
	 	 if(plot.stripchart){
	 			cat(" ->Plotting Expression as stripchart for each subgroup \n")
#print(head(HCgroupsLabExrsCenterScale))

	 	numProbes=lapply(HCgroupsLabExrsCenterScale,nrow)
	 	for(i in 1:length(HCgroupsLabExrsCenterScale)){
	 		dataStacked=as.vector(HCgroupsLabExrsCenterScale[[i]])

	 		dataStackeddt=cbind.data.frame(Expression=dataStacked, factor.trace=rep(grps, each=nrow(HCgroupsLabExrsCenterScale[[i]])), probeName=rep(rownames(HCgroupsLabExrsCenterScale[[i]]), ncol(HCgroupsLabExrsCenterScale[[i]])), 
	 		indName=rep(colnames(HCgroupsLabExrsCenterScale[[i]]), each=nrow(HCgroupsLabExrsCenterScale[[i]])))
	 		dataStackeddt$Grp=dataStackeddt$factor.trace
	 		nindiv=table(dataStackeddt$Grp)/numProbes[[i]]
	 		
#	 		print(head(dataStackeddt))
	 		if(all(nindiv==nindiv[1])){ 
	 			nindiv=as.character(nindiv[1]);
	 		} else nindiv=paste(nindiv,collapse=", ")
	 
	 			footnote <- paste("mean expression Z-score +/- 95%CI; N=",nindiv,"; ",numProbes[[i]]," probes",sep="")
#	 print(footnote)
			##=============
	 		## plot stripchart
	 		##

		if(!probes.boxplot){
		#### jitter classique
#		d=data.frame(Grp=rep(c('before','after'), 2000), Expression=rexp(4000, 1))
#				ggstrip= ggplot(d, aes(x=Grp, y = Expression)) + 
#				geom_jitter()
	
		
				ggstrip= ggplot(dataStackeddt, aes(x=Grp, y = Expression)) + 
#				geom_jitter()
					theme_bw()+ theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border = element_blank()) +
					theme(axis.line=element_line(colour="#888888"))+
					geom_violin() +
#					geom_violin(aes(fill=Grp)) +
##					geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5) +
#					geom_sina() +  #  package ggforce
#					geom_boxplot(width = 0.1) +
					geom_jitter(position=position_jitter(0.16) , colour="#888888", alpha=0.05, shape=16, size=3) + 
#					geom_sina(aes(color=Grp)) +
#					geom_jitter(width=0.5, position=position_jitter(0.05), size=3, colour="#888888") + 
					stat_summary(fun.data = mean_cl_normal, geom="errorbar", colour="red", width=0.1, size=1) + 
					stat_summary(fun.y=mean, geom="point", color="red", size=2)  + 
#					labs(title=paste("Cluster",i), y="Expression Z-score")+#, x=factors.names[1], colour=factors.names[2]) + 
					labs(title=paste("Cluster",i), y="Expression Z-score", caption=footnote)+#, x=factors.names[1], colour=factors.names[2]) + 
					theme(plot.title=element_text(size=24, hjust = 0.5), plot.caption=element_text(size=14, hjust = 0.5),
						axis.title.x=element_text(size=18), axis.title.y=element_text(size=18) , 
						axis.text.x=element_text(size=14, colour="#888888", angle=45, hjust=1),axis.text.y=element_text(size=12, colour="#888888")) 
		#-------------->
#				ggstrip= ggplot(dataStackeddt, aes(x=Grp, y = expression)) + 
#			theme_bw()+ theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border = element_blank()) +
#			theme(axis.line=element_line(colour="#888888"))+
##			geom_jitter(width=0.5, position=position_jitter(0.1), colour="#888888") + 
#			geom_jitter(position=position_jitter(0.1), colour="#888888") + 
#			stat_summary(fun.data = mean_cl_normal, geom="errorbar", colour="red", width=0.1, size=1) + 
#			stat_summary(fun.y=mean, geom="point", color="red", size=2)  + 
##			labs(title=paste("Cluster",i), y="Expression Z-score")+#, x=factors.names[1], colour=factors.names[2]) + 
#			labs(title=paste("Cluster",i), y="Expression Z-score", caption=footnote)+#, x=factors.names[1], colour=factors.names[2]) + 
#			theme(plot.title=element_text(size=24, hjust = 0.5), 
#				axis.title.x=element_text(size=18), axis.title.y=element_text(size=18) , 
#				axis.text.x=element_text(size=14, colour="#888888", angle=45, hjust=1),axis.text.y=element_text(size=12, colour="#888888")) 
			
##		png(file.path(MAtrixData$parameters$workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_gp",i,"_stripChart.png",sep="")), width=500, height=500)
##			gstrip <- arrangeGrob(ggstrip, bottom = textGrob(footnote, x = 0.5, hjust = 0.5, vjust=0.5, gp = gpar(fontsize = 11)), heights=c(9,1))
##			grid.draw(gstrip)
##		dev.off()
#		png(file.path(MAtrixData$parameters$workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_gp",i,"_stripChart.png",sep="")), width=500, height=500)
#			plot(ggstrip)
#		dev.off()
# -------------------------->
		
		
		
		} else {
		##### jitter classique avec boxplots par probe
		
		ggstrip= ggplot(data=dataStackeddt, aes(x=Grp, y=expression, color=probeName))+ 
					theme_bw()+ theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border = element_blank()) +
					theme(axis.line=element_line(colour="#888888"))+
					geom_boxplot(outlier.shape=1, notch=F) +
					#geom_jitter(position=position_dodge(0.6), width=0.8) +
					theme(legend.position="none") +
#					geom_jitter(width=0.5, position=position_dodge(0.05), size=3, colour="#888888") + 
					stat_summary(fun.data = mean_cl_normal, geom="errorbar", colour="red", width=0.1, size=1) + 
					stat_summary(fun.y=mean, geom="point", color="red", size=2)  + 
#					labs(title=paste("Cluster",i), y="Expression Z-score")+#, x=factors.names[1], colour=factors.names[2]) + 
					labs(title=paste("Cluster",i), y="Expression Z-score", caption=footnote)+#, x=factors.names[1], colour=factors.names[2]) + 
					theme(plot.title=element_text(size=24, hjust = 0.5), 
						axis.title.x=element_text(size=18), axis.title.y=element_text(size=18) , 
						axis.text.x=element_text(size=14, colour="#888888", angle=45, hjust=1),axis.text.y=element_text(size=12, colour="#888888")) 
					 
		}
			
				png(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_gp",i,"_stripChart.png",sep="")), width=500, height=500)
					plot(ggstrip)
#					gstrip <- arrangeGrob(ggstrip, bottom = textGrob(footnote, x = 0.5, hjust = 0.5, vjust=0.5, gp = gpar(fontsize = 11)), heights=c(9,1))
#					grid.draw(gstrip)
				#dev.off()
	 		}			
	# #		ggsave(,plot= g, width=10, height=10)
	# #ggsave("rtest.png",g)
	 	} ####################"" END ggplot2
	

	###=======================
	## plot de la heatmap
	###=======================
	if(hmp.plot){
		 cat(" ->plot heatmap for each subgroup \n")
		sink(".temp")
		for(i in 1:length(cut02$lower)){
		  if(length(labels(cut02$lower[[i]]))>1){
			rowIds=NA;
		#    if(length(labrow)>1){ rowIds=labrow[labels(cut02$lower[[i]])]
		#    }else if(labrow==T) rowIds=DEGres$ResTable$GeneName[labels(cut02$lower[[i]])]
		#if(length(labrow)>1){ rowIds=labrow[labels(cut02$lower[[i]])]
		  if(labrow==T) rowIds=DEGres$ResTable[labels(cut02$lower[[i]]),"GeneName"]

			useRasterTF=T;
			if(fileType=="jpg")
			  jpeg(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_gp",i,".jpg",sep="")),width=wdt, height=900,quality=100)
			if(fileType=="tiff")
			  tiff(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_gp",i,".tiff",sep="")),width=wdt, height=900)
			if(fileType=="png")
			  png(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_gp",i,".png",sep="")),width=wdt, height=900)
			if(fileType=="emf"){
			  emf(file = file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_gp",i,".emf",sep="")), width = wdte, height = 12,
			  bg = "transparent", fg = "black", pointsize = 12, custom.lty = FALSE)
			  useRasterTF=F
			}
			if(fileType=="eps"){
				postscript(file.path(workingPath,"DEG",paste(prefix,"_heatmap_",suffix,"_gp",i,".eps",sep="")),  width = wdte, height = 12,
				bg = "transparent", pointsize = 12)
			}



			hm02gp=heatmap(exprData[labels(cut02$lower[[i]]),], Rowv=str(cut02$lower[[i]]),
			Colv=hmp$colDendrogram,
			col= col.hm,
			distfun=distfunTRIX,
			hclustfun=hclustfun,
			labRow =rowIds,
			labCol=colid,
			ColSideColors=gpcol,
			cexCol=cexcol,
			cexRow=cexrow,
			scale=scale,
			na.rm=T,
			margins=c(8,8),
			useRaster=useRasterTF)

			legend("topleft",legend=levels(groups),text.col=cl[(1:length(levels(groups)))+1],border=F,adj=c(0,0),horiz=F,bty="n")
			#dev.off()
		  }
		}
		sink()
	}

unlink(".temp")
}
