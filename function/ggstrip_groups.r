#' ### Author: Yannick Lippi and adapted to shiny by Franck Soubès
#' ### Bioinformatics Master Degree - University of Bordeaux, France
#' ### Link: https://github.com/fsoubes/MA_Trix_App
#' ### Where: GET-TRiX's facility
#' ### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
#' ### Licence: GPL-3.0


###############################
## ggstrip per group and per gene
###############################


ggstrip_groups <- function(  grps , wSet, probesID, SubIndivID=1:ncol(wSet), SubGrpsID, dietNamesCol ) {
  
    
    nindiv=table(grps)
    nindiv=nindiv[nindiv!=0]
    if(all(nindiv==nindiv[1])){ 
      nindiv=as.character(nindiv[1]);
    } else nindiv=paste(nindiv,collapse=", ")
    
    datai=cbind.data.frame(Group=grps, expression=as.numeric(wSet[1,-1]), row.names = NULL) # -(1:2)
    geneName=wSet[1,"X"] #GeneName

    footnote <- paste("Error bar: mean +/- SEM; n=",nindiv,sep="")
    ggstrip= ggplot(datai, aes(x=Group, y = expression)) + 
      theme_classic() +				
      geom_jitter( position=position_jitter(0.15), size=2) +
      # scale_color_manual(values=as.character(dietNamesCol$groupsColors[SubGrpsID])) + 
      # stat_summary(fun.data = mean_cl_normal, geom="pointrange", colour="black", size=1) + 
      # stat_summary(fun.y = mean, geom="point", colour="black", size=5, shape=3) +
      stat_summary(fun.data = mean_se, geom="errorbar", colour="darkred", size=1,aes(width=0.2)) + 
      labs( y=paste(geneName,"expression level"), caption=footnote)+ 
      theme(plot.caption=element_text(size=12, hjust = 0.5), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16) , 
            legend.title = element_text(size=12),
            axis.text.x=element_text(size=12, colour="#888888", angle=45, hjust=1),axis.text.y=element_text(size=12, colour="#888888")
      ) 
    
    plot(ggstrip)
    return(ggstrip)			
  
}

