### Author: Yannick Lippi and adapted to shiny by Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0



#' ggstrip_groups which aims is to return a strip plot of the expression for a selected gene for each groups.
#'
#' @param grps A character vector sorted by group of each sample
#' @param wSet A dataframe of the normalised expression
#' @param probesID A vector of the expression of a selected row from the outputted table
#'
#' @return a ggplot object
#'
#' @export
#'


ggstrip_groups <- function(grps , wSet, probesID) {


    nindiv=table(grps)
    nindiv=nindiv[nindiv!=0]
    if(all(nindiv==nindiv[1])){
      nindiv=as.character(nindiv[1]);
    } else nindiv=paste(nindiv,collapse=", ")

    datai=cbind.data.frame(Group=grps, expression=as.numeric(wSet[probesID,-(1:2)]), row.names = NULL)
    geneName=wSet[probesID,"GeneName"]
    footnote <- paste("Error bar: mean +/- SEM; n=",nindiv,sep="")
    ggstrip= ggplot(datai, aes(x=Group, y = expression)) +
      theme_classic() +
      geom_jitter( position=position_jitter(0.15), size=2) +
      stat_summary(fun.data = mean_se, geom="errorbar", colour="darkred", size=1,aes(width=0.2)) +
      labs( y=paste(geneName,"expression level"), caption=footnote)+
      theme(plot.caption=element_text(size=12, hjust = 0.5), axis.title.x=element_text(size=16), axis.title.y=element_text(size=16) ,
            legend.title = element_text(size=12),
            axis.text.x=element_text(size=12, colour="#888888", angle=45, hjust=1),axis.text.y=element_text(size=12, colour="#888888")
      )

    plot(ggstrip)
    return(ggstrip)
}
