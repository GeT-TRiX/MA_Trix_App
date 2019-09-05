### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#' downloadFiles is a shiny widget which aims is to download a reactive plot
#'
#' @param id Shiny id
#' @param label Shiny label
#'
#' @return Widget in the gui
#'
#' @export
#'

downloadFiles <- function(id, label = "Save your Scree plot") {
  ns <- NS(id)
  downloadButton(ns("downloadgraphs"), label ,   style ="color: #fff; background-color: #337ab7; border-color: #2e6da4")
}


#' selFormat is a shiny widget which aims is to specify the output format
#'
#' @param id Shiny id
#'
#' @return Widget in the gui
#'
#' @export
#'

selFormat <- function(id) {
  ns <- NS(id)
  selectInput(ns("format"), label = NULL, choices = c("png", "eps", "pdf", "svg"))
}

#' downoutputfiles is a shiny module which aims is to export reactive plot
#'
#' @param input Internal
#' @param output Internal
#' @param session Internal
#' @param projectname A reactive character (MA project or session data)
#' @param suffix A character
#' @param data A reactive expression to be plot
#' @param w Width of the exported file
#' @param h Height pf the expored file
#' @param cutheat A boolean value
#' @param volcform A boolean value
#' @param hm A boolean value
#' @param clustvenn A boolean value. Default NULL.
#' @param rown A reactive character to display (show) or hide labels in the outputted heatmap
#'
#' @export
#'

downoutputfiles <- function(input, output, session , projectname, suffix = "plot.png",  data , w = 12  , h = 12 ,
                            cutheat = F, volcform = F, hm =F ,  clustvenn = NULL, rown =NULL) {

observe({
  if(!is.null(clustvenn)){
    req(data())
    pdf(NULL)
    if(class(data()) == "graphNEL")
      shinyjs::disable("downloadgraphs")
    else
      shinyjs::enable("downloadgraphs")
    dev.off()
  }
})



output$downloadgraphs  <- downloadHandler(

filename <- function() {

paste0(basename(file_path_sans_ext(projectname())), suffix , input$format , sep ='')},

content <- function(file) {


  if ( input$format == "pdf")

    pdf(file,
        width = w,
        height = h,
        pointsize = 12)


  else if ( input$format == "png")

      png(
          file,
          width = w,
          height = h,
          units = "in",
          pointsize = 12,
          res = 100)

  else if ( input$format == "svg")

    svg(file,
        width = w,
        height = h,
        pointsize = 12
  )

  else if (cutheat  && input$format == "eps")

    cairo_ps(filename=file, width=10, height=10,pointsize = 12)

  else if(volcform  &&  input$format == "eps")
    cairo_ps(filename=file, width=10, height=10,pointsize = 12, fallback_resolution = 600)

##    ggsave(file,device=cairo_ps, fallback_resolution = 600)

  else
      eps(file,
        width = w,
        height = h,
        pointsize = 12)

  if(hm == T){

    revRowInd <- match(c(1:length(data$hm$rowInd)), data$hm$rowInd)
    revColInd <- match(c(1:length(data$hm$colInd)), data$hm$colInd)
    par(mar=c(5,5,1,1.10))
    if(is.null(data$colgroup))cl = palette(palette)else  cl=palette(data$colgroup)
    heatmap.2(t(data$hm$carpet)[revRowInd, revColInd], Rowv=data$hm$rowDendrogram, Colv=data$hm$colDendrogram, col=data$hm$col, useRaster = T, keysize=1, na.rm=T,  na.color="black",labRow = switch(rown(), hide = NA , show = data$rownames),
              trace = c("none"), layout(lmat =rbind(4:3,2:1),lhei = c(0.05,1), lwid = c(0.1,1)),key=T,density.info="density", scale="row", RowSideColors = data$rows , ColSideColors = data$cols, cexRow =0.9)
    mtext(side=3,sort(levels(data$groups)),adj=1,padj=seq(0,by=1.4,length.out=length(levels(data$groups))),col=cl[(1:length(levels(data$groups)))],cex=1,line=-1)
  } else
    plot(data())


  dev.off()
})

}
