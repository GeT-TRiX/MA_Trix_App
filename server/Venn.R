### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0


#########################################
######## Venn diagram                   #
#########################################


getvennlist <- reactiveValues(vennlist = NULL) # Initiliazition of the reactive values for the vennlist
mycont <- callModule(getDegenes, "degvenn", data = user_cont , meth = NULL, dflogfc = user_fc ,  maxDe = NULL, reg = reactive(input$regulation), case =2) #Outisde observe to update fc widget step (0.1, [1;2] and (1, [2;10]))


# observe({
#   req(length(user_cont()) >0) # User selection
#   getvennlist$vennlist <- mycont() # push shiny module vennlist to the reactive values object
# })


vennlist <- reactive({
  req (length(user_cont())>0)
  if (is.null(csvf()))
    return(NULL)
  
  # adj <- user_cont()
  # fc <- user_fc()
  # cutoffpval <-input$pvalvenn
  # cutofffc <- input$fcvenn
  # reg <- input$regulation
  # cl <- makeCluster(getOption("cl.cores", 2))
  # clusterExport(cl,c("adj","fc","cutoffpval","cutofffc","reg","cutoffpval"),envir=environment())
  #mycont = Vennlist(adj,fc, reg, cutoffpval, cutofffc,cl)
  #stopCluster(cl)
  getvennlist$vennlist <- mycont() # push shiny module vennlist to the reactive values object
  probven = rowtoprob(getvennlist$vennlist,csvf()[[3]], user_cont())

  if(input$dispvenn == "genes")
    if(input$Notanno){
      vennlist <- lapply(probven[[2]], grep, pattern="^chr[A-z0-9]{1,}:|^ENSMUST|^LOC[0-9]{1,}|^[0-9]{4,}$|^A_[0-9]{2}_P|^NAP[0-9]{4,}|[0-9]{7,}", value=TRUE, invert=TRUE)
      Rtojs <- toJvenn(vennlist,user_cont())
    }
  else
    Rtojs <- toJvenn(probven[[2]],user_cont())
  else
    Rtojs <- toJvenn(probven[[1]],user_cont())

  Mymode <-  input$updamod # Mode
  Myfont <-  input$myfont # Font size
  Mystat <-  input$mystat # Stat
  Myswitch <-  input$dispswitch # Stat

  wrongcol <- function(y)
    if (any(grepl("col2rgb", y)))
      invokeRestart("muffleWarning")

  col2js =  tryCatch({
    col2rgb(mycol()) %>%  lapply(.,function(x)return(x)) %>% withCallingHandlers(error = wrongcol)
  }, error = function(e) {shinyjs::alert("Wrong color")})


  session$sendCustomMessage(type="updatejvenn", Rtojs)
  session$sendCustomMessage(type="updatejcol", col2js)

  return(probven)
})

jvennc_input <- reactive({
  input$fill
})

jvenncol <- shiny::debounce(jvennc_input, 500)

mycol <- reactive({
  if(!jvenncol() == ""){

    mycol = gsub("^\\s+|\\s+$", "", unlist(strsplit(jvenncol(), ",")))
  }
  else
    mycol = ""
})

subsetstatRm <- reactive({
  req(subsetstat())
  return(subsetstat()[[1]][myindex()])
})

choix_cont <- callModule(boxChooser, "selcompvenn", label = "Choose your comparison", data = reactive(colnames(subsetstatRm())) , group = csvf, case = 2 , empty = T )

#' user_cont is a reactive function that  return the a subset dataframe with the comparison(s) selected
#'
#' @param subsetstat A data frame corresponding to the pvalue or subsetstat pvalue
#' @param choix_cont A set of contrasts selected by the user
#'
#' @return A reactive dataframe with the contrast selected
#'
#' @export
#'

user_cont <- reactive({
  
  req(subsetstat())
  if (input$methodforvenn == "FDR")
    mysel = (subset(subsetstat()[[1]],
                  select = choix_cont()))
  else
    mysel = (subset(subsetstat()[[3]],
                    select = choix_cont()))
  return(mysel)
})


#' user_fc is a reactive function that  return the a subset dataframe with the comparison(s) selected
#'
#' @param subsetstat A data frame corresponding to the logfc value
#' @param choix_cont A set of contrasts selected by the user
#'
#' @return A reactive dataframe with the contrast selected
#'
#' @export
#'

user_fc <- reactive({

  mysel = (subset(subsetstat()[[2]],
                  select = choix_cont()))
  return(mysel)
})


callModule(downoutputables, "savevennlist", projectname = projectname , suffix = "_filtered_venn.csv" , data = reactive(vennlist()[[2]]) , cont = user_cont, case = 1 )


observe({
req(input$dispvenn)
callModule(downoutputables, "saveallset", projectname = projectname , suffix = "_inter_venn.csv" , data = switch(input$dispvenn, genes = reactive(vennlist()[[2]]), transcripts =, probes = reactive(vennlist()[[1]])) , cont = user_cont , case = 2 )
})



#' myindex is a reactive function returning the column indices for which there's more than one significant genes
#'
#' @param subsetstat data frame corresponding to the subsetstat.pval
#'
#' @return myindex a numeric vector
#'
#' @export
#'


myindex<- reactive({
  
  req(filtermethjvenn())
    
  myl = lapply(seq(ncol(subsetstat()[[3]])),function(x)
    return(which(subsetstat()[[3]][[x]] < 0.05 ))) 
  
  indexnull = which( sapply(myl ,length) == 0)
  
  if(length(indexnull)>0)
    selcol = colnames(subsetstat()[[1]][,-c(indexnull),drop = FALSE])
  else
    selcol = colnames(subsetstat()[[1]])

  return(selcol)

})


filtermethjvenn <- reactive({
  return(input$filtermethjvenn)
})