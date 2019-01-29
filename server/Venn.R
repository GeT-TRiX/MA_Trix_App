### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


#########################################
######## Venn diagram                   #
#########################################

value=T # boolean at t=0

#' bool is a reactive function that return the bool value in the local environment
#'
#' @value boolean
#'
#' @return bool a reactive boolean outside the reactive environment
#'
#' @export

output$bool <- reactive({
  value
})

outputOptions(output,"bool",suspendWhenHidden=F)

#' vennlist is a reactive function which aim is to return a list of signficant probenames
#'
#' @param csvf a data frame
#' @param user_cont a subset data frame with the selected comparisons for the adj.p.val or p.val
#' @param user_fc a subset data frame with the selected comparisons for the logfc
#' @param regulation vector input
#' @param pvalvenn numeric input for the p value cutoff
#' @param fcvenn numeric input for the logfc value cutoff
#'
#' @return probven a reactive list of probenames
#'
#' @export

vennlist <- reactive({
  req(user_cont() > 0)

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
  mycont = Vennlist(user_cont(),user_fc(), input$regulation, input$pvalvenn, input$fcvenn)
  probven = rowtoprob(mycont,csvf()[[3]], user_cont())



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



observe({


  validate(
    need(csvf(), 'You need to import data to visualize this plot!'))


observe({

groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
output$contout <- renderUI( ##validate

  checkboxGroupInput(
    inputId = "cont" ,
    label =  "Choose your comparison",
    choices = colnames(subsetstat()[[1]][myindex()]),
    inline = groupinline
  )
)
})

})


observeEvent(input$allCont, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "cont",
    label = "Choose your comparison",

    choices = colnames(subsetstat()[[1]][myindex()]),
    selected = colnames(subsetstat()[[1]][myindex()]),
    inline = groupinline
  )
})

observeEvent(input$noCont, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(session,
                           "cont",
                           label = "Choose your comparison",
                           choices = colnames(subsetstat()[[1]][myindex()]),
                           inline=groupinline
  )
})



#' indnull is a reactive function that return a vector for the contrasts with 0 genes significant at a treshold set to 5%
#'
#' @param vennlist a list
#'
#' @return indnull a reactive vector
#'
#' @export

indnull <- reactive({

    indexnull = which( sapply(vennlist()[[1]] ,length) == 0)
    return(indexnull)
})


#' choix_cont is a reactive function that return the contrast selected by the user
#'
#' @param cont a set of contrasts selected by the user
#'
#' @return choix_cont a set of characters input
#'
#' @export
#'

choix_cont <- reactive({
  return(input$cont)
})


#' user_cont is a reactive function that  return the contrast selected by the user
#'
#' @param subsetstat data frame corresponding to the pvalue or subsetstat pvalue
#' @param choix_cont a set of contrasts selected by the user
#'
#' @return user_cont a reactive data frame with the contrast selected
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


#' user_cont is a reactive function that  return the contrast selected by the user
#'
#' @param subsetstat data frame corresponding to the logfc value
#' @param choix_cont a set of contrasts selected by the user
#'
#' @return user_cont a reactive data frame with the contrast selected
#'
#' @export
#'

user_fc <- reactive({

  mysel = (subset(subsetstat()[[2]],
                  select = choix_cont()))
  return(mysel)
})


output$downloadvenn <- downloadHandler(
  filename = function() {
    paste(basename(file_path_sans_ext(projectname())),
          '_filtered_venn',
          '.csv',
          sep = '')
  },
  content = function(fname) {
    write.table(
      try(myventocsv(vennlist()[[2]]  , user_cont())),
      fname,
      na = "",
      row.names = F,
      col.names = T,
      append = TRUE,
      sep = ";"
    )
  }
)


output$downloadsetven <- downloadHandler(
  filename = function() {
    paste(basename(file_path_sans_ext(projectname())),
          '_inter_venn',
          '.csv',
          sep = '')
  },
  content = function(fname) {
    if(input$dispvenn == "genes")
    write.table(
      try(mysetventocsv(setvglobalvenn(vennlist()[[2]], user_cont(), dll = T))),
      fname,
      na = "",
      row.names = F,
      col.names = T,
      append = TRUE,
      sep = ";"
    )
    else
      write.table(
        try(mysetventocsv(setvglobalvenn(vennlist()[[1]], user_cont(), dll = T))),
        fname,
        na = "",
        row.names = F,
        col.names = T,
        append = TRUE,
        sep = ";"
      )

  }
)


#' myindex is a reactive function returning the column indices for which there's more than one significant genes
#'
#' @param subsetstat data frame corresponding to the subsetstat.pval
#'
#' @return myindex a numeric vector
#'
#' @export
#'


myindex<- reactive({

  myl = lapply(seq(ncol(subsetstat()[[1]])),function(x)
    return(which(subsetstat()[[1]][[x]] < 0.05)))

  indexnull = which( sapply(myl ,length) == 0)
  if(length(indexnull)>0)
    selcol = colnames(subsetstat()[[1]][,-c(indexnull),drop = FALSE])
  else
    selcol = colnames(subsetstat()[[1]])

  return(selcol)

})
