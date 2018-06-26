#########################################
######## Venn diagram                   #
#########################################

value=T # boolean at t=0

#' bool is a reactive function that return the bool value in the local environment
#'
#' @value a boolean
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
  mycont = Vennlist(user_cont(),user_fc(), input$regulation, input$pvalvenn, input$fcvenn)
  probven = rowtoprob(mycont,csvf()[[3]], user_cont())
  #colnames(probven) = names(user_cont())
  #return(mycont)
  return(probven)
})

#' Vennplot is a reactive function that return a plot object or a link if the user want to display more tha  5sets
#'
#' @param Vennploted a reactive object
#'
#' @return Vennplot a reactive object to be plot
#' 
#' @export

Vennplot <- reactive({
  
  req(vennlist)
  
  #' Vennplot is a reactive function that return an object of type venn if the number of set is stricly inferior to 6
  #' or a link to a website if it's not
  #'
  #' @param user_cont a subset data frame with the selected comparisons for the adj.p.val or p.val
  #' @param input$vennsize the police size for the contrasts
  #' @param vennlist a list of probenames
  #' @param pvalvenn numeric input for the p value cutoff
  #' @param fcvenn  numeric input for the logfc value cutoff
  #' @param methodforvenn character input
  #' @param dispvenn character input for plot a venn diagram with probes or genes
  #' @param csvf data frame corresponding to the alltoptable
  #'
  #' @return Vennploted a reactive object to be plot
  #'
  #' @export
  
  Vennploted <- reactive({
    
  
    
  if(length(user_cont()) <= 5){
  #g = Vennfinal(vennlist(), user_cont(), cex = input$vennsize, input$pvalvenn, input$fcvenn)
    
  g = Vennfinal(vennlist()[[1]], user_cont(), cex = input$vennsize, input$pvalvenn, input$fcvenn, input$methodforvenn, input$dispvenn , csvf()[[3]])
  

   observe({value <<-T}) # listen inside the reactive expression 
    
   #' output$bool is a reactive function that set the bool value to T
   #'
   #' @value a boolean
   #'
   #' @return bool a reactive boolean inside the reactive environment
   #' 
   #' @export
    
    output$bool <- reactive({ 
      value
    })
  
  return(g)}
  else { 

    observe({ value <<- F}) # listen inside the reactive expression 
    
    #' output$bool is a reactive function that set the bool value to F
    #'
    #' @value a boolean
    #'
    #' @return bool a reactive boolean inside the reactive environment
    #'
    
    output$bool <- reactive({
      value
    })
    
    output$image <- renderUI({
      tags$img(src = "https://i.imgur.com/lB5wmMp.png")
    })
    url <- a("venntools", href = "http://jvenn.toulouse.inra.fr/app/example.html", target = "_blank")
    url2 <- a("venntools2", href = "http://bioinfogp.cnb.csic.es/tools/venny/", target = "_blank")
    output$sorry <- renderUI({tagList("You're trying to plot more than 5 sets, download the csv file and use the following tool", url)})
    
    }
  })
  
  return(Vennploted())
})

observe({
  
  
  validate(
    need(csvf(), 'You need to import data to visualize this plot!'))
  #req(csvf())

  
  
observe({
    
   groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)  
    
output$contout <- renderUI(
  ##validate

  checkboxGroupInput(
    inputId = "cont" ,
    label =  "Choose your comparison",
    choices = colnames(adjusted()[[1]][,-1][myindex()]),
    #selected = colnames(adjusted()[[1]][,-1][myindex()])
    inline = groupinline
  )
)
})

})

observe({
  
  req(myindex())
  print("check")
  print(colnames(adjusted()[[1]][,-1][myindex()]))
  
})


observeEvent(input$allCont, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "cont",
    label = "Choose your comparison",

    choices = colnames(adjusted()[[1]][,-1][myindex()]),
    selected = colnames(adjusted()[[1]][,-1][myindex()]),
    inline = groupinline
  )
})

observeEvent(input$noCont, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(session,
                           "cont",
                           label = "Choose your comparison",
                           #choices = colnames(adjusted()[[1]][,-1][,-c(indnull())])
                           choices = colnames(adjusted()[[1]][,-1][myindex()]),
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
#' @param adjusted data frame corresponding to the pvalue or adjusted pvalue
#' @param choix_cont a set of contrasts selected by the user
#'
#' @return user_cont a reactive data frame with the contrast selected
#'
#' @export
#'

user_cont <- reactive({
  req(adjusted())
  
  if (input$methodforvenn == "FDR")
    mysel = (subset(adjusted()[[1]],
                  select = choix_cont()))
  else 
    mysel = (subset(adjusted()[[3]],
                    select = choix_cont()))
  return(mysel)
})


#' user_cont is a reactive function that  return the contrast selected by the user
#'
#' @param adjusted data frame corresponding to the logfc value
#' @param choix_cont a set of contrasts selected by the user
#'
#' @return user_cont a reactive data frame with the contrast selected
#'
#' @export
#'

user_fc <- reactive({
  
  mysel = (subset(adjusted()[[2]],
                  select = choix_cont()))
  return(mysel)
})


output$downloadvenn <- downloadHandler(
  filename = function() {
    paste(basename(file_path_sans_ext(projectname())),
          '_clustered_venn',
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
          '_clustered_venn',
          '.csv',
          sep = '')
  },
  content = function(fname) {
    if(input$dispvenn == "genes")
    write.table(
      try(myventocsv(setvglobalvenn(vennlist()[[2]], user_cont()) , user_cont())),
      fname,
      na = "",
      row.names = F,
      col.names = T,
      append = TRUE,
      sep = ";"
    )
    else
      write.table(
        try(myventocsv(setvglobalvenn(vennlist()[[1]], user_cont()) , user_cont())),
        fname,
        na = "",
        row.names = F,
        col.names = T,
        append = TRUE,
        sep = ";"
      )
      
  }
)


# user_length<- reactive({
#   return(length(user_cont()))
# })

################# TO DO commented

#' myindex is a reactive function returning the column indices for which there's more than one significant genes
#'
#' @param adjusted data frame corresponding to the adjusted.pval
#'
#' @return myindex a numeric vector
#'  
#' @export
#'
 

myindex<- reactive({
  
  myl = lapply(seq(ncol(adjusted()[[1]])),function(x)
    return(which(adjusted()[[1]][[x]] < 0.05)))
    
  indexnull = which( sapply(myl ,length) == 0)
  final = colnames(adjusted()[[1]][,-c(indexnull)])
  return(final)
  
})
