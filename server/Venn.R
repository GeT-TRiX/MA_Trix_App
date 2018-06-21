#########################################
######## Venn diagram                   #
#########################################

value=T # boolean at t=0

#' output$bool is a reactive function that return the bool value in the local environment
#'
#' @value a boolean
#'
#' @return \bool a reactive boolean outside the reactive environment
#'

output$bool <- reactive({
  value
})

outputOptions(output,"bool",suspendWhenHidden=F)

#' vennlist is a reactive function which aim is to return a list of signficant genes for a treshold pvalue of 5%
#'
#' @csvf a data frame
#' @user_cont a data frame with the contrast selected
#'
#' @return \vennlist a reactive list
#'

vennlist <- reactive({
  req(user_cont() > 0)
  
  if (is.null(csvf()))
    return(NULL)
  mycont = Vennlist(pval = csvf()[[3]], user_cont(),user_fc(), input$regulation, input$pvalvenn, input$fcvenn)
  probven = rowtoprob(mycont,csvf()[[3]], user_cont() )
  #colnames(probven) = names(user_cont())
  return(probven)
})

#' Vennplot is a reactive function that return a plot object or a link if the user want to display more tha  5sets
#'
#' @param Vennploted a reactive object
#'
#' @return \Vennplot a reactive object to be plot
#'

Vennplot <- reactive({
  
  req(vennlist)
  
  #' Vennplot is a reactive function that return an object of type venn if the number of set is stricly inferior to 6
  #' or a link to a website if it's not
  #' 
  #' @param user_cont a reactive data frame with the selected contrast
  #' @param input$vennsize the police size for the contrasts
  #' @param vennlist a list
  #'
  #' @return \Vennploted a reactive object to be plot
  #'
  
  Vennploted <- reactive({
    
  
    
  if(length(user_cont()) <= 5){
  #g = Vennfinal(vennlist(), user_cont(), cex = input$vennsize, input$pvalvenn, input$fcvenn)
    g = Vennfinal(vennlist(), user_cont(), cex = input$vennsize, input$pvalvenn, input$fcvenn, input$methodforvenn)
  

   observe({value <<-T}) # listen inside the reactive expression 
    
   #' output$bool is a reactive function that set the bool value to T 
   #'
   #' @value a boolean
   #'
   #' @return \bool a reactive boolean inside the reactive environment 
   #'
    
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
    #' @return \bool a reactive boolean inside the reactive environment 
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

observeEvent(input$allCont, {
  updateCheckboxGroupInput(
    session,
    "cont",
    label = "Choose your comparison",

    choices = colnames(adjusted()[[1]][,-1][myindex()]),
    selected = colnames(adjusted()[[1]][,-1][myindex()])
  )
})

observeEvent(input$noCont, {
  updateCheckboxGroupInput(session,
                           "cont",
                           label = "Choose your comparison",
                           #choices = colnames(adjusted()[[1]][,-1][,-c(indnull())])
                           choices = colnames(adjusted()[[1]][,-1][myindex()])
  )
})

#' indnull is a reactive function that return a vector for the contrasts with 0 genes significant at a treshold set to 5%
#'
#' @param vennlist a list
#'
#' @return \indnull a reactive vector 
#'

indnull <- reactive({

    indexnull = which( sapply(vennlist() ,length) == 0)
    return(indexnull)
})


#' choix_cont is a reactive function that return the contrast selected by the user
#'
#' @param input$cont a set of contrasts selected by the user
#'
#' @return \choix_cont a set of characters input
#'

choix_cont <- reactive({
  return(input$cont)
})


#' user_cont is a reactive function that  return the contrast selected by the user
#'
#' @param adjusted a data frame corresponding to the contrasts selected
#' @param choix_cont a set of character 
#'
#' @return \user_cont a reactive data frame with the contrast selected
#'

user_cont <- reactive({
  
  
  if (input$methodforvenn == "FDR")
    mysel = (subset(adjusted()[[1]],
                  select = choix_cont()))
  else 
    mysel = (subset(adjusted()[[3]],
                    select = choix_cont()))
  return(mysel)
})

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
      try(myventocsv(vennlist()  , user_cont())),
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

myindex <- reactive ({
  
  myl = lapply(seq(ncol(adjusted()[[1]])),function(x)
    return(which(adjusted()[[1]][[x]] < 0.05)))
    
  indexnull = which( sapply(myl ,length) == 0)
  final = colnames(adjusted()[[1]][,-c(indexnull)])
  return(final)
  
})
