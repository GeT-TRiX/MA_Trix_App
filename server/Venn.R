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
  #print(value)
  value
})

outputOptions(output,"bool",suspendWhenHidden=F)

#' vennlist is a reactive function which aim is to return a list of signficant genes for a treshold pvalue of 5%
#'
#' @csvf a data frame
#' @user_cont a  data frame with the contrast selected
#'
#' @return \vennlist a reactive list
#'

vennlist <- reactive({
  if (is.null(csvf()))
    return(NULL)
  mycont = Vennlist(pval = csvf()[[3]], user_cont())
  return(mycont)
})

#' Vennplot is a reactive function that return a plot object or a link if the user want to display more tha  5sets
#'
#' @param Vennploted a reactive object
#'
#' @return \Vennplot a reactive object to be plot
#'

Vennplot <- reactive({
  
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
  g = Vennfinal(vennlist(), user_cont(), cex = input$vennsize)
  

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
    output$sorry <- renderUI({tagList("You're trying to plot more than 5 sets, download the csv file and use the following tool", url)})
    
    }
  })
  
  return(Vennploted())
})


output$contout <- renderUI(
  checkboxGroupInput(
    inputId = "cont" ,
    label =  "Choose your comparison",
    choices = colnames(adjusted()[[1]][,-1]),
    selected = colnames(adjusted()[[1]][,-1])
    #choices = colnames(adjusted()[[1]][,-1][,-c(indnull())]),
    #selected = colnames(adjusted()[[1]][,-1][,-c(indnull())])
  )
)

observeEvent(input$allCont, {
  updateCheckboxGroupInput(
    session,
    "cont",
    label = "Choose your comparison",
    #choices = colnames(adjusted()[[1]][,-1][,-c(indnull())]),
    #selected = colnames(adjusted()[[1]][,-1][,-c(indnull())])
    choices = colnames(adjusted()[[1]][,-1]),
    selected = colnames(adjusted()[[1]][,-1])
  )
})

observeEvent(input$noCont, {
  updateCheckboxGroupInput(session,
                           "cont",
                           label = "Choose your comparison",
                           #choices = colnames(adjusted()[[1]][,-1][,-c(indnull())])
                           choices = colnames(adjusted()[[1]][,-1])
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

  mysel = (subset(adjusted()[[1]],
                  select = choix_cont()))
  return(mysel)
})

output$downloadvenn <- downloadHandler(
  filename = function() {
    "myvenn.csv"
  },
  content = function(fname) {
    write.table(
      myventocsv(vennlist(), user_cont()),
      fname,
      na = "",
      row.names = F,
      col.names = T,
      append = TRUE,
      sep = ";"
    )
  }
)


# 
# user_length<- reactive({
#   return(length(user_cont()))
# })

