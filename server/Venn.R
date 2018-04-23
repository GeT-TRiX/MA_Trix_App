vennlist<- reactive({
  if (is.null(csvf()))
    return(NULL)
  mycont = Vennlist(pval = csvf()[[3]], user_cont())
  return(mycont)
})
    
Vennplot <- reactive({
  
  Vennploted <- reactive({
  if(length(user_cont()) <= 5){
  g = Vennfinal(vennlist(), user_cont(), cex = input$vennsize)
  return(g)
  }
  else{
    output$image <- renderUI({
      tags$img(src = "https://i.imgur.com/lB5wmMp.png")
    })
    url <- a("venntools", href = "http://bioinformatics.psb.ugent.be/webtools/Venn/", target = "_blank")
    output$sorry <- renderUI({tagList("You're trying to plot more than 7 sets, download the csv file and use the following tool", url)})
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

indnull <- reactive({
    indexnull = which( sapply(vennlist() ,length) == 0)
    return(indexnull)
})


choix_cont <- reactive({
  return(input$cont)
})

user_length<- reactive({
  return(length(user_cont()))
})

user_cont <- reactive({

  mysel = (subset(adjusted()[[1]],
                  select = choix_cont()))
  return(mysel)
})
