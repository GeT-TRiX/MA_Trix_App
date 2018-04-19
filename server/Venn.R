

vennlist <- reactive({

  if (is.null(csvf()))
    return(NULL)
  #mycont = Vennlist(pval = csvf()[[3]], adjusted()[[1]][,-1])
  mycont = Vennlist(pval = csvf()[[3]], user_cont())
})



Vennplot <- reactive({
  Vennfinal(vennlist(),user_cont())
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


# choix_cont <- eventReactive(input$vennd, {
#   return(input$cont)
# }, ignoreNULL = F)

choix_cont <- reactive({
  return(input$cont)
})



user_cont <- reactive({

  mysel = (subset(adjusted()[[1]],
                  select = choix_cont()))
  
  return(mysel)
})
