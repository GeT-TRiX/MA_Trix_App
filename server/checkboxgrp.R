#################################
######## Select the groups      #
#################################


output$individusel <- renderUI(
  checkboxGroupInput(
    inputId = "indiv" ,
    label =  "Choose your group to visualize",
    # choices =  colnames(csvf()[[1]][,-1]),
    # selected = colnames(csvf()[[1]][,-1])
    choices =  levels(csvf()[[2]]$Grp),
    selected = levels(csvf()[[2]]$Grp)
    
  )
)

observeEvent(input$allIndividus, {
  updateCheckboxGroupInput(
    session,
    "indiv",
    label = "Choose your group to visualize",
    #choices = colnames(csvf()[[1]][,-1]),
    #selected = colnames(csvf()[[1]][,-1])
    choices =  levels(csvf()[[2]]$Grp),
    selected = levels(csvf()[[2]]$Grp)
  )
})

observeEvent(input$noIndividus, {
  updateCheckboxGroupInput(session,
                           "indiv",
                           label = "Choose your group to visualize",
                           #choices = colnames(csvf()[[1]][,-1]))
                           choices =  levels(csvf()[[2]]$Grp))
})

#' Reactive function in the aim of selecting individuals
#'
#' @param input specific of the individuals data frame
#'
#' @return string of the different individuals selected
#'


# choix_individus <- reactive({
#   return(input$indiv)
# })

choix_grp <- eventReactive(input$heatm, {
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  return(input$indiv)
}, ignoreNULL = F)


# choix_grp <- reactive({
#   return(input$indiv)
# })

#' Reactive function in the aim of having selected individuals in a list
#'
#' @param input specific of the individuals data frame
#'
#' @return a list of the different individuals selected
#'


list_ind <- reactive({
  return(list(input$indiv))
})

# output$indiv <-  renderText({
#   choix_individus()
# })

# output$indiv <-  renderText({
#   my_final <<- paste(choix_grp(),as.character(),  sep=",") 
# })

#' Reactive function that select specific individuals in the data frame
#'
#' @param csv Data frame corresponding to the pData table
#'
#' @return \new_group a new factor with the corresponding individuals from the checkbox with the good levels
#'


#new_group <-reactive(csvf()[[2]][csvf()[[2]]$X %in% choix_individus(),])


new_group <- eventReactive(input$heatm, {
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(),]
}, ignoreNULL = F)

# new_group <- reactive({
#   inFile <- input$file
#   if (is.null(inFile))
#     return(NULL)
#   csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(), ]
# })

#' Reactive function that  select specific individuals in the data frame
#'
#' @param \csv Data frame corresponding to the Workingset
#'
#' @return adj a new data frame with all the adj.P.Val
#'


new_data <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  #subset(csvf()[[1]],select = choix_individus())
  select(csvf()[[1]], as.character(factor(new_group()$X)))
})

