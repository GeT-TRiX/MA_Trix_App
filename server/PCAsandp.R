#########################################
######## PCA part                       #
#########################################

output$individuselpca <- renderUI( 
  checkboxGroupInput(
    inputId = "indivpca" ,
    label =  "Choose your group to visualize",
    choices =  levels(csvf()[[2]]$Grp),
    selected = levels(csvf()[[2]]$Grp)
    
  )
)
# Select all groups
observeEvent(input$allIndividuspca, {
  updateCheckboxGroupInput(
    session,
    "indivpca",
    label = "Choose your group to visualize",
    choices =  levels(csvf()[[2]]$Grp),
    selected = levels(csvf()[[2]]$Grp)
  )
})

# Unselect all groups
observeEvent(input$noIndividuspca, {
  updateCheckboxGroupInput(session,
                           "indivpca",
                           label = "Choose your group to visualize",
                           choices =  levels(csvf()[[2]]$Grp))
})



#' choix_grp is a reactive function which aim is to select/unselect groups
#'
#' @param input'$indiv' specific of the individuals data frame
#'
#' @return a reactive  character value for the different individuals selected
#'

choix_grpca <- reactive({
  req(input$indivpca)
  
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  
  return(input$indivpca)
})



#' list_ind is a reactive function in the aim of having selected groups in a list
#'
#' @param input specific of the individuals data frame
#'
#' @return a reactive list for the different individuals selected
#'


list_ind <- reactive({
  return(list(input$indivpca))
})



#' new_group is an eventreactive function that select specific groups in the data frame
#'
#' @param csvf Data frame corresponding to the pData table
#'
#' @return \newgroup an eventreactive factor with the corresponding groups selected
#'

#new_group <- eventReactive(input$heatm, {
new_grouppca <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grpca(),]
})


#' PCAres is a reactive function that computed a PCA of non-normalized data
#'
#' @param csvf a data frame corresponding to the WorkingSet
#'
#' @return \PCAres a reactive data frame with PCA attributes
#'

PCAres <- reactive({
  req(csvf())
  if (is.null(csvf()[[1]]))
    return(NULL)

  mypca = res.pca(new_datapca(), scale =F)
  return(mypca)
})


#' Scree_plot is a reactive function which aim is to display the eigenvalues of the data
#'
#' @param PCAres a reactive data frame with PCA attributes
#'
#' @return \Screeplot a reactive plot
#'

Scree_plot <- reactive({
  req(PCAres())
  mybar = eboulis(PCAres())
  return(mybar+theme_classic())
})



#' new_data is a reactive function that aim is to select specific individuals in the data frame
#'
#' @param \csvf Data frame corresponding to the Workingset
#'
#' @return \newdata a reactive data frame with specific columns depending on the user's choices
#'


new_datapca <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  #subset(csvf()[[1]],select = choix_individus())
  select(csvf()[[1]], as.character(factor(new_grouppca()$X)))
})


output$savescre <- downloadHandler(
  
  filename <- function() {
    paste0(basename(file_path_sans_ext(projectname())), '_screeplot.png', sep='')    
  },
  content <- function(file) {
    
    png(file,
        width =1400,
        height = 1400,
        units = "px",
        pointsize= 12,
        res=100
    )
    
    plot(Scree_plot())
    dev.off()
  })



output$eigpca <- renderPlot({
  
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(length(new_grouppca()) >0, 'You need to select groups!')%next%
      need(length(unique(new_grouppca()$Grp)) >1, 'You need to select more than one group!')
      )
  
  plot(Scree_plot())
  
}, width = 1200 , height = 600, res = 100)


#' labeled is a reactive function which aim is to display or not the labels in the PCA render plot
#'
#' @param input$label a boolean
#'
#' @return \labeled a reactive  boolean depending of the user's choice to display or not the labels
#'

labeled <- reactive({
  
  if(input$label == T)
    showlab = "all"
  else 
    showlab = "none"
  
  return (showlab)
})


output$PCA <- renderPlot({

  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(length(unique(new_grouppca()$Grp)) >0, 'You need to select groups!')%next%
      need(length(unique(new_grouppca()$Grp)) >1, 'You need to select more than one group!')
  )
  
  plot(PCAplot())
  
}, width = 1200 , height = 800, res = 100)


output$savepca <- downloadHandler(
  
  filename <- function() {
    paste0(basename(file_path_sans_ext(projectname())), '_pca.png', sep='')    
  },
  content <- function(file) {
    
    png(file,
        width =1400,
        height = 1400,
        units = "px",
        pointsize= 12,
        res=100
    )
    
    plot(PCAplot())
    dev.off()
  })