#########################################
######## PCA part                       #
#########################################

observe({
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  
  
  output$individuselpca <- renderUI(
    checkboxGroupInput(
      inputId = "indivpca" ,
      label = NULL,
      #label =  "Choose your group to visualize",
      choices =  levels(csvf()[[2]]$Grp),
      selected = levels(csvf()[[2]]$Grp),
      inline   = groupinline
      
    )
  )
  
})
# Select all groups
observeEvent(input$allIndividuspca, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "indivpca",
    label = "Choose your group to visualize",
    choices =  levels(csvf()[[2]]$Grp),
    selected = levels(csvf()[[2]]$Grp),
    inline = groupinline
  )
})

# Unselect all groups
observeEvent(input$noIndividuspca, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "indivpca",
    label = "Choose your group to visualize",
    choices =  levels(csvf()[[2]]$Grp),
    inline = groupinline
  )
})



#' choix_grpca is a reactive function in the aim of selecting different groups
#'
#' @param indivpca a vector input corresponding to the selected groups
#'
#' @return  a reactive value of type character for the different groups selected
#'
#' @export

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
#' @export


list_ind <- reactive({
  return(list(input$indivpca))
})



#' new_grouppca is a reactive function that select specific groups in the data frame
#' @param heatm  a clickable input button
#' @param csvf a Data frame corresponding to the pData table
#'
#' @return new_grouppca a reactive factor with the corresponding groups selected
#'
#' @export


new_grouppca <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grpca(), ]
})


#' PCAres is a reactive function that computed a PCA of non-normalized data
#'
#' @param csvf a data frame corresponding to the WorkingSet
#' @param new_datapca a reactive data frame
#'
#' @return PCAres a reactive data frame with PCA attributes
#'
#' @export


PCAres <- reactive({
  req(csvf())
  if (is.null(csvf()[[1]]))
    return(NULL)
  
  mypca = res.pca(new_datapca(), scale = F)
  return(mypca)
})


#' Scree_plot is a reactive function which aim is to display the eigenvalues of the data
#'
#' @param PCAres a reactive data frame with PCA attributes
#'
#' @return Screeplot a reactive plot
#'
#' @export

Scree_plot <- reactive({
  req(PCAres())
  mybar = eboulis(PCAres())
  return(mybar + theme_classic())
  
})



#' new_datapca is a reactive function that aim is to select specific individuals in the data frame
#'
#' @param csvf Data frame corresponding to the Workingset
#'
#' @return new_datapca a reactive data frame
#'
#' @export
#'


new_datapca <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  #subset(csvf()[[1]],select = choix_individus())
  select(csvf()[[1]], as.character(factor(new_grouppca()$X)))
})


output$savescre <- downloadHandler(filename <- function() {
  paste0(basename(file_path_sans_ext(projectname())), '_screeplot.png', sep =
           '')
},
content <- function(file) {
  png(
    file,
    width = 1200,
    height = 1200,
    units = "px",
    pointsize = 12,
    res = 100
  )
  
  plot(Scree_plot())
  dev.off()
})



output$eigpca <- renderPlot({
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(length(new_grouppca()) > 0, 'You need to select groups!') %next%
      need(length(unique(
        new_grouppca()$Grp
      )) > 1, 'You need to select more than one group!')
  )
  
  plot(Scree_plot())
  
})


#' labeled is a reactive function which aim is to display or not the labels in the PCA render plot
#'
#' @param label a boolean input
#'
#' @return Labeled a reactive  boolean depending of the user's choice to display or not the labels
#'
#' @export

labeled <- reactive({
  if (input$label == T)
    showlab = "all"
  else
    showlab = "none"
  
  return (showlab)
})


output$PCA <- renderPlot({
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(length(unique(
        new_grouppca()$Grp
      )) > 0, 'You need to select groups!') %next%
      need(length(unique(
        new_grouppca()$Grp
      )) > 1, 'You need to select more than one group!')
  )
  
  plot(PCAplot() + theme_minimal())
  
})


output$savepca <- downloadHandler(filename <- function() {
  paste0(basename(file_path_sans_ext(projectname())), '_pca.', input$formpca, sep =
           '')
},
content <- function(file) {
  if (input$formpca == "pdf")
    
    pdf(file,
        width = 12,
        height = 12,
        pointsize = 12)
  
  
  else if (input$formpca == "png")
    
    png(
      file,
      width = 1200,
      height = 1200,
      units = "px",
      pointsize = 12,
      res = 100
    )
  else
    eps(file,
        width = 12,
        height = 12,
        pointsize = 12)
  
  
  plot(PCAplot())
  dev.off()
})
