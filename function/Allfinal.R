### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
### Licence: GPL-3.0

#########################################
######## Updating a colourInput         #
#########################################


colourpicker::updateColourInput(
  session,
  "col1",
  label = "downregulated genes:",
  value = firstcol,
  showColour = NULL,
  allowTransparent = FALSE,
  allowedCols = c("green", "orange", "blue"),
  returnName = T
)


colourpicker::updateColourInput(
  session,
  "col3",
  label = "upregulated genes:",
  value = lastcol ,
  showColour = NULL,
  allowTransparent = FALSE,
  allowedCols = c("red", "yellow"),
  returnName = T
)

#' col_choice1 is a reactive function that return a character color
#'
#' @param col1 input character color for the highest values
#'
#' @return col_choice1  a reactive value
#'
#' @export
#'

col_choice1 <- reactive({
  return(input$col1)
})

#' col_choice3 is a reactive function that return a character color
#'
#' @param col3 input character color for the lowest values
#'
#' @return  col_choice3 reactive value
#'
#' @export
#'

col_choice3 <- reactive({
  return(input$col3)
})


#' my_intermediate is a reactive function that return a character color
#'
#' @param col_choice1 character color for the lowest values
#' @param col_choice3 character color for the highest values
#'
#' @return inter a reactive character intermediate color between the lowest and the highest values
#'
#' @export
#'


my_intermediate <- reactive({
  
  if (col_choice1() == "green" & col_choice3() == "red")
    inter = "black"
  
  else if (col_choice1() == "orange" & col_choice3() == "red")
    inter = "yellow"
  
  else if (col_choice1() == "blue" & col_choice3() == "red"){#
    user_choice <- eventReactive(input$submit, input$text)
    inter <- user_choice()
  }
  
  else if (col_choice1() == "blue" & col_choice3() == "yellow")
    inter = "black"
  
  else
    inter= NULL
  
  return(inter)
  
})### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0

##################################
######## Modify buttons          #  
##################################



#' global is a ReactiveValues function
#'
#' @param clicked bool set to FALSE
#' @param heatm input triggered by click event
#'
#' @return clicked a boolean which can be TRUE or FALSE
#'
#' @export

global <- reactiveValues(clicked = FALSE)

observe({
  if(length(input$heatm)){
    if(input$heatm) global$clicked <- TRUE
  }
})


output$button <-  renderUI({ 
  if(!is.null(input$heatm) & global$clicked){
    shiny::actionButton("heatm", "Update Heatmap", icon = icon("repeat"), style = "color: #fff; background-color: #b77033; border-color: #b77033")
  }
  else{
    shiny::actionButton("heatm", "Print Heatmap", style = "color: #fff; background-color: #337ab7; border-color: #337ab7")
  }

})### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


#################################
######## Select the comparisons #
#################################

# Render in the UI.R the levels for the pData Group 

observe({
  
groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)  
output$comphm <- renderUI(
  checkboxGroupInput(
    inputId = "selcomphm" ,
    label =  "Choose your comparison",
    choices =  colnames(subsetstat()[[1]]),
    inline = groupinline
  )
)
})


#Select all the contrasts

observeEvent(input$allcomphm, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "selcomphm",
    label = "Choose your comparison",
    choices = colnames(subsetstat()[[1]]),
    selected = colnames(subsetstat()[[1]]),
    inline = groupinline
  )
})

#Unselect all the contrasts
observeEvent(input$nocomphm, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(session,
                           "selcomphm",
                           label = "Choose your comparison",
                           choices = colnames(subsetstat()[[1]]),
                           inline= groupinline
                           )
})


#' selected_test is an eventreactive function in the aim of selecting different comparison after a clickable event
#'
#' @param selcomphm input id corresponding to the checkboxgroup for the different comparisons
#'
#' @return  a reactive value of type character for the different comparisons selected
#'
#' @export

selected_test <- reactive({
  return(input$selcomphm)
})








### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


#############################################################################
######## align group name in the pannel if there is more than 6 groups      #
#############################################################################


# Render in the UI.R the levels for the pData Group

observe({
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  
  output$grpselhm <- renderUI(
    checkboxGroupInput(
      inputId = "grouphm" ,
      label =  "Choose your group to visualize",
      choices =  levels(csvf()[[2]]$Grp),
      selected = levels(csvf()[[2]]$Grp),
      inline = groupinline
    )
  )
  
})

# Select all groups
  
observeEvent(input$allgrphm, {
    
    groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
    
    updateCheckboxGroupInput(
      session,
      "grouphm",
      label = "Choose your group to visualize",
      choices =  levels(csvf()[[2]]$Grp),
      selected = levels(csvf()[[2]]$Grp),
      inline = groupinline
    )
  })


  # Unselect all groups

  observeEvent(input$nogrphm, {
    groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
    updateCheckboxGroupInput(session,
                             "grouphm",
                             label = "Choose your group to visualize",
                             choices =  levels(csvf()[[2]]$Grp),
                             inline = groupinline )
    
  })

  
  
#################################
######## Select the groups      #
#################################  
  


#' list_ind is a reactive function in the aim of having selected groups in a list
#'
#' @param indiv input id corresponding to the checkboxgroup for the different groups
#'
#' @return a reactive list for the different individuals selected
#'
#' @export


list_ind <- reactive({
  return(list(input$grouphm))
})



#' subsetgroup_hm is an eventreactive function that select specific groups in the data frame
#' 
#' @param csvf a Data frame corresponding to the pData table
#' @param grouphm an input value of type character for the different groups selected
#'
#' @return subsetgroup_hm an eventreactive factor with the corresponding groups selected
#'
#' @export


subsetgroup_hm <- reactive({
  req(csvf())
  csvf()[[2]][csvf()[[2]]$Grp %in% input$grouphm, ]
})


#' subsetwset is a reactive function that aim is to select specific individuals in the data frame
#'
#' @param csvf Data frame corresponding to the Workingset
#' @param subsetgroup_hm a reactive factor with the corresponding groups selected
#'
#' @return subsetwset a reactive data frame with specific columns depending on the user's choices
#'
#' @export


subsetwset <- reactive({
  req(csvf())
  select(csvf()[[1]], as.character(factor(subsetgroup_hm()$X)))
})
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



#########################################
######## Colors for the  PCA groups     #
#########################################

#' colspca is a reactive function which aim is to dynamically create widgets depending on the number of groups
#'
#' @param brewer.pal a local list defined in the RcolorBrewer package
#' @param mycolgrppca a dataframe representing the selected groups
#'
#' @return colspca a reactive number of widget-s
#'
#' @export
#' 

colspca <- reactive({
    req(mycolgrppca())
    pcapal = brewer.pal(8,"Dark2") %>%
    list(brewer.pal(10,"Paired")) %>%
    unlist()
  
  lapply(seq_along(unique(mycolgrppca())), function(x) {
    colourInput(
      paste("colpca", x, sep = "_"),
      levels(mycolgrppca())[x],
      pcapal[x],
      allowedCols =  pcapal,
      palette = "limited",
      returnName = T)
  })
})



#' colorfluidpca is a reactive function wich aim is to group colors side by side
#' depending of the number of groups odd or even for  the gui.
#' 
#'
#' @param colspca a reactive number of widget-s
#'
#' @return html code interpreted by shiny
#' 
#' @export
#'


colorfluidpca <- reactive({
  req(colspca())
  lapply(1:length(colspca()), function(i){
    
    j = length(colspca())
    if(length(colspca()) %%2==0){
      if (i %% 2 == 0) {
        fluidRow(column(6, colspca()[[i - 1]]), column(6, colspca()[[i]]))
      }
    }
    else{
      if (i %% 2 ==0 && j!=i) {
        fluidRow(column(6, colspca()[[i - 1]]), column(6, colspca()[[i]]))
      }
      else if (j == i){
        fluidRow(column(6, colspca()[[i]]))
      }
    }
    
  })
  
})



output$myPanelpca <- renderUI({ # display the colourInput in the UI
  colorfluidpca()
})



#' colorspca is a reactive function which aim is to create as many variables as groups
#'
#' @param mycolgrppca  a reactive data frame
#'
#' @return colorspca a reactive  list containing the different variable names
#'
#' @export
#' 


colorspca <- reactive({
  req(mycolgrppca())
  lapply(seq_along(unique(mycolgrppca())), function(i) {
    input[[paste("colpca", i, sep = "_")]]
  })
})

#' mycolgrppca is a reactive function which aim is to display the total number of groups
#'
#' @param csvf a dataframe
#' @param subsetgroup_pca a reactive factor with the corresponding groups selected
#'
#' @return mycolgrppca a reactive reorder dataframe
#'
#' @export

mycolgrppca <- reactive  ({
  req(csvf())
  mygrpcol <- subsetgroup_pca()$Grp %>%
    sort() %>%
    unique() %>%
    droplevels()
  
  return(mygrpcol)
})
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


#########################################
######## Colors for the  groups         #
#########################################

#' mycolgrp is a reactive function which aim is to display the number of groups selected
#'
#' @param subsetgroup_hm a subset data frame of the pData
#'
#' @return mycolgrp a reactive data frame
#'
#' @export

mycolgrp <- reactive  ({
  req(subsetgroup_hm())
  mygrpcol <- subsetgroup_hm()$Grp %>%
    sort() %>%
    unique() %>%
    droplevels()
  
  
  return(mygrpcol)
})

#' cols is a reactive function which aim is to dynamically create widgets in function of the number of groups
#'
#' @param palette a local list defined in the environment
#' @param mycolgrp a dataframe representing the selected groups
#' @param mypaletA a list which contaings the colors values corresponding to the different groups
#'
#' @return cols a reactive number of widget-s
#'
#' @export

cols <- reactive({
  req(mycolgrp())
  if (is.null(mypal()) )
    lapply(seq_along(mycolgrp()), function(i) {
      

      colourInput(
        paste("col", i, sep = "_"),
        levels(mycolgrp())[i],
        palette[i],
        allowedCols =  palette,
        palette = "limited",
        returnName = T)
    })
  
  else 
  lapply(seq_along(mycolgrp()), function(i) {
    
    colourInput(
      paste("col", i, sep = "_"),
      levels(mycolgrp())[i],
      mypaletA()[i],
      allowedCols =  palette,
      palette = "limited",
      returnName = T)
    })
  
})

#' mypaletA is a reactive function which aim is to set colors if the advanced graphical settings are not displays
#'
#' @param colors a list of input for the different user's choice
#'
#' @return mypaletA a reactive list of colors attributed by ranking order to the different groups
#'
#' @export

mypaletA <- reactive  ({
  if (is.null(mypal))
    return(NULL)
  else
    mypal = (colors())
  return(mypal)
})

#' mypal is a reactive function which aim is to unlist the choice of colors
#'
#' @param colors a list of input for the different user's choice
#'
#' @return mypal a reactive  that unlist the colors attributed to the different groups
#'
#' @export

mypal <- reactive({
  unlist(colors())
})


#' colorfluidhm is a reactive function wich aim is to group colors side by side
#' depending of the number of groups odd or even for  the gui.
#' 
#'
#' @param cols a reactive number of widget-s
#'
#' @return html code interpreted by shiny
#' 
#' @export
#'


colorfluidhm <- reactive({
  req(cols())
  lapply(1:length(cols()), function(i){

    j = length(cols())
    if(length(cols()) %%2==0){
      if (i %% 2 == 0) {
        fluidRow(column(6, cols()[[i - 1]]), column(6, cols()[[i]]))
      }
    }
    else{
      if (i %% 2 ==0 && j!=i) {
        fluidRow(column(6, cols()[[i - 1]]), column(6, cols()[[i]]))
      }
      else if (j == i){
        fluidRow(column(6, cols()[[i]]))
      }
    }
    
    })
    
})


output$myPanel <- renderUI({
  
  colorfluidhm()
})



#' colors is a reactive function which aim is to create as many variables as groups
#'
#' @param mycolgrp  a reactive data frame
#'
#' @return colors a reactive  list containing the different variable names
#'
#' @export
#' 

colors <- reactive({
  req(mycolgrp())
  lapply(seq_along(mycolgrp()), function(i) {
    input[[paste("col", i, sep = "_")]]
  })
})### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


#########################################
######## Cut heatmap Part               #
#########################################




#' cutfinal is a reactive function that return heatmap or ggplot2 object
#'
#' @param hmobj$obj heatmap object
#' @param cut a numeric input corresponding to the height where the dendogram is cut
#' @param subsetwset a data frame with specific columns depending on the user's choices
#' @param genename a data frame
#' @param groups a data frame of the choosen groups
#' @param cutcluster a numeric input corresponding to the selected cluster to display
#' @param cutinfo a character input to select the plot to display heatmap, boxplot or stripchart
#'
#' @return a ggplot object or heatmapply object
#'
#' @export
#'

cutfinal <- reactive({
    req(hmobj$obj)

    pdf(NULL)
    cutHeatmaps(
      hmobj$obj,
      height =  hmsize$cut,
      genename = csvf()[[3]],
      exprData = data.matrix(subsetwset()),
      groups = droplevels(subsetgroup_hm()$Grp),
      num = input$cutcluster,
      type = input$cutinfo,
      mypal = unlist(colors())
    )
})


# render to the ui the number of clusted for a define height in function of the current heatmap object
output$cutcluster <- renderUI({
  req(hmobj$obj)
  cut02 = cut( hmobj$obj$rowDendrogram, h = hmsize$cut)
  selectInput("cutcluster",
              "Choose your cluster",
              choices =  seq(1, length(cut02$lower), by = 1))
})


output$event <- renderPrint({ # interactive cursor that shows the selected points
  d <- event_data("plotly_hover")
  if (is.null(d)) "Hover on a point!" else d
})

observe({
  req(hmobj$obj)
  if (req(input$cutinfo) == "Heatmap") {
     output$cutheatmap <- renderPlot({ # Plot/Render an object of class plotly
        cutfinal()
     })

  }
  else{
    output$cutheatmap <- renderPlotly({
      ggplotly(cutfinal())

    })
  }
})


output$savecut <- downloadHandler(

  filename <- function() {
    paste0(basename(file_path_sans_ext(projectname())), '_cutheat.',input$formcut, sep='')
  },
  content <- function(file) {
    if (input$formcut == "pdf")

      pdf(file,
          width = 10,
          height = 10,
          pointsize = 12)


    else if (input$formcut == "png")

      png(file,
          width =1000,
          height = 1000,
          units = "px",
          pointsize= 12,
          res=100
      )
    else
      cairo_ps(filename=file, width=10, height=10,pointsize = 12)

    plot(cutfinal())
    dev.off()
  })
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


###############################
######## Summarise data       #
###############################

#' data_summary is a reactive function that return the indexes for the signficant genes
#'
#' @param csvf data frame
#' @param pval1 a numeric input corresponding to the cutoff pvalue
#' @param method a charactger input for the statistical method selected BH or raw
#'
#' @return \datasummary a reactive data frame with the indexes corresponding to the sigificant genes for 5 Fold change 1.2,2,4,6,10
#'
#' @export

data_summary <- reactive({
  req(csvf())
  myfinalfc(csvf()[[3]], input$pval1, input$method)
})### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



#' subsetstat is a reactive function that return a list containing multiple data frames
#' with the adj.P.val, logFC and P.value selected for the corresponding groups
#'
#' @param csvf Data frame corresponding to the Alltoptable
#'
#' @return subsetstat a reactive list of data frames
#'
#' @export


subsetstat <- reactive({
  
  req(csvf())
  
  
  adj = csvf()[[3]][, grep("^adj.P.Val|^FDR|^padj",
                           names(csvf()[[3]]),
                           value = TRUE), drop= F]
  
  logfc = csvf()[[3]][, grep("^logFC|log2FoldChange|logFC", 
                             names(csvf()[[3]]),
                             value = TRUE), drop= F]
  
  pval = csvf()[[3]][, grep("^P.value|PValue|pvalue",
                            names(csvf()[[3]]),
                            value = TRUE), drop= F]
  
  vecstat = c("^adj.P.Val_","^logFC_","^P.value_") # Put your statistical prefix here for multitest comparisons
  subsetstat = list(adj,logfc,pval)
  for(i in 1:length(subsetstat))
    names(subsetstat[[i]]) = gsub(
      pattern = vecstat[i],
      replacement = "",
      x = names(subsetstat[[i]]),
      perl = T
    )
  
  
  return(subsetstat)
  
})




# subsetcomp is a reactive function that return a list of data frame depending on the selected comparisons
#'
#' @param subsetstat list of three data frame corresponding to the grep of respectively Adj.pval, P.val and logFC columns
#' @param selected_test character corresponding to the defined contrast set by the user
#'
#' @return usergroup a reactive list containing three data frame for each contrast selected
#'
#' @export

subsetcomp <- reactive({ 
  
  req(selected_test(),subsetstat())
  
  subsetedcomp = list()
  for (i in 1:3)
    subsetedcomp[[i]] = (subset(subsetstat()[[i]],
                           select = selected_test()))
  
  return(subsetedcomp)
})



#' subsetDEG is a reactive function that return the indexes for the signficant genes 
#'
#' @param subsetcomp a list of three data frame with rows selected according to the contrasts selected
#' @param intput$fc a numeric FC selected
#' @param input$decidemethod a character method, default = BH
#' @param input$pval a numeric pvalue
#' @param input$maxgen a numeric maxgen, default = NULL
#'
#' @return subsetDEG a reactive data frame with the indexes corresponding to the sigificant genes
#'
#' @export
#' 


subsetDEG <- reactive({
  
  req(subsetcomp())
  
  indexDEG = decTestTRiX(
    subsetcomp()[[1]],
    subsetcomp()[[2]],
    subsetcomp()[[3]],
    DEGcutoff = input$pval,
    FC = input$fc,
    cutoff_meth = input$decidemethod,
    maxDE = input$maxgen)
  
  return(indexDEG)
  
})


#' ### Author: Franck Soubès
#' ### Bioinformatics Master Degree - University of Bordeaux, France
#' ### Link: https://github.com/fsoubes/MA_Trix_App
#' ### Where: GET-TRiX's facility
#' ### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
#' ### Licence: GPL-3.0


myreorderwk <- reactive({ ## add GeneName
  
  req(csvf())
  wkingsetclean <- csvf()[[1]]
  samplesgroup <- factor(csvf()[[2]]$Grp)
  samplesnum <- parse_number(as.character(csvf()[[2]]$X))
  colnames(wkingsetclean)[-1] <- paste(samplesgroup, samplesnum , sep = ".") 
  wkingsetclean$GeneName <- csvf()[[3]]$GeneName
  return(wkingsetclean)
  
})



filenamestrip <- reactive({
  req(csvf(),projectname())
  
  return( paste0(
    basename(tools::file_path_sans_ext(projectname())), # Add cutoff
    '_strip_chart',
    sep = ''
  ))
  
})



filterwkingset <- reactive({   
  
  req(myreorderwk())
  myreorderwk() %>% select(ProbeName,GeneName, sort(names(.[-1]))) %>% slice(., getDegenes()[[1]]) %>% mutate_if(is.numeric, funs(format(., digits = 3)))
  
})




output$orderedwk <- DT::renderDataTable(DT::datatable(filterwkingset(), 
options = list(scrollX = TRUE, 
                  pageLength = 150, 
                  scrollY=550,  
                  stateSave = T,  
                  dom = 'Bfrtip',
                buttons = list(
                 list(extend = 'csv',
                      filename =  filenamestrip()[1]),
                 list(extend = 'pdf',
                      filename = filenamestrip()[1],
                      title = "My Title",
                      header = FALSE)
                )),
selection = 'single', 
 extensions=c("Buttons",'Scroller'),
 filter =c("none"),
rownames= FALSE ))





getDegenes <- reactive({
  
  req(subsetstat(), csvf(), input$pvalstrip)
  
  indexDEG = decTestTRiX(
    subsetstat()[[1]],
    subsetstat()[[2]],
    subsetstat()[[3]],
    DEGcutoff = input$pvalstrip,
    FC = input$fcstrip,
    cutoff_meth = input$decidemethodstrip, maxDE=NULL )
  return(indexDEG)
  
})




callstripgenes <- reactive({
  
  validate(
    need(input$orderedwk_row_last_clicked, 'Search your gene and select the corresponding row'))
  
  req(getDegenes(), filterwkingset(), req(input$orderedwk_row_last_clicked))
  grps <- gsub("[.][0-9]*","",colnames(filterwkingset()[-(1:2)]), perl=T)
  ggp=ggstrip_groups(grps=grps , wSet= filterwkingset() , probesID= input$orderedwk_row_last_clicked)
  
})


output$renderstripgenes <- renderPlot({
  req(callstripgenes())
  plotOutput(callstripgenes())
})


output$savestriplot <- downloadHandler(filename <- function() {
  paste0(
    basename(tools::file_path_sans_ext(projectname())), # add  gene name
    '_',
    selectedstripgene(), 
    '_strip_chart.',
    input$formstrip,
    sep = ''
  )
},
content <- function(file) {
  if (input$formstrip == "pdf")
    
    pdf(file,
        width = 16,
        height = 7,
        pointsize = 12)
  
  else if (input$formstrip == "png")
    png(
      file,
      width = 1600,
      height = 700,
      units = "px",
      pointsize = 12,
      res = 100
    )
  else
    eps(file,
        width = 16,
        height = 7,
        pointsize = 12)
  
  print(callstripgenes())
  
  dev.off()
})


selectedstripgene <- reactive({
  req(input$orderedwk_row_last_clicked)
  
  return(filterwkingset()[input$orderedwk_row_last_clicked,"GeneName"] )
  
})


output$selected_stripgene <- renderText({ 
  paste("You have selected", selectedstripgene(), "gene.")
})



### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


###############################
########heatmap function & co #
###############################

boolhm <- F


output$heatmbool <- reactive({
  boolhm
})

observe({
  print(boolhm)
  
})

outputOptions(output, "heatmbool", suspendWhenHidden = F)

observe({
  req(csvf(),length(selected_test()) >0,input$reactheat == T| global$clicked)
  
  observe({boolhm <<-T}) # modify and lock the bool value to false
  
  output$heatmbool <- reactive({
    boolhm
  })
  
})


#' rowname is a reactive function which aim is to hide or show the rownames
#'
#' @param input$rowname  a boolean radio button input
#'
#' @return rowname a reactive boolean value
#'
#' @export

rowname <- reactive({
  rowname <- switch(input$rowname,
                    hide = F,
                    show = T,
                    F)
  return(rowname)
})

#' colname is a reactive function which aim is to show or hide the colnames
#'
#' @param input$colname  a boolean radio button input
#'
#' @return colname a reactive  reactive boolean value
#'
#' @export

colname <- reactive({
  colname <- switch(input$colname,
                    hide = T,
                    show = F,
                    F)
  return(colname)
})



heatmapobj <- NULL # declare outside the observeEvent
formatidus <- NULL
hmbis <- reactiveValues()
hmboth <- reactiveValues()
hmobj <- reactiveValues()
hmsize <- reactiveValues()


observe({
  
  #' heatmapfinal is an isolate function that only react to a user's click on the heatmap button 
  #' 
  #' @param hmbis a data frame with all the individuals selected
  #' @param subsetDEG  a data frame with the indexes corresponding to the sigificant genes
  #' @param subsetgroup_hm  a data frame with the corresponding groups 
  #' @param workingPath the current user's repository 
  #' @param my_palette a vector of colors 
  #' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
  #' @param Rowdistfun a character value set by the user to defined the method to calculate the dendogram matrix distance for the genes input$dist
  #' @param Coldistfun a character value set by the user to defined the method to calculate the dendogram matrix distance for the contrasts input$dist
  #' @param mycex a numeric value which aim is to change the size of the legend in the heatmap defined by the user input$legsize
  #' @param cexrow  a numeric value to change the size of the police legend for the rows input$rowsize
  #' @param cexcol a numeric value to change the size of the police legend for the columns input$colsize
  #' @param meanGrp a boolean value to compute or not the mean of each contrasts in the heatmap input$meangrp
  #' @param mypal a list of values 
  #' @param showcol a boolean value used to hide or show the colnames input$colname
  #' @param showrow a boolean value used to hide or show the rownames input$rowname
  #' @param genename a data frame 
  #' @param notplot a boolean value for applying dev.off or not on the heatmap
  #' @param rowv  dendogram object
  #' @param ColOrd  positive numbers, used as cex.axis in for the row or column axis labeling
  #' @param gpcol  matrix with colors associated to each groups 
  #' @param gpcolr  matrix with gray color depending on the clusters
  #' @param distfunTRIX function that computes whether euclidian or pearson for Hierarchical Clustering
  #' @param height a numeric object corresponding to the selected cluster to display
  #' @param rastering a graphical boolean
  #' @param geneSet 
  #'
  #' @return  a data frame with the cluster and the corresponding genes 
  #' 
  #' @export
  #' 
  
  heatmapfinal <- function(isplot  = F, israstering = T) {
    if (is.null(my_intermediate()))
      mypal = (colorRampPalette(c("green", "black", "red"))(n = 75))
    else
      mypal = (colorRampPalette(c(
        col_choice1(), my_intermediate(), col_choice3()
      ))(n = 75))
    
    plotHeatmaps(
      
      isolate(hmbis()[[1]]),
      geneSet =  isolate(hmbis()[[7]]),
      droplevels(subsetgroup_hm()$Grp),
      workingPath = wd_path,
      my_palette = (colorRampPalette(
        c(col_choice1(), my_intermediate(), col_choice3())
      )(n = 75)),
      mycex = input$legsize ,
      cexrow = input$rowsize ,
      cexcol = input$colsize ,
      mypal =  unlist(colors()),
      showcol = colname(),
      showrow = rowname(),
      genename =  csvf()[[3]],
      notplot = isplot,
      rowv = hmbis()[[4]],
      ColvOrd = hmbis()[[3]],
      gpcol = hmbis()[[5]],
      gpcolr = hmbis()[[6]],
      distfunTRIX = isolate(hmbis()[[2]]),
      height = hmbis()[[8]], 
      rastering = israstering
    )
    
  }
  
  
  output$warningsheat <- renderPrint({
    validate(need(
      csvf(),
      'You need to import data to visualize to plot the Heatmap' ) %next% 
      need(length(selected_test()) >0, 'You need to select a contrast(s)') %next% 
      need(input$heatm , 'You need to click on the heatmap button down below the heatmap settings')
    )
  })
  
  
  heatid <- input$side
  if (grepl("Heatmap", heatid)) {
    if (input$reactheat == T)
      source(file.path("server", "Plotreact.R"), local = TRUE)$value #
    else
      source(file.path("server", "Plotreact2.R"), local = TRUE)$value #
    
  }
  
  
  output$savehm <- downloadHandler(filename <- function() {
    paste0(basename(file_path_sans_ext(projectname())),
           '_heatmap.',
           input$formhm,
           sep = '')
  },
  content <- function(file) {
    myras = ifelse(input$formhm == "emf", F, T)
    
    if (input$formhm == "emf")
      
      emf(
        file,
        width = 9,
        height = 12,
        pointsize = 12,
        coordDPI = 300
      )
    
    else if (input$formhm == "png")
      png(
        file,
        width = 900,
        height = 1200,
        units = "px",
        pointsize = 12,
        res = 100
      )
    else
      eps(file,
          width = 7,
          height = 9)
    
    if (!is.null(subsetDEG()[[1]]))
      withProgress(message = 'Saving heatmap:',
                   value = 0, {
                     n <- NROW(subsetDEG()[[1]])
                     for (i in 1:n) {
                       incProgress(1 / n, detail = "Please wait...")
                     }
                     heatmapfinal(isplot = F,israstering =myras)
                   })
    dev.off()
    
  })
  
  
  output$downloadcut <- downloadHandler(
    filename = function() {
      paste(basename(file_path_sans_ext(projectname())),
            '_clustered_hm',
            '.csv',
            sep = '')
    },
    content = function(file) {
      write.csv(ordered(), file, row.names = FALSE)
    }
  )
  

  
  
  ordered <- reactive({
    
    req(hmobj$hm)
  
    if (input$decidemethod == "FDR")
      met = "adj.P.Val_"
    else
      met = "P.value_"
    
    mycont = paste0(met, selected_test())

    ordered = csvf()[[3]] %>% filter(  csvf()[[3]][[1]]  %in% hmobj$hm[[2]] )  %>%
      select(dataid(),  mycont) %>%
      full_join(hmobj$hm[,-1], ., by = dataid() ) %>%
      select(dataid(), GeneName, mycont, cluster) %>%
      mutate_if(is.numeric, funs(format(., digits = 3)))

    rightor = sort(as.integer(rownames(ordered)), decreasing = T)
    ordered = ordered[match(rightor, rownames(ordered)), ]
    
    return(ordered)
  })
 
  grouplength <- reactive({
    
    req(ordered())
    mydfhmgen = (subset( hmobj$hm, !duplicated(subset( hmobj$hm, select=GeneName)))) 
    lengthofmyclust = sapply(1:NROW(unique( hmobj$hm$cluster)),function(x)
    return(length(which(hmobj$hm$cluster ==x)))) %>%  
    cbind(.,sapply(1:NROW(unique( hmobj$hm$cluster)),function(x)
    return(length(which(mydfhmgen$cluster ==x))))) %>% as.data.frame() %>%
    setNames(.,c("total number of probes","total number of genes")) 
    rownames(lengthofmyclust) <- sapply(1:NROW(unique(hmobj$hm$cluster)), function(x)
    return(paste("cluster", x)))
    
    lengthofmyclust <- rbind(lengthofmyclust,c(sum(unlist(lengthofmyclust$`total number of probes`)),sum(unlist(lengthofmyclust$`total number of genes`))))
    rownames(lengthofmyclust)[length(rownames(lengthofmyclust))]<- "total"
    
    return(lengthofmyclust)
    
  })
  
  output$totalgenbyc <- DT::renderDataTable(DT::datatable(grouplength() )) #
  
  
  output$clustering <- DT::renderDataTable(DT::datatable(ordered() ,  options = list(scrollX = TRUE) ))

  
})

### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


###############################
######## Hide buttons         #
###############################

addTooltip(session, id = "dist", title = "correlation:\n dist = 1-corr", placement = "left", trigger="hover")

shinyjs::onclick("toggleAdvanced",
                 shinyjs::toggle(id = "advanced", anim = TRUE)) ## hide and show event


shinyjs::onclick("toggleAdvancedcolors",
                 shinyjs::toggle(id = "advancedcol", anim = TRUE))

shinyjs::onclick("toggleAdvancedJvenn",
                 shinyjs::toggle(id = "advancedjvenn", anim = TRUE))
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0
### TODO comment


#########################################
######## From Shiny to highcharts       #
#########################################

axisParameters <- list(
  topcatdav = list( min = 0, max = 12, legend = 'Source: <a href="https://www.highcharts.com/"  target="_blank">Plot produce with highcharts</a> and <a href="https://shiny.rstudio.com/" target= "_blank">Shiny</a>', title= "top genes")
)

addpercentpop <- reactive({

  req(myresdavitab())
  
  # paraltest <- myresdavitab()
  # cl <- makeCluster(getOption("cl.cores", 4))
  # clusterExport(cl,c("paraltest"),envir=environment())
  # clusterEvalQ(cl, library(dplyr))
  
  reumdiff = lapply(1:length(myresdavitab()),function(x)return(sapply(length(myresdavitab()[[x]]$Count), function(y){
    return(as.numeric(as.character(myresdavitab()[[x]]$Count))/as.numeric(as.character(myresdavitab()[[x]]$Pop.Hits))*100)})) %>%
      mutate(myresdavitab()[[x]],percent = .)) %>% bind_rows()
  
  # d = parLapply(cl, 1:length(paraltest),function(x)return(sapply(length(paraltest[[x]]$Count), function(y){
  #   return(as.numeric(as.character(paraltest$Count))/as.numeric(as.character(paraltest[[x]]$List.Total))*100)})) %>%
  #     mutate(paraltest[[x]],percent = .))
  # d = rbind.fill(d)
  # stopCluster(cl)
  # d
})

dfenrichtojson <- reactive({
  
  req(addpercentpop())
  param <- list(search= "Fold.Enrichment", n_points=length(addpercentpop()$Fold.Enrichment), x_start=min(as.numeric(addpercentpop()$Fold.Enrichment)))
  filtered <- addpercentpop()
  return(DftoHighjson(filtered,param))
  
})


observe({
  
  req(dfenrichtojson())
  newData <- c(axisParameters$topcatdav, list(series=dfenrichtojson()))
  islab = input$addlabelhigh 
  session$sendCustomMessage(type="updateVariable", newData) # send to javascript data
  session$sendCustomMessage("handler1", islab) #send to javascript labels
})
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



#########################################
######## PCA part                       #
#########################################

observe({
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  
  output$grpselpca <- renderUI(
    checkboxGroupInput(
      inputId = "groupca" ,
      label = NULL,
      choices =  levels(csvf()[[2]]$Grp),
      inline   = groupinline
    )
  )
})

# Select all groups
observeEvent(input$allgrpca, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "groupca",
    label = "Choose your group to visualize",
    choices =  levels(csvf()[[2]]$Grp),
    selected = levels(csvf()[[2]]$Grp),
    inline = groupinline
  )
})

# Unselect all groups
observeEvent(input$nogrpca, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "groupca",
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
  req(csvf())
  req(input$groupca)
  return(input$groupca)
})




#' list_ind is a reactive function in the aim of having selected groups in a list
#'
#' @param input specific of the individuals data frame
#'
#' @return a reactive list for the different individuals selected
#'
#' @export


list_ind <- reactive({
  return(list(input$groupca))
})



#' subsetgroup_pca is a reactive function that select specific groups in the data frame
#' @param heatm  a clickable input button
#' @param csvf a Data frame corresponding to the pData table
#'
#' @return subsetgroup_pca a reactive factor with the corresponding groups selected
#'
#' @export


subsetgroup_pca <- reactive({
  req(csvf())
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grpca(),]
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
  req(csvf())
  select(csvf()[[1]], as.character(factor(subsetgroup_pca()$X)))
})


output$savescre <- downloadHandler(filename <- function() {
  paste0(basename(file_path_sans_ext(projectname())), '_screeplot.png', sep =
           '')
},
content <- function(file) {
  png(
    file,
    width = 1200,
    height = 1000,
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
      need(length(input$groupca) >0 ,'You need to select groups!') %next%
      need(length(unique(
        subsetgroup_pca()$Grp
      )) > 1, 'You need to select more than one group!')
  )

  plot(Scree_plot())

},  height = plotHeight)

js$calcHeight()


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
        subsetgroup_pca()$Grp
      )) > 0, 'You need to select groups!') %next%
      need(length(unique(
        subsetgroup_pca()$Grp
      )) > 1, 'You need to select more than one group!')
  )

  plot(PCAplot() + theme_minimal())

},  height = plotHeight)


output$PCAvarender <- renderPlot({
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(length(unique(
        subsetgroup_pca()$Grp
      )) > 0, 'You need to select groups!') %next%
      need(length(unique(
        subsetgroup_pca()$Grp
      )) > 1, 'You need to select more than one group!')
  )

  plot(PCAvarplot())

},  height = plotHeight)



output$savepca <- downloadHandler(filename <- function() {
  paste0(basename(file_path_sans_ext(projectname())), '_pca.', input$formpca, sep = '')
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
      width = 2500,
      height = 2500,
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
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


###############################
########  PCA function        #
###############################


#' PCAplot is a function that return a factoextra object of PCA type
#'
#' @param brew.pal a color object from the RcolorBrewer package
#' @param PCAres a data frame with PCA attributes
#' @param label a boolean value depending of the user's choice to display or not the labels
#' @param habillage a data frame corresponding to the pData
#' @param addEllipseda a boolean input to add ellipse to the data distribution  for the different groups
#' @param ellipse.level a numeric value set to 0.8
#' @param repel a boolean input to avoid overlaps between the label points
#' @param axes a numeric input vector of length 2 specifying the dimensions to be plotted
#' @param labelsize a numeric input representing the police size to display for the different labels
#' @param pointsize a numeric input representing the diameter of each points displayed in the graph
#' @param mean.point a boolean input use to display or not the mean point
#'
#' @return p a factoextra object
#'
#' @export


PCAplot <- function() {
  
  pcapal = brewer.pal(10, "Paired") %>%
    list(brewer.pal(8, "Dark2")) %>%
    unlist()
  
  empty <- reactive ({
    if (is.null(colorspca()[[1]])) {
      palpca = pcapal
    }
    else
      palpca = unlist(colorspca())
    return(palpca)
    
  })
  
  p <- fviz_mca_ind(
    PCAres(),
    label = labeled(),
    habillage = droplevels(subsetgroup_pca()$Grp),
    addEllipses = input$ellipse ,
    ellipse.level = 0.8,
    repel = input$jitter,
    axes = c(as.integer(input$dim1), as.integer(input$dim2)),
    labelsize = input$labelsiize,
    pointsize = input$pointsiize,
    mean.point = input$meanpoint
  )
  
  return(p + scale_color_manual(values = empty()))
}



### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


###############################
######## not Reactive side    #
###############################


shinyjs::enable("heatm")



#' hmbis is an event reactive function that pre-computed hierarchical clustering on microarray data
#'
#' @param subsetwset a data frame with all the individuals selected
#' @param subsetDEG  a data frame with the indexes corresponding to the sigificant genes
#' @param subsetgroup_hm  a data frame with the corresponding groups
#' @param workingPath the current user's repository
#' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
#' @param Rowdistfun a function used to compute the distance for the rows
#' @param Coldistfun a function used to compute the distance for the columns
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#' @param genename a data frame
#'
#' @return  a list of objects which aim is to being passed as argument in the plotHeatmaps function
#'
#' @export
#'

hmbis <- reactive({
  withProgress(message = 'Performing the hierarchical clustering:', # Add sliderbar when loading heatmap
               value = 0,
               {
                 n <- NROW(subsetDEG()[[1]]) #number of row in the subsetDEG dataframe
                 for (i in 1:n) {
                   incProgress(1 / n, detail = "Please wait...")
                 }
                 
                 truncatedhat(
                   data.matrix(subsetwset()),
                   subsetDEG()[[1]],
                   droplevels(subsetgroup_hm()$Grp),
                   workingPath = wd_path,
                   k = input$clusters,
                   mypal = unlist(colors()),
                   Rowdistfun = input$dist ,
                   Coldistfun = input$dist,
                   meanGrp = input$meangrp,
                   genename =  csvf()[[3]],
                   algo = input$algomet
                 )
                 
               })
})


observeEvent(input$heatm, {
  
  if (is.null(my_intermediate())) {
    pdf(NULL) 
    heatmapfinal(isplot = F)
    shinyjs::alert("The colors defined for the heatmap are not fit to be together!!")
    return(NULL)
  }
  else
    output$distPlot <- renderPlot({
      isolate({

        hmbis()
        hmsize$cut <- hmbis()[[8]]
        
        observe({
          boolhm <<- T
        })
        
        output$heatmbool <- reactive({
          boolhm
        })
        
        withProgress(message = 'Plotting heatmap:', # Add sliderbar when loading heatmap
                     value = 0,
                     {
                       n <- NROW(subsetDEG()[[1]]) #number of row in the subsetDEG dataframe
                       for (i in 1:n) {
                         incProgress(1 / n, detail = "Please wait...")
                       }
                       hmboth$tot <- heatmapfinal(isplot = F)
                       hmobj$hm <- hmboth$tot[[1]]
                       hmobj$obj <- hmboth$tot[[2]]
                     })
      })
      
    })
})
  


###############################
######## Reactive side        #
###############################

shinyjs::disable("heatm")


#' hmbis is an event reactive function that pre-computed hierarchical clustering on microarray data 
#'
#' @param subsetwset a data frame with all the individuals selected
#' @param subsetDEG  a data frame with the indexes corresponding to the sigificant genes
#' @param subsetgroup_hm  a data frame with the corresponding groups 
#' @param workingPath the current user's repository 
#' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
#' @param Rowdistfun a function used to compute the distance for the rows
#' @param Coldistfun a function used to compute the distance for the columns
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#' @param genename a data frame
#'
#' @return  a list of objects which aim is to being passed as argument in the plotHeatmaps function
#' 
#' @export
#' 


hmbis <- reactive( {

      truncatedhat(
        data.matrix(subsetwset()),
        subsetDEG()[[1]], 
        droplevels(subsetgroup_hm()$Grp),
        workingPath = wd_path,
        k = input$clusters,
        mypal = unlist(colors()),
        Rowdistfun = input$dist ,
        Coldistfun = input$dist,
        meanGrp = input$meangrp,
        genename =  csvf()[[3]],
        algo = input$algomet
        
      )
    
    
  
})




output$distPlot <- renderPlot({
    
    validate(need(
      csvf(),
      'You need to import data to visualize to plot the Heatmap' ) %next%
        need(length(selected_test()) >0, 'You need to select a contrast(s) with reactivity triggered you dont need to click on the update heatmap button')
    )
    
    if(is.null(my_intermediate())){
      
      isolate({pdf(NULL) 
        heatmapfinal(isplot = F)
        })
      shinyjs::alert("The colors defined for the heatmap are not fit to be together!!")
      return(NULL)
      
    }
    
    
    if ( input$reactheat == T){
      
      hmbis()
      hmsize$cut <- hmbis()[[8]]
      observe({boolhm <<-T})
      output$heatmbool <- reactive({
        boolhm
      })

      hmboth$tot <- heatmapfinal()
      hmobj$hm <- hmboth$tot[[1]]
      hmobj$obj <-hmboth$tot[[2]]
      
    }
    else{
      
      validate(
          need(input$heatm, 'You are not in reactive mod anymore, please click on the heatmap button in order to update the heatmap' )
      )
      NULL
    }

})





### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


##########################################
######## Plot the data frame wiht input ##
##########################################

output$designtab <- renderDataTable(csvf()[[2]]) # Data frame corresponding to the pData


output$data_summary <- renderDataTable(data_summary()) # Summary of the significant genes depending on the pvalue with FC set to (1.2,2,4,6,10)


 
observe({
  
  req(input$dispvenn, vennfinal())
  
  if(input$dispvenn == "probes" &&  (is.null(input$filteredcompjv) || input$filteredcompjv == "" ))
    output$vennresinter <- DT::renderDataTable(DT::datatable(vennfinal()[[1]], list(lengthMenu =  c('5', '10', '15')),extensions=c("Buttons",'Scroller'),  options = list(scrollX = TRUE,  pageLength = 150, scrollY=530,  stateSave = T,  dom = 'Bfrtip',
                                                                                                                                      buttons = c( 'csv',  'pdf' )) ), server = F)
  else if (input$dispvenn == "genes"  &&  (is.null(input$filteredcompjv) || input$filteredcompjv == "" ))
    output$vennresinter <- DT::renderDataTable(DT::datatable(vennfinal()[[2]], list(lengthMenu =  c('5', '10', '15')),extensions=c("Buttons",'Scroller'), options = list(scrollX = TRUE ,pageLength = 150, scrollY=530,  stateSave = T,dom = 'Bfrtip',
                                                                                                                                      buttons = c( 'csv',  'pdf' ))), server = F)
  else
    output$vennresinter <- DT::renderDataTable(DT::datatable(topngenesDT(), list(lengthMenu =  c('5', '10', '15')),extensions=c("Buttons",'Scroller'),  options = list(scrollX = TRUE ,pageLength = 150, scrollY=530,  stateSave = T,dom = 'Bfrtip',
                                                                                                                                   buttons = c( 'csv',  'pdf' ))), server = F)
    
})
    
rounddavidtable <- reactive({
  req(davidwebservice)
  return(lapply(1:NROW(davidwebservice()), function(x)
  return(format(davidwebservice()[[x]], digits = 3))))
})

output$davidgo <- DT::renderDataTable(DT::datatable(rounddavidtable()[[as.numeric(input$cutgo)]][, -9] , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons'),server=F)

#' myrenderedtop is a reactive function which aim is to display or not the labels in the PCA render plot
#'
#' @param csvf a data frame
#'
#' @return  a reactive data frame
#'
#' @export


myrenderedtop <- reactive({
  req(csvf())
  csvf()[[3]] %>% 
    select_if(.,grepl("^Probe|^Tran|^Gene|^logFC|^P.value|^adj.P", colnames(.))) %>%
    mutate_if(is.numeric, funs(format(., digits = 3)))
})

output$subsetgroup_hm <- DT::renderDataTable(DT::datatable(myrenderedtop() , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons',filter =c("none")))


observe({
  req(!is.null(length(myresdavitab())))
output$cat_MF <- DT::renderDataTable({
  if(is.null(myresdavitab()[[1]]))
    return(NULL)
  else DT::datatable(myresdavitab()[[1]] , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons') })
})

observe({
  req(length(myresdavitab())>1)
output$cat_BP <- DT::renderDataTable({
DT::datatable(myresdavitab()[[2]] ,options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons') })})

observe({
req(length(myresdavitab())>2)

output$cat_CC <- DT::renderDataTable({
 DT::datatable(myresdavitab()[[3]] , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons' ) })
})


output$debug <- DT::renderDataTable({
  req(Venncluster())
  summary(Venncluster()) %>% as.data.frame() %>% mutate_if(is.numeric, funs(format(., digits = 3)))
})

observe({
  req(length(myresdavitab())>3)

  output$cat_KEGG <- DT::renderDataTable({
  DT::datatable(myresdavitab()[[4]] , options = list(scrollX = TRUE, dom = 'Bfrtip', buttons = I('colvis')), extensions = 'Buttons') })
})
#https://shiny.rstudio.com/gallery/chat-room.html


# Create a spot for reactive variables specific to this particular session
sessionVars <- reactiveValues(username = "")

# Track whether or not this session has been initialized. We'll use this to
# assign a username to unininitialized sessions.
init <- FALSE

# When a session is ended, remove the user and note that they left the room. 
session$onSessionEnded(function() {
  isolate({
    vars$users <- vars$users[vars$users != sessionVars$username]
    vars$chat <- c(vars$chat, paste0(linePrefix(),
                                     tags$span(class="user-exit",
                                               sessionVars$username,
                                               "left the room.")))
  })
})

# Observer to handle changes to the username
observe({
  # We want a reactive dependency on this variable, so we'll just list it here.
  input$user
  
  if (!init){
    # Seed initial username
    sessionVars$username <- paste0("User", round(runif(1, 10000, 99999)))
    isolate({
      vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                        tags$span(class="user-enter",
                                                  sessionVars$username,
                                                  "entered the room.")))
    })
    init <<- TRUE
  } else{
    # A previous username was already given
    isolate({
      if (input$user == sessionVars$username || input$user == ""){
        # No change. Just return.
        return()
      }
      
      # Updating username      
      # First, remove the old one
      vars$users <- vars$users[vars$users != sessionVars$username]
      
      # Note the change in the chat log
      vars$chat <<- c(vars$chat, paste0(linePrefix(),
                                        tags$span(class="user-change",
                                                  paste0("\"", sessionVars$username, "\""),
                                                  " -> ",
                                                  paste0("\"", input$user, "\""))))
      
      # Now update with the new one
      sessionVars$username <- input$user
    })
  }
  # Add this user to the global list of users
  isolate(vars$users <- c(vars$users, sessionVars$username))
})

# Keep the username updated with whatever sanitized/assigned username we have
observe({
  updateTextInput(session, "user", 
                  value=sessionVars$username)    
})

# Keep the list of connected users updated
output$userList <- renderUI({
  tagList(tags$ul( lapply(vars$users, function(user){
    return(tags$li(user))
  })))
})

# Listen for input$send changes (i.e. when the button is clicked)
observe({
  if(input$send < 1){
    # The code must be initializing, b/c the button hasn't been clicked yet.
    return()
  }
  isolate({
    # Add the current entry to the chat log.
    if(input$entry != "")
      vars$chat <<- c(vars$chat,
                      paste0(
                        linePrefix(),
                        tags$span(class = "username",
                                  tags$abbr(title = Sys.time(), sessionVars$username)),
                        ": ",
                        tagList(input$entry)
                      ))
    
  })
  # Clear out the text entry field.
  updateTextInput(session, "entry", value="")
})

# Dynamically create the UI for the chat window.
output$chat <- renderUI({
  if (length(vars$chat) > 500){
    # Too long, use only the most recent 500 lines
    vars$chat <- vars$chat[(length(vars$chat)-500):(length(vars$chat))]
  }
  # Save the chat object so we can restore it later if needed.
  saveRDS(vars$chat, "chat.Rds")
  
  # Pass the chat log through as HTML
  HTML(vars$chat)
})### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


url <- reactiveValues()
gores <- reactiveValues()

observe({
  req(url)
  
  output$DAVID <- renderUI({
    shiny::actionButton(
      inputId = 'DAVID',
      "Open DAVID",
      style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
      onclick = paste("window.open(", url$myurl)
    )
  })
})


observe({
  
  #' totaclust is a reactive function which aim is to dynamically return a widget object of selectinput type ranging from 1 to the maximum number of cluster
  #'
  #' @param hmobj data frame of the significant genes associated with the corresponding cluster index
  #'
  #' @return selectInput widget
  #' @export
  #'
  
  
  totalclust <- reactive({
    req(hmobj$hm)
    n <- unique(hmobj$hm$cluster)
    selectInput("cutgo",
                "Choose your cluster",
                choices =  seq(1, NROW(n) , by = 1))
    
  })
  
  
  output$cutgo <- renderUI({
    totalclust()
  })
  
})




#' clustergrep is a reactive function which aim is to return a list of genes for the selected cluster without the non-annotated genes
#'
#' @param hm data frame of the significant genes associated with the corresponding cluster index
#' @param cutgo a numeric input 
#'
#' @return list of genes
#' @export
#'
#' 

clustergrep <- reactive({
  
  req(hmobj$hm, input$cutgo)
  
  genlist <- hmobj$hm[!duplicated(hmobj$hm$GeneName),] %>%
    dplyr::select(cluster, GeneName)   %>%
    filter(cluster == input$cutgo)
  
  mygensymb = genlist$cluster %>%
    length() %>%
    matrix(1, .) %>%
    as.double() %>%
    setNames(genlist$GeneName) %>%
    names() %>% as.list() %>%
    .[lapply(., function(x)
      length(grep("chr", x, value = FALSE))) == 0]
  
  return(mygensymb)
})

#' davidwebservice is an eventreactive function which aim is to querrying the DWS to return a dataframe summary 
#'
#' @param GO clickable event
#' @param hm data frame of the significant genes associated with the corresponding cluster index
#' @param Species list of annotated elements 
#' @param catinfo vector of enrichment categories, BP, CC, MF, Kegg
#'
#' @return data frame 
#' @export
#' 
#' 


davidwebservice <- eventReactive(input$GO, {

    req(hmobj$hm)
    
    withProgress(message = 'Performing GO enrichment:',
                 value = 0, {
                   n <- NROW(50)
                   for (i in 1:n) {
                     incProgress(1 / n, detail = "Please wait...")
                   }
                   library(RDAVIDWebService)
                   
                   timeoutdav <- function(y)
                     if (any(grepl("Read timed out", y)))
                       invokeRestart("muffleWarning")
                   
                   tryCatch({
                     mygodavid = probnamtoentrez(hmobj$hm, Species()[[1]]) %>%
                       davidquery(input$Species, input$catinfo) %>% withCallingHandlers(error = timeoutdav)
                   }, warning = function(e) {
                     warning("David's server is busy")
                     
                     return(cbind("David's server is busy") %>% as.data.frame() %>% setNames("Error"))
                     
                   })
                 })
    
    
    updateTabsetPanel(session, "heatmapmainp",
                      selected = "maingo")
    
    return(mygodavid)
  })



#' davidurl is a reactive function that aim is to return an url of grouped genes 
#'
#' @param clustergrep list of genes
#'
#' @return character 
#' @export
#'

davidurl <- reactive({
  req(clustergrep())
  
  source_python('./python/enrichmurl.py')
  mydavurl = enrichmentdav(clustergrep())
  mygloburl <- paste(`mydavurl`, ",", "'_blank')")
  
  return(mygloburl)
})


observe({
  req(davidurl())
  url$myurl = davidurl()
})


# output$clustgo <- renderPrint({ 
#   validate(
#     need(csvf(), 'You need to import data to visualize the data!') %next%
#       need(input$cutgo,
#         'You need to click on the heatmap button! then on the run GO button'
#       )
#   )
#   gores$obj <- isolate(testad())
#   
#   req(input$cutgo, input$slidergo)
#   x <- input$cutgo
#   if (!is.null(testad()[[as.integer(x)]])) {
#     for (go in input$slidergo[[1]]:input$slidergo[[2]]) {
#       if (Ontology(testad()[[as.integer(x)]][[1]][[go]]) == input$onto) {
#         cat(paste("GOID:", (GOID(
#           gores$obj[[as.integer(x)]][[1]][[go]]
#         ))))
#         cat("\n")
#         cat(paste("Term:", (Term(
#           gores$obj[[as.integer(x)]][[1]][[go]]
#         ))))
#         cat("\n")
#         cat(paste("Ontology:", (Ontology(
#           gores$obj[[as.integer(x)]][[1]][[go]]
#         ))))
#         cat("\n")
#         cat(paste("Definition:", (Definition(
#           gores$obj[[as.integer(x)]][[1]][[go]]
#         ))))
#         cat("\n")
#         cat(paste("Synonym:", (Synonym(
#           gores$obj[[as.integer(x)]][[1]][[go]]
#         ))))
#         cat("\n")
#         
#         cat("--------------------------------------\n")
#       }
#     }
#   }
#   else
#     print("Sorry, no enriched genes for this cluster")
#   
# })


#' myentreztosymb is a reactive function which aim is to convert entrez ID to GENE  the selected rows in the output data table
#'
#' @param davidwebservice data frame 
#' @param cutgo a numeric input 
#' @param davidgo_rows_selected selected rows
#' @param Species list of annotated elements 
#'
#' @return a data frame
#' @export
#'


myentreztosymb <- reactive({
  
  req( davidwebservice())
  
    
  myselectedrows = (davidwebservice()[[as.numeric(input$cutgo)]][input$davidgo_rows_selected, c("Genes", "Term"),  drop = FALSE]) 

  if(length(myselectedrows["Genes"][[1]])>0){
    
    myentreztosymb = lapply(1:NROW(myselectedrows),function(x){
      myselectedrows$Genes[[x]] %>% strsplit( ", ") %>% unlist() %>% mget(x= .,envir = Species()[[2]],ifnotfound = NA) %>%  unlist() %>%
        unique() %>% cbind(myselectedrows$Term[[x]]) %>% as.data.frame() %>% setNames(., c("Genes", "Term"))
    
    })
    
    return(myentreztosymb)
  }
  else{

    return(NULL)
  }
  
})

output$printmessage <- renderPrint({
  req(davidwebservice())
  cat("You can select the rows in the table above in order to display the gene names")
  cat("\n")
  cat("\n")

})


output$printselected <- renderPrint({

  req(myentreztosymb())

    for(i in 1:length(myentreztosymb())){
      cat(paste("GOID and Term: " , unique(myentreztosymb()[[i]]$Term)))
      cat("\n")
      cat("Genes: ")
      cat(paste( myentreztosymb()[[i]]$Genes, collapse = " ,"))
      cat("\n")
      cat("\n")
    }

})


myresdavitab <- reactive({
  req(davidwebservice())
  mygotabres(davidwebservice()[[as.numeric(input$cutgo)]])
})

output$titlegomain <- renderText({
  req(input$GO)
  mytitlevenn <<- print("DAVID Gene Set Enrichment Analysis")
})


output$titlegotop <- renderText({
  req(input$GO)
    mytitlevenn <<- print("Top 10 Significantly Enriched GO and KEGG Terms")
})


output$savegohmdavxlsx = downloadHandler(filename <- function() { paste0(basename(file_path_sans_ext(projectname())), '_go.',"xlsx", sep = '')},
  content = function(file) {
    
    withProgress(message = 'Creation of the xlsx table:',
                 value = 0, {
                   n <- NROW(50)
                   for (i in 1:n) {
                     incProgress(1 / n, detail = "Please wait...")
                   }
    
    
    library(xlsx)
    
    for (i in 1:length(davidwebservice())) {
      if (i == 1)
        write.xlsx(file = file,
                   davidwebservice()[[i]],
                   sheetName = paste("Cluster", i))
      else
        write.xlsx(
          file = file,
          davidwebservice()[[i]],
          sheetName = paste("Cluster", i),
          append = TRUE
        )
      }
    })
    
    
    
  }
)


#' Species is a reactive function which aim is to return annotated packages for a specific genome
#'
#' @param Species character input
#' @param Speciesvenn character input
#'
#' @return
#' @export
#'


Species <- reactive({
  if (input$Species == "Homo sapiens" || input$Speciesvenn == "Homo sapiens") {
    # human
    library("org.Hs.eg.db")
    return(list(org.Hs.egALIAS2EG, org.Hs.egSYMBOL))
  }
  else if (input$Species == "Mus musculus" || input$Speciesvenn == "Mus musculus" ) {
    # Mouse
    library("org.Mm.eg.db")
    return( list(org.Mm.egALIAS2EG, org.Mm.egSYMBOL))
  }
  else if (input$Species == "Danio rerio" || input$Speciesvenn == "Danio rerio") {
    #Zebra fish
    library("org.Dr.eg.db")
    return(list(org.Dr.egALIAS2EG, org.Dr.egSYMBOL))
  }
  else if (input$Species == "Gallus gallus" || input$Speciesvenn == "Gallus gallus") {
    # chicken
    library("org.Gg.eg.db")
    return(list(org.Gg.egALIAS2EG, org.Gg.egSYMBOL))
  }
  else if (input$Species == "equCab2" || input$Speciesvenn == "equCab2") {
    # horse
    library("org.Gg.eg.db")
    return(list(org.Gg.eg.dbALIAS2EG))
  }
  else if (input$Species == "Caenorhabditis elegans" || input$Speciesvenn == "Caenorhabditis elegans") {
    # cC elegans
    library("org.Ce.eg.db")
    return(list(org.Ce.egALIAS2EG, org.Ce.egSYMBOL))
  }
  else if (input$Species == "Rattus norvegicus" || input$Speciesvenn == "Rattus norvegicus") {
    # Rat
    library("org.Rn.eg.db")
    return(list(org.Rn.egALIAS2EG, org.Rn.egSYMBOL ))
  }
  else if (input$Species == "Sus scrofa" || input$Speciesvenn == "Sus scrofa") {
    # Pig
    library("org.Ss.eg.db")
    return(list(org.Ss.egALIAS2EG, org.Ss.egSYMBOL))
  }
  
})
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0

output$myNUM <- renderPrint({ # number of signficant genes in the heatmap produced
  req(subsetDEG())
  if(is.null(subsetDEG()[[1]]))
    return("X")
  else
    cat(length(subsetDEG()[[1]]))
})


output$maxGen <- renderPrint({ # number of signficant genes in the heatmap produced
  req(input$maxgen)
  cat(input$maxgen)
})


output$col <-  renderText({ # Groups selected
  my_final <<- paste(input$grouphm,as.character(),  sep=",") 
  my_final[length(input$grouphm)] <<- gsub(",","",my_final[length(input$grouphm)])
  my_final
})


output$testtt <- renderText({ #Contrast selected
  my_final <<- paste(selected_test(),as.character(),  sep=",") 
  my_final[length(selected_test())] <<- gsub(",","",my_final[length(selected_test())])
  my_final
})


output$myPVAL <- renderText({ #pvalue selected
  input$pval
})


output$myFC <- renderText({ #Fold change value selected, default =1
  input$fc
})

output$myMET <- renderText({ #Method for choosing the signficant genes, default = FDR (BH method)
  input$decidemethod
})

output$myCLUST <- renderText({ #number of clusted selected, default = 3
  input$clusters
})

output$myMAT <- renderText({ #Method for the matrix distance, default = correlation method (pearson)
  input$dist
})

output$myPAL <- renderText({ #Colors selected for the different groups, default see palette in the global environment
  if(is.null(mypal()))
    palette[1:length(input$grouphm)]
  else
    paste(mypal(),as.character(),  sep=",")
})

output$myLEG <- renderText({ #Legend size, default = 0.8
  input$legsize
})

output$myROW <- renderText({#Row size, default = 0.9
  input$rowsize
})
output$myCOL <- renderText({#Col size, default = 0.9
  input$colsize
})


### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


output$venngenes <- renderPrint({ # number of signficant genes in the heatmap produced
  req(input$selcontjv)
  if(input$dispvenn == "probes")
    cat(length(vennfinal()[[1]][[1]]))
  else
    cat(length(vennfinal()[[1]]$GeneName))
})


output$contvenn <- renderText({ #Contrast selected
  my_final <<- paste(colnames(user_cont()),as.character(),  sep=",") 
})

output$continter <- renderText({ #Contrast selected
  my_final <<- paste(vennchoice(),as.character(),  sep=",") 
})

output$totalgenes <- renderText({
  
  req(vennlist()[[1]])
  sum(sapply(vennlist()[[1]],length))
  
})


output$myPVALvenn <- renderText({ #pvalue selected
  input$pvalvenn
})


output$myFCvenn <- renderText({ #Fold change value selected, default =1
  input$fcvenn
})

output$topgenesdf <- renderText({ #Method for choosing the signficant genes, default = FDR (BH method)
  input$topgenes
})

### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



plotHeight <- reactive({
  ifelse(is.null(input$plotHeight), 0, (input$plotHeight/1.25)) ## responsive plot
})


#########################################
######## Loading screen                 #
#########################################

hide(id = "loading-content", anim = TRUE, animType = "fade",time=2)
hide(id = "loading-content-bar", anim = TRUE, animType = "fade",time=2)

observe({
  collapsestate <- input$sidebarCollapsed
  session$sendCustomMessage(type="iscollapse", collapsestate)
})


#########################################
######## Redirection Panel              #
#########################################

observeEvent(input$heatmapanel, {

isolate(
  if (grepl("widgetheat", input$heatmapanel)) {

    updateTabsetPanel(session, "heatmapmainp",
                      selected = "hmmainpan")
  }
  else if (grepl("cutheatmainp", isolate(input$heatmapanel))) {

    updateTabsetPanel(session, "heatmapmainp",
                      selected = "cuthmmainpan")
    
  }
)
})

#########################################
######## FC step 0.1 if FC <2           #
#########################################

observe({
  
  if (input$fcvenn <= 2)
    updateSliderInput(
      session,
      "fcvenn",
      label = "FC treshold",
      value = NULL,
      min = 1,
      max = 10,
      step = .1
    )
  else
    updateSliderInput(
      session,
      "fcvenn",
      label = "FC treshold",
      value = NULL,
      min = 1,
      max = 10,
      step = 1
    )
  
  if (input$fc <= 2)
    updateSliderInput(
      session,
      "fc",
      label = "FC treshold",
      value = NULL,
      min = 1,
      max = 10,
      step = .1
    )
  else
    updateSliderInput(
      session,
      "fc",
      label = "FC treshold",
      value = NULL,
      min = 1,
      max = 10,
      step = 1
    )
  
})

#################################################
######## Download data and reset button heatmap #
#################################################

output$downloadData <- downloadHandler(filename <- function() {
  paste("sampleData", ".zip", sep = '')
},
content <- function(file) {
  file.copy("data/sampleData.zip", file)
},
contentType = "zip")

observeEvent(input$resetAll, {
  reset("form")
})


#########################################
######## Citation packages              #
#########################################

#' mypacklist is a reactive function which aim is to display the different packages used in the current session
#'
#' @param sessionInfo version information about R, the OS and attached or loaded packages. 
#'
#' @return a data frame
#'
#' @export
#' 


mypacklist <- reactive({
  mysess <- sessionInfo()
  dfpack <- names(sessionInfo()$otherPkgs) %>%
    lapply(function(x)
      return(
        paste(mysess$otherPkgs[[x]]$Package, mysess$otherPkgs[[x]]$Version)
      )) %>%
    unlist() %>%
    cbind(., unlist(lapply(names(mysess$otherPkgs), function(x)
      return(paste(mysess$otherPkgs[[x]]$Title))))) %>%
    as.data.frame() %>%
    setNames(c('Version', "Title"))
  
  return(dfpack)
})


observeEvent(input$session, {
  req(mypacklist())
  output$sessinfo <- renderDataTable(mypacklist())
})



#########################################
######## Grep project name              #
#########################################

file_name <- reactive({
  req(csvf())
  inFile <- csvf()[[4]] 
  if (class(inFile)== "character")
    return(tools::file_path_sans_ext(inFile))
  else
    return (tools::file_path_sans_ext(inFile$name))
})

projectname <- reactive({
  req(file_name())
  splitbyunder <- strsplit(file_name(), "_")
  MAindex = grepl("^MA", splitbyunder[[2]])
  isindex = which(MAindex == T)
  outputname = list(splitbyunder[[2]][isindex], MAindex)
  if(length(outputname[[1]]) == 0){
    return(Sys.Date())
  }
  else
    return(outputname)
  
})


dataid <- reactive({
  return(colnames(csvf()[[3]][1]))
})
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


#' vennchoice is a reactive function that return user's selected comparisons
#'
#' @param intscol character input
#'
#' @return character vector
#' @export
#'

vennchoice <- reactive({
  if (is.null (input$intscol))
    return(NULL)
  else
    return(input$intscol)
})


#' venninter is a reactive function which aim is to return a set of lists for each possible logical relations between a finite collection of different sets
#'
#' @param vennlist list of probenames
#' @param user_cont character vector
#'
#' @return multiple lists
#' @export
#'

venninter <- reactive({
  req(vennlist(), user_cont())
  myelist <- setvglobalvenn(vennlist()[[1]], user_cont())
  return(myelist)
})


#' vennfinal is a reactive function which return a list of data frame corresponding to the computationnal mean of each logFC for the possible logical relations between a finite collection of different sets
#' and a data frame with as primary key the probenames associated with the corresponding gene names and logFC
#'
#'
#' @param vennchoice reactive character vector
#' @param subsetstat dataframe subset of the alltoptable
#' @param dispvenn character input between probes and genes
#' @param venninter multiple lists of probenames
#'
#' @return a list of two data frames
#' @export
#'

vennfinal <- reactive({

  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(choix_cont(), 'Set your thresholds and then select your comparison to display the Venn diagram!')%next%
      need(input$selcontjv ,'You need to click on a number (Venn diagram) to display the data table!'))

  reslist = list()
  
  if(!input$Allcont && !input$dispvenn == "genes")
    resfinal <- filterjvenn(input$jvennlist, input$selcontjv, csvf()[[3]],dataid(), input$dispvenn )
  else if (input$Allcont && !input$dispvenn == "genes")
    resfinal <- filterjvenn(input$jvennlist, choix_cont(),  csvf()[[3]], dataid(), input$dispvenn)
  else if (!input$Allcont && input$dispvenn == "genes")
    resfinal <- filterjvenn(input$jvennlist, input$selcontjv,   csvf()[[3]], dataid(),  input$dispvenn, unlist(vennlist()[[1]]))
  else
    resfinal <- filterjvenn(input$jvennlist, choix_cont(), csvf()[[3]] ,dataid(), input$dispvenn, unlist(vennlist()[[1]]))

  if(input$Notanno){
    resfinal <- resfinal %>%  filter(., !grepl("^chr[A-z0-9]{1,}:|^ENSMUST|^LOC[0-9]{1,}|^[0-9]{4,}$|^A_[0-9]{2}_P|^NAP[0-9]{4,}|[0-9]{7,}",GeneName)) %>% as.data.frame()
  }
  
  reslist[[1]] <- resfinal
  if(!input$Allcont)
    mycont =input$selcontjv
  else
    mycont =choix_cont()
  if(input$dispvenn == "genes")
    reslist[[2]] <- meanrankgenes(resfinal, stat ="logFC_", multcomp = mycont , jvenn=  T)
  
  return(reslist)
})




output$venntitle <- renderText({
  req(input$selcontjv)
  if(input$dispvenn == "probes")
    mytitlevenn <<- print(paste("Barplot showing the top ", input$topgenes ," genes"))
  else
    mytitlevenn <<- print(paste("Barplot showing the computationnal logFC mean of the top " ,input$topgenes , " genes before the rendering table"))
})


output$venngenesbef <- renderText({
  req(input$selcontjv)
  if(input$dispvenn == "genes")
  mytitlevenn <<- print(paste("Barplot showing the computationnal logFC mean of the top " ,input$topgenes , " genes after the rendering table"))

})


output$dfvenn <- renderText({
  req(input$selcontjv)
  if(input$dispvenn == "probes")
    mytitlevenn <<- print(paste("Table showing the ProbeNames and GeneNames associated with their respective logFC for the intersection(s) selected"))
  else
    mytitlevenn <<- print(paste("Table showing the GeneNames associated with the average logFC for the intersection(s) selected"))


})

output$dfvennbef <- renderText({
  req(input$selcontjv)
  if(input$dispvenn == "genes")
    mytitlevenn <<- print(paste("Table showing the GeneNames associated with their respective logFC for the intersection(s) selected"))

})


#' venntopgenes is a reactive function which aim is to return the user's input top n genes
#'
#' @param filtertopjvenn numeric input
#'
#' @return numeric input
#' @export
#'

venntopgenes <- reactive({
  if (is.null (input$filtertopjvenn))
    return(NULL)
  else
    return(input$filtertopjvenn)
})



output$downloadvennset = downloadHandler('venns-filtered.csv',
  content = function(file) {
    s = input$vennresinter_rows_all
    if(input$dispvenn == "probes")
      write.csv2(vennfinal()[[1]][s, , drop = FALSE], file)
    else
      write.csv2(vennfinal()[[2]][s, , drop = FALSE], file)
  }
)


#' plottopgenes is an event reactive function which aim is to plot the top n genes selected by the user from the rendering data table
#'
#' @param topdegenes clickable event button
#' @param venntopgenes numeric input
#' @param vennchoice reactive character vector
#' @param vennfinal a list of two data frames
#' @param dispvenn character input between probes and genes
#'
#' @return ggplot object
#' @export
#'

plottopgenes <- eventReactive(input$topdegenes, {
  req(vennfinal(), venntopgenes(), input$selcontjv)

  if(input$Allcont)
    mycont <- paste0("logFC_", choix_cont())
  else
    mycont <- paste0("logFC_", input$selcontjv)
  
  if(input$dispvenn == "probes" &&  (is.null(input$filteredcompjv) || input$filteredcompjv == "" ) )
    myplot <- topngenes(vennfinal()[[1]][input$vennresinter_rows_all, , drop = FALSE],mycont, venntopgenes(), input$dispvenn)
  else if(input$dispvenn == "genes" &&  (is.null(input$filteredcompjv) || input$filteredcompjv == "" ))
    myplot <- topngenes(vennfinal()[[2]][input$vennresinter_rows_all, , drop = FALSE],mycont, venntopgenes(), input$dispvenn)
  else
    myplot <- topngenes(topngenesDT()[input$vennresinter_rows_all, , drop = FALSE],mycont, venntopgenes(), input$dispvenn)
  
    

  return(myplot)
})




observeEvent(input$topdegenes, {
  isolate(output$barplotvenn <- renderPlot({
    req(plottopgenes())
    plotOutput(plottopgenes())
  }))

})


observeEvent(input$topdegenes, {
  isolate(output$barplotvennmean <- renderPlot({
    req(plottopgenesmean(), input$dispvenn == "genes")
    plotOutput(plottopgenesmean())

  }))

})



observe({
  validate(need(csvf(), 'You need to import data to visualize this plot!'))

  output$savebarplot <- downloadHandler(filename <- function() {
    paste0(
      basename(tools::file_path_sans_ext(projectname())),
      '_venn_barplot.',
      input$formvenbar,
      sep = ''
    )
  },
  content <- function(file) {
    if (input$formvenbar == "pdf")

      pdf(file,
          width = 16,
          height = 7,
          pointsize = 12)

    else if (input$formvenbar == "png")
      png(
        file,
        width = 1600,
        height = 700,
        units = "px",
        pointsize = 12,
        res = 100
      )
    else
      eps(file,
          width = 16,
          height = 7,
          pointsize = 12)

    print(plottopgenes())

    dev.off()
  })

})



####################
# Addition for report
# MA0439
####################


filteredcolvenn <- reactive ({
  
  req(vennfinal(), venntopgenes(), input$selcontjv)
  filteredcol = na.omit((as.numeric(gsub("([0-9]+).*$", "\\1", unlist(input$vennresinter_state$order)))))
  if(input$dispvenn == "probes")
    colnamefil = colnames(vennfinal()[[1]][filteredcol])
  else 
    colnamefil = colnames(vennfinal()[[2]][filteredcol])
  
  colnamefil = gsub(
    pattern = "logFC_" ,
    replacement = "",
    x = colnamefil,
    perl = T
  )

  return(colnamefil)
})


topngenesDT <- reactive ({
  
  req(input$filteredcompjv, vennfinal())

  topngenesDT <- csvf()[[3]] %>% select( ProbeName, GeneName, paste0(ifelse(input$filtermethjvenn == "FDR", "adj.P.Val_" , "P.value_"), input$filteredcompjv)) %>% 
  {if (input$dispvenn == "genes") filter ( ., GeneName  %in% vennfinal()[[2]]$GeneName) else filter(., ProbeName  %in% vennfinal()[[1]]$ProbeName)}
  topngenesDT$rank <- topngenesDT %>% select( paste0(ifelse(input$filtermethjvenn == "FDR", "adj.P.Val_" , "P.value_"), input$filteredcompjv)) %>% rank(.) 
  topngenesDT <- topngenesDT %>% arrange( desc(rank) ) %>% top_n(-input$filtertopjvenn, rank) 
  if (input$dispvenn == "genes")
    topngenesDT <-vennfinal()[[2]] %>% filter (GeneName %in% topngenesDT$GeneName)
  else
    topngenesDT <-vennfinal()[[1]] %>% filter (ProbeName %in% topngenesDT$ProbeName)
  
    
 return(topngenesDT)
})


output$filtercompjvenn <- renderUI({
  
  req( input$selcontjv)
  tags$div(
    class = "jvennfiltparam",selectInput('filteredcompjv',
                                     'filter comp', choices = c("", input$selcontjv), selected = ""))
})






### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


observe({
  req(Venncluster())
  updateSliderInput(session, "clusterNumber", max = nrow(summary(Venncluster())))
})

output$saveclusterchoose <- downloadHandler(filename <- function() {
  paste0(basename(file_path_sans_ext(projectname())), '_venn', input$clusterNumber, 'cluster.', input$formvennclus, sep =
           '')
},
content <- function(file) {
  

  if (input$formvennclus == "pdf")

    pdf(file,
        width = 12,
        height = 12,
        pointsize = 12)


  else if (input$formvennclus == "png")

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
    
    
    acyclgo()
    dev.off()
  
})


output$clusterPlot <- renderPlot({
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
    need(choix_cont(), 'Set your thresholds and then select your comparison to display the Venn diagram!')%next%
    need(input$selcontjv ,'You need to click on a number (Venn diagram) to display the data table!') %next%
    need(input$GOvenn ,'You need to click on the run Analysis button!')) 
    req(Venncluster())
    plot2D(Venncluster(), input$clusterNumber)
})



davidtag<- reactive({req(Venncluster())
  davidGODag<-DAVIDGODag(members(Venncluster())[[input$clusterNumber]],  pvalueCutoff=0.1, input$catvenn ) })



acyclgo <- function() {
  req(davidtag())
  result = plotGOTermGraph(g=goDag(davidtag()),r=davidtag(), max.nchar=40, node.shape="ellipse")
  return(result)
}


observe({
  req(acyclgo())
  pdf(NULL)
  if(class(acyclgo()) == "graphNEL")
    shinyjs::disable("saveclusterchoose")
  else
    shinyjs::enable("saveclusterchoose")
})



# output$debug <- renderPrint({
#   req(Venncluster())
#   summary(Venncluster()) %>% as.data.frame()
# })


#' Venncluster is an event reactive function which aim is to interogate David web services database to collect relevant information about the list of genes for a specific intersection
#'
#' @param GOvenn clickable event button
#' @param vennfinal a list of two data frames
#' @param Species list of annotated elements
#' @param Speciesvenn character input
#'
#' @return david enrichment object
#' @export
#'

Venncluster <- eventReactive(input$GOvenn, {

    req(vennfinal())

    withProgress(message = 'Performing GO enrichment:',
                 value = 0, {
                   n <- NROW(50)
                   for (i in 1:n) {
                     incProgress(1 / n, detail = "Please wait...")
                   }
                   library(RDAVIDWebService)

                   timeoutdav <- function(y)
                     if (any(grepl("Read timed out", y)))
                       invokeRestart("muffleWarning")

                   tryCatch({
                     mygodavid = probnamtoentrezvenn(vennfinal()[[1]]$GeneName , Species()[[1]]) %>%
                     davidqueryvenn(input$Speciesvenn) %>% withCallingHandlers(error = timeoutdav)
                   }, warning = function(e) {

                     shinyjs::alert("David's server is busy")
                     warning("David's server is busy")
                     return(NULL)

                   })
                 })
    pdf(NULL)
    return(mygodavid)
  })
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
  
  wrongcol <- function(y)
    if (any(grepl("col2rgb", y)))
      invokeRestart("muffleWarning")
  
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
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


#########################################
######## Venn part                      #
#########################################


output$myVenn <- renderPlot({
  validate(
    need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(
        length(user_cont()) > 0,
        'You need to  select your p-value and then some groups!'
      )
  )
  
  req(Vennplot())
  
  Vennplot()
  
},  height = plotHeight)

observe({
  validate(need(csvf(), 'You need to import data to visualize this plot!'))
  
  output$savevenn <- downloadHandler(filename <- function() {
    paste0(basename(tools::file_path_sans_ext(projectname())),
           '_venn_diagram.',
           input$formven,
           sep = '')
  },
  content <- function(file) {
    if (input$formven == "pdf")
      
      pdf(file,
          width = 12.5,
          height = 12,
          pointsize = 12)
    
    else if (input$formven == "png")
      png(
        file,
        width = 1250,
        height = 1200,
        units = "px",
        pointsize = 12,
        res = 100
      )
    else
      cairo_ps(
        filename = file,
        width = 11,
        height = 11,
        pointsize = 12
      )
    
    
    grid.draw(Vennplot())
    dev.off()
  })
  
})### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


#######################################################
######## grep gene family                             #
#######################################################


genetodisplay <- reactive({
  if(is.null(input$fillvolc))
    return(NULL)
  else{
    if(!input$fillvolc == "")
      mycol = gsub("^\\s+|\\s+$", "", unlist(strsplit(input$fillvolc, ",")))
    else
      mycol = ""
    return(toupper(mycol))
  }
})


family_input <- reactive({
  input$findfamily
})


family_d <- shiny::debounce(family_input, 750) # Delay input debounche also pour search genes



#######################################################
######## parse gene symbol for min and maj            #
#######################################################


familytopdisp <- reactive({
  if(is.null(family_d))
    return(NULL)
  else{
    if(!family_d() == ""){
      genfam = grep(pattern = toupper(family_d()), toupper(csvf()[[3]]$GeneName)) %>% slice(csvf()[[3]],.)%>% select(GeneName)  %>% unlist() %>% as.character() 
    }
    else
      genfam =""
    return(toupper(genfam))
  }
})


#######################################################
######## Desactivate volcano input dep on user choice #
#######################################################


observe({
  
  if(input$findfamily != ""){
    shinyjs::disable("topvolc")
    shinyjs::disable("fillvolc")
  }
  else if(input$fillvolc != ""){
    shinyjs::disable("topvolc")
    shinyjs::disable("findfamily")
  }
  else if(!is.na(input$topvolc)){
    shinyjs::disable("findfamily")
    shinyjs::disable("fillvolc")
  }
  else{
    shinyjs::enable("topvolc")
    shinyjs::enable("findfamily")
    shinyjs::enable("fillvolc")
  }
  
})


#################################################
######## Plot and save volcano                  #
#################################################

volcano <- reactive({
  req(csvf(),input$volcacomp )
  EnhancedVolcano(csvf()[[3]], lab= csvf()[[3]]$GeneName , x = paste0("logFC_",input$volcacomp) ,
                  y = paste0(ifelse(input$method == "FDR", "adj.P.Val_","P.value_"),input$volcacomp),
                  topgenes = input$topvolc,DrawConnectors= T,#DrawConnectors = ifelse(is.na(input$topvolc),T,F),
                  pCutoff = input$volcpval ,FCcutoff = input$volcfc ,transcriptPointSize = input$volcpt,transcriptLabSize = input$volclab,
                  title =  gsub("-"," versus " ,input$volcacomp),cutoffLineType = "twodash", findfamily =  ifelse(familytopdisp() == "" , NA,familytopdisp()),regulationvolc = input$regulationvolc, 
                  displaylab = ifelse(genetodisplay() =="", NA, genetodisplay()),legendLabSize = 10,
                  cutoffLineCol = "black",cutoffLineWidth = 1,legend=c("NS","Log (base 2) fold-change","P value",
                                                                       "P value & Log (base 2) fold-change"))
})

volcboth <- reactiveValues()
volcobj <- reactiveValues()


output$volcanoplot <- renderPlot({
  
  validate(need(csvf(), 'You need to import data to visualize this plot!'))
  
  req(volcano())
  volcboth$tot <- volcano()
  volcobj$plot <- volcboth$tot[[1]]
  volcobj$dt <-volcboth$tot[[2]]
  volcobj$plot
  
},  height = plotHeight)

output$compvolc <- renderUI({
  req(subsetstat())
  selectInput("volcacomp", "Choose a comparison", choices = colnames(subsetstat()[[1]]))
})


output$savevolcano <- downloadHandler(filename <- function() {
  paste0(basename(file_path_sans_ext(projectname())), '_volcano.', input$formvolc, sep ='')
},
content <- function(file) {
  if (input$formvolc == "pdf")
    
    pdf(file,
        width = 12,
        height = 12,
        pointsize = 12)
  
  
  else if (input$formvolc == "png")
    
    png(
      file,
      width = 2500,
      height = 2500,
      units = "px",
      pointsize = 12,
      res = 100
    )
  
  else
    ggsave(file,device=cairo_ps, fallback_resolution = 600)
  
  
  plot(volcano()[[1]])
  dev.off()
})


vocfilt <- reactive({
  req(csvf(), input$topvolc,  volcanocomp())
  volcobj$top <- meanrankgenes(isolate(volcobj$dt), "logFC_" , input$volcacomp,  volcanocomp(), input$regulationvolc  )
  return(volcobj$top)
})


volcanocomp <- reactive({
  return(c(input$volcacomp, input$addvolcacomp))
})

volcplototp <- reactive({
  req(vocfilt())
  
  selcomp <-  paste0("logFC_",  volcanocomp())
  myplot <- topngenes(vocfilt(), selcomp , input$topvolc  ,meandup = "genes")
  return(myplot)
})




observe({

output$barplotvolc <- renderPlot({
  validate(need(input$topvolc, 'Add an input in the max number of genes widget!'))
    req(volcplototp())
    plotOutput(volcplototp())
  })
  
})


observe({
  validate(need(csvf(), 'You need to import data to visualize this plot!'))
  
  output$savevolcplot <- downloadHandler(filename <- function() {
    paste0(
      basename(tools::file_path_sans_ext(projectname())),
      '_venn_barplot.',
      input$formvenbar,
      sep = ''
    )
  },
  content <- function(file) {
    if (input$formvolcbar == "pdf")
      
      pdf(file,
          width = 16,
          height = 7,
          pointsize = 12)
    
    else if (input$formvolcbar == "png")
      png(
        file,
        width = 1600,
        height = 700,
        units = "px",
        pointsize = 12,
        res = 100
      )
    else
      eps(file,
          width = 16,
          height = 7,
          pointsize = 12)
    
    print(volcplototp())
    
    dev.off()
  })
  
})


output$addcompvolc <- renderUI({
  req(input$volcacomp)
  compleft <- names(subsetstat()[[1]])[!names(subsetstat()[[1]]) %in% input$volcacomp]
  selectizeInput(
  'addvolcacomp', 'Add comparison(s) for the barplot', choices = compleft, multiple = TRUE
  )
})
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


# increase loading files

options(shiny.maxRequestSize=128000000)
options(digits=3)

userId <- Sys.getenv("SHINYPROXY_USERNAME")
listofteam <- list("E01", "E05")



cutheatmlist = list( Boxplot = c( `True` = 'Boxplot'), Heatmap=c(`True` = "Heatmap"),
                     Stripchart=c(`Without boxplot`="LB", `With boxplot` = "WB"))


categoerygen = c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY")

# Loading packages

#sudo apt-get install libv8-dev
list.of.packages <- c("AnnotationDbi","shiny","shinythemes","shinyjs","ggplot2","shinyBS","plyr","shinyFiles",
                      "BH","data.table","DT","readr","colourpicker","shinydashboard","heatmaply",
                      "tools","devEMF","R.devices","FactoMineR","factoextra","gplots","V8",
                      "RColorBrewer","foreach","doParallel","gridExtra","plotly","dplyr","reticulate","Hmisc")



new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages,function(x){
suppressPackageStartupMessages(library(x,character.only=TRUE))})

source("css/csstips.R")
source("function/PCA.R")
source("function/heatmtruncated.R")
source("function/formatingtables.R")
source("function/decideTestTrix.R")
source("function/ggstrip_groups.r")
source("function/vennplot.R")
source("function/create_forked_task.R")
source("function/cutheat.R")
source("function/gosearch.R")
source("function/highchartconverter.R")
source("function/EnhancedVolcano.R")
source("./module/csvmodules.R")



################################
######## Chat env             ##
################################

# Globally define a place where all users can share some reactive data.
vars <- reactiveValues(chat=NULL, users=NULL)

# Restore the chat log from the last session.
if (file.exists("chat.Rds")){
  vars$chat <- readRDS("chat.Rds")
} else {
  vars$chat <- "Welcome to MATRiX Chat!"
}

#' Get the prefix for the line to be added to the chat window. Usually a newline
#' character unless it's the first line.
linePrefix <- function(){
  if (is.null(isolate(vars$chat))){
    return("")
  }
  return("<br />")
}

################################
######## Variables env        ##
################################


firstcol = "green"
intercol = "black"
lastcol = "red"
wd_path= getwd()
firstdim = 1
secdim = 2


`%next%` <- shiny:::`%OR%`


palette = brewer.pal(8,"Dark2") %>%
  list(brewer.pal(10,"Paired")) %>%
  unlist()



textInputRow<-function (inputId, label, value = "") {
  div(style="display:inline-block",
      tags$label(label, `for` = inputId),
      tags$input(id = inputId, type = "text", value = value,class="input-small"))
}
# Source https://www.r-bloggers.com/a-shiny-app-serves-as-shiny-server-load-balancer/



lapply(1:60, function(x) {
  tops <- system("top -n 1 -b -u shiny", intern = TRUE)
  if(length(tops) > 0) {
    ids <- grep("R *$", tops)
    header <- grep("%CPU", tops)
    names <- strsplit(gsub("^ +|%|\\+", "", tops[header]), " +")[[1]]
    
    if(length(ids) > 0) {
      dat <- as.data.frame(do.call(rbind, strsplit(gsub("^ *", "", tops[ids]), " +")))
      names(dat) <- names
      info <- as.data.frame(do.call(rbind, lapply(dat$PID, function(pid) {
        netstat <- system(paste("sudo netstat -p | grep", pid), intern = TRUE)
        lsof <- system(paste("sudo lsof -p", pid, "| grep /home/fsoubes"), intern = TRUE)
        users <- length(grep("ESTABLISHED", netstat) & grep("tcp", netstat))
        app <- regmatches(lsof, regexec("home/fsoubes/(.*)", lsof))[[1]][2]
        c(app = app, users = users)
      })))
    } else {
      info <- data.frame(app = "app", users = 0)
    }
    write.table(info, file = "/home/fsoubes/server/monitor.log")
  }  
})

library(dplyr)
monitor <- "monitor.log"
monitor=read.table(file=monitor,header=T) %>% as.data.frame() %>% 
dplyr::filter(., grepl("[0-9]",app))%>% arrange(.,app)
write.csv(FILE,file="monitor.csv")

### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


library(testthat)
library(shinytest)
library(AnnotationDbi)

test_that("Application works", {
  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  expect_pass(testApp(".", compareImages = FALSE))
})
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


shinyjscode <- "
shinyjs.init = function() {
  $(window).resize(shinyjs.calcHeight);
}
shinyjs.calcHeight = function() {
  Shiny.onInputChange('plotHeight', $(window).height());
}
"

shinyServer(function(input, output,session) {


  #######################################################
  ##                                                   ##
  ##                    LOAD FILES                     ##
  ##                                                   ##
  #######################################################

  #source(file.path("server", "csvFile.R"), local = TRUE)$value #
  csvf <- callModule(csvFile, "datafile",stringsAsFactors = FALSE) #  Module for importing data
  
  ##########################################
  ######## Widget update and info         ##
  ##########################################

  source(file.path("server", "Utilities.R"), local = TRUE)$value # Utilities method (packages citations, fc step, zipdownload and panel redirection and project name)

  ##########################################
  ######## HOME page                      ##
  ##########################################

  source(file.path("server", "Datasummary.R"), local = TRUE)$value # Reactive function that return the indexes for the signficant genes
  source(file.path("server", "Rendertable.R"), local = TRUE)$value #  All the output csv except for the heatmap page that are in heatmapshiny source
  source(file.path("server", "Checkboxgrphm.R"), local = TRUE)$value # Heatmap function for select specific groups

  ##########################################
  ######## Volcano page                   ##
  ##########################################

  source(file.path("server", "Volcanoshiny.R"), local = TRUE)$value # Volcano plot

  ################################
  ######## PCA page             ##
  ################################

  source(file.path("server", "PCAshiny.R"), local = TRUE)$value # PCA plot function
  source(file.path("server", "PCAselgroup.R"), local = TRUE)$value # all parameters for pca plot and more
  source(file.path("server", "Colforpca.R"), local = TRUE)$value # Color for pca plot

  ################################
  ######## Venn page            ##
  ################################

  source(file.path("server", "Venn.R"), local = TRUE)$value # Generate the vennlist and select the contrasts and send them to Jvenn
  #source(file.path("server", "Vennrender.R"), local = TRUE)$value # TODO add static Venn
  source(file.path("server", "Venninter.R"), local = TRUE)$value # Selected intersection Venn mean and barplot from the data table with the export. TODO remove not valuable info
  source(file.path("server", "Trackervenn.R"), local = TRUE)$value # Tracker for Venn

  ################################
  ######## Venn GO              ##
  ################################

  source(file.path("server", "Vennquery.R"), local = TRUE)$value # Venn query DAVID

  ################################
  ######## Heatmap page         ##
  ################################

  source(file.path("server", "Checkboxcontrast.R"), local = TRUE)$value #Select the comparison for the heatmap
  source(file.path("server", "Changeheatmbut.R"), local = TRUE)$value # Change the heatmap button color
  source(file.path("server", "Hidevent.R"), local = TRUE)$value # Hide parameters such as number of clusters ... and tooltip for dist
  source(file.path("server", "Heatmapshiny.R"), local = TRUE)$value # Generate the heatmap
  source(file.path("server", "Trackerhm.R"), local = TRUE)$value # Tracker for heatmap parameters
  source(file.path("server", "Computemean.R"), local = TRUE)$value # Mean for heatmap selected groups
  source(file.path("server", "GetDEgenes.R"), local = TRUE)$value # Subset dataframe restable (stat, comp and deg to list of dataframes)
  source(file.path("server", "Backgroundcolor.R"), local = TRUE)$value # Background color for the heatmap
  source(file.path("server", "Colorforhm.R"), local = TRUE)$value # Color for each different group in the hm

  ##########################################
  ######## GO enrichissment               ##
  ##########################################

  source(file.path("server", "Shinygohm.R"), local = TRUE)$value # functional analysis by querying DAVID web service
  source(file.path("server", "Highchartshiny.R"), local = TRUE)$value # Convert output david table to json and send it to bubble.js

  ################################
  ######## cutheatmap page      ##
  ################################

  source(file.path("server", "Cutheatmap.R"), local = TRUE)$value # Generation of boxplot

  ##########################################
  ######## Contact chat                   ##
  ##########################################

  source(file.path("server", "Shinychat.R"), local = TRUE)$value # Chat for the app associated with the file Chat.rds
  source(file.path("server", "Groupstripshiny.R"), local = TRUE)$value # Utilities method (packages citations, fc step, zipdownload and panel redirection and project name)
  
  
  
})
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


###############################
######## dashboardsidebar     #
###############################



makeselectInputbutton <- function(selectInput, buttonId, buttonLabel, size = "default", style = "default"){
  size <- switch(size, `extra-small` = "btn-xs", small = "btn-sm", 
                 large = "btn-lg", "default")
  style <- paste0("btn-", style)
  
  tags$script(HTML(paste0("
                          $(document).ready(function() {
                          var inputElements = document.getElementsByTagName('input');
                          for(var i = 0; i < inputElements.length; i++){
                          var input = inputElements[i];
                          
                          if(input.getAttribute('value') == '", checkboxValue, "'){
                          
                          var button = document.createElement('button');
                          button.setAttribute('id', '", buttonId, "');
                          button.setAttribute('type', 'button');
                          button.setAttribute('class', '", paste("btn action-button", style , size), "');
                          button.appendChild(document.createTextNode('", buttonLabel, "'));
                          
                          input.parentElement.parentElement.appendChild(button);
                          };
                          }
                          });
                          ")))
}

shinyjscode <- "
shinyjs.init = function() {
$(window).resize(shinyjs.calcHeight);
}

shinyjs.calcHeight = function() {
Shiny.onInputChange('plotHeight', $(window).height());
}
"


dbHeader <- dashboardHeader(title = "MATRiX")
dbHeader$children[[2]]$children <-  tags$a(tags$img(src='matrix.png',height='40',width='40',style="margin:5px 0 5px 0;",align='left'),
                                           tags$h3("MATRiX",style="font-family:Purisa; margin:15px 25px 5px 0;color:white; "))


sidebar <- dashboardSidebar( 
  
  useShinyjs(),
  tags$style(type="text/css", inactivity),
  shinyjs::extendShinyjs(text = shinyjscode),
  
  tags$head(
    tags$script(src = "custom.js")),
  div(id = "loading-content-bar",p()),
  
  div(
    id = "matrixapp",
    sidebarMenu(id = "side",
                menuItem("Home", tabName = "Home", icon = icon("home")),
                menuItem("Upload Data", tabName = "Upload", icon = icon("upload")),
                menuItem("PCA", tabName = "PCA", icon = icon("line-chart")),
                menuItem("Venn diagram", tabName = "Venn", icon = icon("line-chart")),
                menuItem("Heatmap", tabName = "Heatmap", icon = icon("line-chart")),
                menuItemOutput("dymMenu"),
                collapsed = TRUE,
                
                tags$a(img(
                  src = "GeT_logo-RVB.png",
                  height = 50,
                  width = 180,
                  style = "position:absolute;bottom:50px;margin:0 0 15px 10px;"
                ) , href="https://get.genotoul.fr/en/", target="_blank"),
                tags$a(img(
                  src = "Logotype-INRA-transparent.png",
                  height = 43,
                  width = 168,
                  style = "position:absolute;bottom:0;margin:0 0 15px 10px;"
                ) , href="https://www6.toulouse.inra.fr/toxalim", target="_blank")
                
    )
  )
)

###############################
######## dashboardbody        #
###############################
###############################
######## Upload Page          #
###############################

body <- dashboardBody(
  
  #tags$head(includeScript("google-analytics.js")),
  tags$style(type="text/css", inactivity),
  useShinyjs(),
  extendShinyjs(text = 'shinyjs.hideSidebar = function(params) { $("body").addClass("sidebar-collapse");
                $(window).trigger("resize"); }'),
  extendShinyjs(text='shinyjs.showSidebar = function(params) { $("body").removeClass("sidebar-collapse");
                $(window).trigger("resize"); }'),
  tags$style(HTML("
                  .tabbable > .nav > li > a[data-value='hmpan'] {background-color: red;   color:white}
                  .tabbable > .nav > li > a[data-value='cutpan'] {background-color: blue;  color:white}
                  ")),
    
  includeCSS("./css/style.css"),
  div(
    id = "loading-content",
    br(),br(),
    br(),
    h2("Please wait while MATRiX is loading...")),
  div(
    id = "matrixapp",
    tabItems(
      tabItem(tabName = "Home",
              fluidRow(
                column(width=9,
                       div(style="width:100% ;max-width: 1500px; height: 1500px max-height: 2200px;",id = "homepage",
                           
                           tabBox(title="Welcome to MATRiX", width=NULL,id = "homepage",
                                  
                                  tabPanel("About", style = "background-color: #ffffff;",
                                           tags$h3("MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics."),
                                           p("This project initiated by Yannick Lippi aims to facilitate access to biologist in order to publish graphs such as heatmap, PCA or Venn diagram related to specifics data produced by TRiX's facility.", tags$br(),"
                                             
                                             MATRiX is an application dedicated to DNA chip analysis, this application incorporates quality control with Principal components analysis to summarizes microarray data and differential analysis with various methods such as Venn diagram, Heatmap clustering and GO Enrichment analysis by querying the DWS (DAVID WEB SERVICES).",tags$br(),"
                                             
                                             MATRiX app is working with specific data produced by the limma package, resulting p-values are adjusted according to the Benjamini and Hochberg procedure [Benjamini and Hochberg 1995]. PCA is computed with the FactoMineR package and the plot is produced with the factoextra package, for the Heatmap and Venn diagram the graphs are obtained respectively with the gplots and VennDiagram package, those packages are available on CRAN. This application works only with specific data produced by the plateau TRiX, you can check the example file (MA_Trix_App/sampleData.zip)."),
                                           
                                           p("Hereafter is the global workflow passing by the statistical analysis to the visualization:"),tags$br(),
                                           div(id="workflow",
                                               tags$p(tags$img(src = "whatmaen.png",style="width: 100%; height: 100%")))
                                           ),
                                  tabPanel("Authors", h3("The main contributors to MATRiX:"),
                                           p(a("Yannick Lippi",href="mailto:yannick.lippi@inra.fr"), "(Initiator, beta-testing, feature suggestions)"),
                                           p(a("Franck Soubès", href="mailto:franck.soubes@inra.fr"), "(Coding, Unit testing, documentation, packaging, feature suggestions)",tags$a(href = "https://github.com/fsoubes",target="_blank",
                                                                                                                                                                                     "See github")),
                                           h3("Acknowledgements"),
                                           p("Thanks to the Toxalim's team BioToMyc & TIM and especially to the following people for their helps reporting errors, proposing new features and beta testing of MATRiX:"),
                                           p("Laura Costes,", "Anne Fougerat,","Claire Naylies,", "Philippe Pinton,","Arnaud Polizzi," ,"Marion Regnier," , "Sandrine Ellero-Simatos,","Sarra Smati."),
                                           p("Special Thanks to Didier Laborie for installing the virtual machine with Ubuntu and for answering my questions")
                                           
                                  ),
                                  tabPanel("Packages",
                                           tags$h3("If you are using MATRiX in your work, you can cite some of the packages by clicking on the link down below."),
                                           
                                           actionLink("session",
                                                      "Print version information about R, the OS and attached or loaded packages."),
                                           br(), br(), br(),
                                           DT::dataTableOutput("sessinfo")
                                  ),
                                  tabPanel("Support",
                                           tags$head(
                                             tags$style(
                                               "#entry {width: 100%;position: relative;left: 4%;}
                                               #users ul li {font-family: 'Inconsolata', cursive;font-weight: 500;line-height: 1.5;color: white;position: static;font-size: 18px;}
                                               #users a{color: red;} #users p{color:white;}")),
                                           
                                           includeCSS("www/shinychat.css"),
                                           
                                           # And custom JavaScript -- just to send a message when a user hits "enter"
                                           # and automatically scroll the chat window for us. Totally optional.
                                           includeScript("www/sendOnEnter.js"),
                                           fluidRow(
                                             column(width=9,
                                                    div( style = "width:100% ; max-width: 1200px; height: 500px",
                                                         #tags$h2("Support client"),
                                                         div(
                                                           class = "row-fluid",
                                                           # Create a spot for a dynamic UI containing the chat contents.
                                                           uiOutput("chat"),
                                                           
                                                           # Create the bottom bar to allow users to chat.
                                                           fluidRow(
                                                             div(class="span8",
                                                                 textInput("entry", "")
                                                             ),
                                                             div(class="span2 center",
                                                                 actionButton("send", "Send")
                                                             )
                                                           )
                                                         )))
                                             
                                           )),
                                  tabPanel("Video",
                                           div( id="video",
                                                fluidRow(
                                                  column(8, align="center", offset = 2,
                                                         tags$iframe(src = "https://www.youtube.com/embed/lfI0zRYzeJs?vq=hd1080", width="960", height="540", align= "middle", frameborder="0",allowfullscreen ="1" )
                                                  ))))
                                  
                                           ))),
                column(width=3,
                       div(id="pass",style = "word-wrap: break-word;",
                           box(id="boxpass",title = strong("Session information", style="font-size:25px;"), width = NULL, background = "light-blue",
                               
                               # The right sidebar
                               # Let the user define his/her own ID
                               textInput("user", "Your User ID:", value=""),
                               tags$hr(),
                               h5("Connected Users"),
                               # Create a spot for a dynamic UI containing the list of users.
                               div(id ="users",
                                   uiOutput("userList"),
                                   tags$hr(),
                                   #helpText(HTML("<p>Built using R & <a href = \"http://rstudio.com/shiny/\">Shiny</a>.<p>Source code available <a href =\"https://github.com/trestletech/ShinyChat\">on GitHub</a>.")),
                                   p("Built using R and" ,tags$a(href = "http://rstudio.com/shiny/",target="_blank",
                                                                 "Shiny")),
                                   p("Chat source code is available ",
                                     tags$a(href = "https://github.com/trestletech/ShinyChat",target="_blank",
                                            "here"))
                               ))
                       ),
                       
                       box(
                         title = "What's new in MATRiX", width = NULL, status = "primary",
                         div(style = 'overflow-y: scroll; height: 500px',
                             addNews("Jan 11th 2019", "Data", "MATRiX is now compatible with Microarray, ChIP-seq and RNA-seq data"),
                             addNews("Nov 30th 2018", "MATRiX", "Add tootlip for distance, its now possible to export acyclic graphs in pdf and eps, correct bugs for classification enrichment with 0 nodes by inactivating donwload button"),
                             addNews("Nov 29th 2018", "Venn Diagram/HTML", "Correct bugs now venn diagram table is based on genes and not probes."),
                             addNews("Nov 5th 2018", "Venn Diagram", "Remove non annotated genes."),
                             addNews("Oct 19th 2018", "Volcano plot", "Search group of genes based on regular expression."),
                             addNews("Oct 15th 2018", "Venn Diagram", "Change the library to Jvenn."),
                             addNews("Aug 15th 2018", "Presentation/Video", "Added a video to present MATRiX and add modules to import files."),
                             addNews("Aug 10th 2018", "Upload/Volcano", "You can explore your different comparisons with a volcano plot."),
                             addNews("Aug 6th 2018", "Venn Diagram/Enrichment", "Add acyclic graph if download."),
                             addNews("Aug 4th 2018", "Heatmap/Bubble graph", "You can display or not the labels within the bubbles."),
                             addNews("Aug 3th 2018", "Heatmap/Go(Analysis)", "You can now plot a bubble graph (Highcharts) that summarizes the top categories from the resulting datatable."),
                             addNews("Jul 28th 2018", "Home/Support", "You can asks your questions directly in the Support."),
                             addNews("Jul 25th 2018", "Tutorial/Video", "Soon will be added a video to summarise the application."),
                             addNews("Jul 20th 2018", "New page", "Add a Home page regrouping informatios about the app."),
                             addNews("Jul 16th 2018", "Venn" ,"You can now choose your color for the venn diagram."),
                             addNews("Jul 16th 2018","Bug fixes","Venn diagram display erros when filtering."),
                             addNews("Jul 5th 2018","Venn/DAVID","Add Gene functionnal classification for selected intersection(s)."),
                             addNews("Jun 26th 2018","Add features","It's now possible to interact with the rendering table to filter the table in the aim of plotting the top n genes.
                                     For the GO enrichment it is now possible to select the rows in order to display the gene symbol according to the entrez ids."),
                             addNews("Jun 22th 2018","Bug fixes","For two contrasts the venn.draw function was not ordering the contrast names in the right order."),
                             addNews("Jun 20th 2018","MATRiX","First public release of MATRiX.
                                     Enhancement of the gui with the use of dashboard package."),
                             addNews("Jun 18th 2018","GO enrichment","It is now possible to query the DWS for the Heatmap and save the result in xlsx format for the different clusters"),
                             addNews("Jun 15th 2018","DNS ","Adding DNS for the MATRiX application (matrix.toulouse.inra.fr)"),
                             addNews("Jun 10th 2018","Venn diagram","The venn diagram FC and display of the top n genes
                                     have been added to compare the results of 2 or more contrasts."),
                             addNews("Jun 5th 2018","PCA/Heatmap","Display color groups side by side in the gui."),
                             addNews("May 29th 2018","beta-test","The service will be made available once the beta test phase is officially completed.")
                             )
                             )
                       
                )
                           )
                ),
      
      tabItem(tabName = "Upload",
              bsAlert("alert"),
              tags$style(type='text/css', ".well { max-width: 2em; }"),
              fluidRow(
                tags$head(
                  tags$style(type="text/css", ".myslidermain .irs-grid-text {bottom: 5px;color: #333;}
                             .myslidermain .irs-min{color: #333;font-size: 10px;line-height: 1.333;text-shadow: none;top: 0;padding: 1px 3px;
                             background: rgba(0,0,0,0.1);border-radius: 3px;-moz-border-radius: 3px}
                             .myslidermain .irs-max{color: #333;font-size: 10px;line-height: 1.333;text-shadow: none;top: 0;padding: 1px 3px;
                             background: rgba(0,0,0,0.1);border-radius: 3px;-moz-border-radius: 3px}")
                  ),
                column(width=9,
                       div( style = "width:100% ; max-width: 1500px; height: 1500px max-height: 2200px;" , id = "upload",
                            tabBox(title="Upload/Visualize your data", width=NULL,id = "upload",
                                   
                                   tabPanel("Import your data", style = "background-color: #ffffff;",
                                            conditionalPanel(condition = 'output.boolmark', #Hide or Show event depending on the loading data success or failure
                                                             tags$h1("How to import ?"),
                                                             tags$ul(
                                                               tags$li("First click on the browse button to load the data"),
                                                               tags$li("After the pop up has appeared, you will have to select the files within the access path that is given in the report produced by GeT-TRiX. "),
                                                               tags$li("You will then find three distinct csv files, these files are respectively named xxx_pData, xxx_WorkingSet and xxx_ResTable."),
                                                               tags$li("The final step consist to select all the data at once and then confirm the selection by clicking on the open button."),
                                                               tags$li("A green message will then appear to confirm the data loading with a summary table.")
                                                             ),
                                                             
                                                             tags$p(
                                                               tags$img(src = "pData.png"),
                                                               tags$img(src = "restable.png"),
                                                               tags$img(src = "workingset.png")
                                                             ),
                                                             
                                                             tags$h1("Tips"),
                                                             tags$ul(
                                                               tags$li("You can select a region by handling the left click button if the files are stacked together, if it's not the case you can select the different files by maintening the Ctrl button and clicked on the files.")
                                                             ),
                                                             tags$h1("Warning"),
                                                             tags$ul(
                                                               tags$li("It is highly recommanded to not modify these files (removed columns, change column names ...) in the aim of not disturbing the well functionning of the application.")
                                                             )
                                                             ,ns = NS("datafile")
                                            ),
                                            conditionalPanel(condition = '!output.boolmark',
                                                             
                                                             textOutput("myFileName"),
                                                             
                                                             column(12, h3(
                                                               "This table summarizes the number of significant genes depending on the p-value treshold choosen with the slider bar"
                                                             ),
                                                             helpText("Choose your p-value treshold to modify the following data table"),
                                                             div( class= "myslidermain",
                                                                  sliderInput("pval1","",min = 0.01,max = 0.05,value = 0.05,step = 0.01,
                                                                              width = "500"
                                                                  ),
                                                                  dataTableOutput("data_summary") # render a renderTable or renderDataTable within an application page
                                                             )),
                                                             
                                                             column(12,
                                                                    h3("This table shows the samples with the corresponding groups"),
                                                                    dataTableOutput("designtab")
                                                             ), 
                                                             column(12,
                                                                    h3("Show the actual data frame with the columns selected"),
                                                                    dataTableOutput("subsetgroup_hm")
                                                             )
                                                             ,ns = NS("datafile")
                                                             
                                            )),
                                   
                                   tabPanel("Volcano plot", style = "background-color: #ffffff;",
                                            
                                            conditionalPanel(condition = '!output.boolmark',
                                                             
                                                             div(style="display:inline-block;",
                                                                 fluidRow(column(3, style="width:34.0%;",
                                                                                 downloadButton("savevolcano", "Save your Volcano plot" , style =
                                                                                                  "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                                          column( 3, style="width:20%;",
                                                                                  selectInput(
                                                                                    "formvolc",label = NULL,
                                                                                    choices = c("png", "eps", "pdf"))))), 
                                              
                                            plotOutput(outputId = "volcanoplot", height = 900) , 
                                            div(style="display:inline-block", id ="dontwanttoshow",
                                                fluidRow(
                                                
                                                  column(5, style = "width:30%",
                                                         
                                                         downloadButton("savevolcplot", "Save your barplot" , style =
                                                                          "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                  column(3 ,
                                                         selectInput( "formvolcbar",label = NULL,
                                                                      choices = c("png", "eps", "pdf"))))),
                                            
                                            plotOutput(outputId ="barplotvolc", height = 500)
                                            ,ns = NS("datafile"))
                                            
                                   ), 
                                   tabPanel("Stripchart genes", value = "stripgenes", #style ="background-color: #ffffff;",
                                            conditionalPanel(condition = '!output.boolmark',
                                                          
                                                             column(width=12,
                                                                 div(id = "stripbox", 
                                                                    box(title="Filter the table",width = 10, status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,icon = icon("arrow-down"), 
                                                                        column(width=4,  radioButtons("decidemethodstrip",label = "Choose your statistical method",choices = c("adj.p.val (FDR)" = "FDR", "p.value (raw)" = "None"),inline = TRUE)),
                                                                        div( class= "myslidermain", column(3, sliderInput('fcstrip', "Choose your FC cutoff",min = 1, max=10, step = 1,value=1)),
                                                                        column(3,sliderInput('pvalstrip', "Choose your pval cutoff", min=0.01, max=0.05, step=0.01,value=0.05))),
                                                                        
                                                                        column(width=12,  textOutput("selected_stripgene") 
                                                                               #textInput("stripgenesearch", "Search your gene", placeholder = "cyp2b10" ),
                                                                               #bsTooltip("stripgenesearch", "Enter your gene in order to display the group mean expression.","bottom",trigger = "hover", options = list(container = "body"))
                                                                        )
                                                                        # column(width=3,  actionButton("plotstrip",label = "Generate plot",
                                                                        #                               icon = icon("arrow-down")),
                                                                        #        bsTooltip("plotstrip", "Click here to generate the strip graph", options = list(container = "body")),
                                                                        #        tags$style(type='text/css', "#plotstrip { width:100%; margin-top: 25px;}")
                                                                        # 
                                                                        # 
                                                                        # )
                                                                    ))),
                                                             
                                                             column(12,
                                                                    h3("This table shows the normalized values"),
                                                                    dataTableOutput("orderedwk")
                                                             ),
                                                             div(style="display:inline-block;",
                                                                 fluidRow(column(3, style="width:27%;",
                                                                                 downloadButton("savestriplot", "Save your plot" , style =
                                                                                                  "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                                          column( 3, style="width:20%;",
                                                                                  selectInput(
                                                                                    "formstrip",label = NULL,
                                                                                    choices = c("png", "eps", "pdf")))))
                                                             ,ns = NS("datafile"), 
                                                             
                                                             
                                                             plotOutput(outputId ="renderstripgenes", height = "500px", width ="100%")
                                                             
                                                             
                                                             
                                                             )           
                                   
                                   )
                            ))),
                div(id="pass",style = "word-wrap: break-word;",
                    column(width = 3,
                           box(id="boxpass",title = strong("Upload data", style="font-size:25px;"), width = NULL, background = "light-blue",
                               inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
                               
                               
                               downloadLink("downloadData", label = "download sample data", style="color:orange; float:right;"),
                               br(),br(),
                               
                               
                               csvFileInput("datafile", "User data (.csv format)"),
                               fluidRow(column(6,
                                               p("Import local example",style="color:white; font-weight: 700; font-size: 14px;"),
                                               
                                               dirModuleUI("datafile")),
                                        column(6, 
                                               csvIdentifier("datafile", "Unique identifier")
                                        )),
                               
        
                               br(),
                               
                               conditionalPanel(condition = '!output.boolmark',
                                                selectInput(
                                                  "method","Choose your statistical method",choices = c("adj.p.val (FDR)" = "FDR", "p.value (raw)" = "None")),  ns = NS("datafile"))),
                               
                           conditionalPanel(condition = '!output.boolmark',
                               box(id="boxpass2",title = strong("VOLCANO plot", style="font-size:25px;"), width = NULL, background = "light-blue",         
        
                                   fluidRow(column(6,
                                                uiOutput("compvolc")), 
                                            column(6,
                                                   selectInput("regulationvolc", #  Create a select list that can be used to choose a single or multiple items from a list of values.
                                                               "Choose your regulation",
                                                               choices = c("both","up", "down"))
                                                   
                                                   )),
                                         
                                   fluidRow(column(6, sliderInput('volcfc', "Choose your FC cutoff",min = 1, max=10, step = 1,value=1)),
                                            column(6,sliderInput('volcpval', "Choose your pval cutoff", min=0.01, max=0.05, step=0.01,value=0.05))),
                                   fluidRow(column(6, sliderInput('volclab', "Choose your lab size",min = 1, max=6, step = 0.5,value=3.0)),
                                            column(6,sliderInput('volcpt', "Choose your point size", min=0.5, max=3, step=0.1,value=1))),
                                   
                                                   
                                                   
                                                   
                                                div(id = "mytextvolc",
                                                    p(" Highlight your selected gene(s) in the volcano plot with a comma-separated list of input ")
                                                ),
                                                
                                                textInput(inputId = "fillvolc",label = NULL,value = NULL,
                                                          placeholder = "FOXP2,OT,AVPR1a",width = "100%"
                                                ),
                                   fluidRow(column(6,
                                                
                                                textInput(inputId = "findfamily",label = "Highlight a gene family", value = NULL,
                                                          placeholder = "Cyp",width = "100%")),
                                            column(6,
                                                numericInput(
                                                  'topvolc',
                                                  'Max number of genes',
                                                  NULL,min = 0,max = 5000))), 
                                   
                                   uiOutput("addcompvolc")
                                        
                                                # ns = NS("datafile")
                               ), ns = NS("datafile"))
                    )
                )#,
                # conditionalPanel(condition = '!output.boolmark',
                #                  column(12,
                #                         h3("Show the actual data frame with the columns selected"),
                #                         dataTableOutput("subsetgroup_hm")
                #                  ),ns = NS("datafile"))
                
                
                  )),
      
      ###############################
      ######## PCA page             #
      ###############################
      
      tabItem(tabName = "PCA",
              tags$style(type='text/css', ".well { max-width: 20em; }"),
              tags$style(type='text/css', ".well { max-height: 50em; }"),
              fluidRow(column(width = 9,
                              div(style = "width:100% ; max-width: 1500px; height: 1500px max-height: 2200px;",
                                  
                                  tabsetPanel(
                                    
                                    tabPanel(
                                      strong("Scree plot"),
                                      downloadButton("savescre", "Save your Scree plot" , style =
                                                       "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                                      
                                      br(),br(),
                                      plotOutput(outputId = "eigpca", height = 700)
                                    ),
                                    tabPanel(
                                      strong("PCA plot"),
                                      tags$style(type = "text/css",".shiny-output-error:before { visibility: hidden; }"),
                                      
                                      
                                      div(style="display:inline-block;",
                                          fluidRow( column(1 ,
                                                           downloadButton("savepca", "Save your PCA" , style =
                                                                            "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                    column(2),
                                                    column( 3,
                                                            selectInput(
                                                              "formpca",label = NULL,
                                                              choices = c("png", "eps", "pdf")))
                                                    
                                          )),
                                      
                                      plotOutput(outputId = "PCA", height = 700) #,plotOutput(outputId = "PCAvarender", height = 700)
                                    )
                                    
                                  )
                              )
              ),
              div(id="pass",style = "word-wrap: break-word;",
                  column(width=3,
                         box(id="boxpasspca",title = strong("PCA settings",style="font-size:25px;"), width = NULL, background = "light-blue",
                             inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
                             strong("Choose your group to visualize"),
                             uiOutput("grpselpca"),
                             actionButton(
                               inputId = "allgrpca",label = "Select all",
                               icon = icon("check-square-o"),
                               style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             ),
                             actionButton(
                               inputId = "nogrpca",label = "Clear selection",icon = icon("square-o"),
                               style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             ),
                             
                             
                             fluidRow(column(6,
                                             selectInput("dim1",
                                                         label = h6(gettext("x axis")),
                                                         choices = list(
                                                           "Dim 1" = 1,
                                                           "Dim 2" = 2,
                                                           "Dim 3" = 3,
                                                           "Dim 4" = 4,
                                                           "Dim 5" = 5
                                                         ),
                                                         selected = firstdim
                                             )
                             ),
                             column(6,
                                    selectInput("dim2",
                                                label = h6(gettext("y axis")),
                                                choices = list(
                                                  "Dim 1" = 1,
                                                  "Dim 2" = 2,
                                                  "Dim 3" = 3,
                                                  "Dim 4" = 4,
                                                  "Dim 5" = 5
                                                ),
                                                selected = secdim
                                    )
                             )),
                             fluidRow(
                               column(4,
                                      checkboxInput("label", "Add labbels names", TRUE)),
                               column(4,
                                      checkboxInput("meanpoint", "Add mean points", TRUE)),
                               column(4,
                                      checkboxInput("ellipse", "Add ellipses", FALSE))#,
                             ),
                             
                             fluidRow(column(3),
                                      column(8,checkboxInput("jitter", "Avoid overlap between points", FALSE))),
                             verbatimTextOutput("valued"),
                             
                             sliderInput(
                               "labelsiize","Label size",min = 2,max = 6,value = 4,step = 1
                             ),
                             
                             sliderInput("pointsiize","Point size",min = 2,max = 6,value = 2,step = 1
                             ),
                             
                             uiOutput('myPanelpca'),
                             br()
                         ))))),
      ###############################
      ######## Venn Page            #
      ###############################
      
      
      tabItem(tabName = "Venn",
              
              tags$style(type='text/css', ".well { max-width: 25em; }"),
              tags$style(type='text/css', ".well { max-height: 70em; }"),
              fluidRow(column(
                width = 9,
                div(
                  style = "width:100% ; max-width: 1500px; height: 1950px; max-height: 2800px;",
                  tabsetPanel(
                    id = "Vennd",
                    
                    tabPanel(
                      value= "vennset",
                      strong("Visualize the Venn diagram"),
                      
                     column(6,offset = 0, style='padding:0px;',
                           div(style="display:inline-block",
                      fluidRow(
                      column(3, style="width:43%",
                          downloadButton('downloadvenn', "Download the data",
                                         style ="color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                         column(3,
                          downloadButton("downloadsetven", "Download venn set" , style =
                                           "color: #fff; background-color: #337ab7; border-color: #2e6da4"))))),
                          
                          
                          
                          column(width=6, offset = 0, style='padding:0px;',
                                div(id = "vennbox", 
                                    box(title="Filter the datatable",width = 12, status = "primary", solidHeader = TRUE,collapsible = TRUE,collapsed = TRUE,icon = icon("arrow-down"), 
                                        column(3,
                                               numericInput("filtertopjvenn", "Top n genes", value = 50, min=5 , max = 150)),
                                        column(5,
                                               selectInput("filtermethjvenn", "Based on", choices = c("adj.p.val (FDR)"= "FDR", "p.value (raw)" = "None"))),
                                        column(5,
                                               uiOutput("filtercompjvenn"))
                  ))),
# 
#                           shiny::actionButton( 
#                             "togglefiltertabvenn",
#                             "Advanced Filter Options",
#                             href = "#",
#                             icon = icon("filter"),
#                             style = "color: #fff; background-color: #337ab7; border-color: #2e6da4; position:relative;position:absolute;left:50%;margin-left: 15px;"
#                           ),
#                           br(),
#                           
#                           shinyjs::hidden(div( style ="position:absolute;left:50%;margin-left: 15px;",
#                             id = "advancedfilter",
#                             fluidRow( column(3,
#                             numericInput("filtertopjvenn", "Top n genes", value = 50, min=5 , max = 150)),
#                             column(3,
#                             selectInput("filtermethjvenn", "Based on", choices = c("adj.p.val (FDR)"= "FDR", "p.value (raw)" = "None"))),
#                             column(5,
#                             uiOutput("filtercompjvenn")))))
#                           
                 #    ),
                      
                      conditionalPanel(condition = '!output.bool',
                                       uiOutput(outputId = "image")
                                       , uiOutput("sorry")),
                      tags$script(src="libraries/prettify.js") ,
                      tags$script(src="libraries/jvenn.min.js")  ,
                      tags$script(src="libraries/canvas2svg.js")  ,
                      tags$script(src="tooltip.js"),
                      fluidRow(column(6,
                                      tags$script(src="jvenn.js"),
                                      tags$div(id="jvenn-container", style = "background-color: white; width: 100%; height:100%")
                                      
                      ),
                      column(6,
                             div(class= "dfvenn" , style="font-size:24px; margin-top: 17px;",
                                 
                                 #conditionalPanel(condition = "input.togglefiltertabvenn%2==1", br(),br()),
                                 
                                 htmlOutput("dfvenn")),
                             
                             conditionalPanel(condition = "input.dispvenn == 'genes'",
                                              helpText(
                                                "You can directly filter the table by fold change and save the output table"
                                              )),
                              
                             DT::dataTableOutput("vennresinter"),br(),br(),br(),
                             conditionalPanel(condition = "input.selcontjv",
                                              div(class= "dfvennbef" , style="font-size:24px; margin-top: -28px; "))
                             
                      )),
                      div(style="display:inline-block", id ="dontwanttoshow",
                          fluidRow(
                            tags$head(
                              tags$style(type="text/css", ".topgeness label{ display: table-cell; text-align: left; vertical-align: middle; }
                                         .inline .form-group{display: table-row;} ")
                              ),
                            column(3,br(),style= "width:21%;",
                                   actionButton(
                                     inputId = "topdegenes",
                                     label = "Plot top DE genes",
                                     style =
                                       "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                   )),
                            column(3, style= "width:26.0%;",br(),
                                   
                                   downloadButton("savebarplot", "Save your barplot" , style =
                                                    "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                            column(3 ,br(),style= "width:11%;  padding: 0%;",
                                   selectInput( "formvenbar",label = NULL,
                                                choices = c("png", "eps", "pdf")))
                            # ,
                            # column(3,style= "width:9%; padding: 0%;",
                            #        
                            #        uiOutput("topgenesvenn", style= "padding: 0px;font-weight: 400;top: 0px;
                            #                 right: -22px;left: 0px;color: #3c8dbc;position: absolute;"))
                                   )),
                      plotOutput(outputId ="barplotvenn", height = "500px", width ="100%"),
                      br(),
                      h1("Here's a tracker for your different selections:"),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # add style.css in order to add better police
                      ),
                      
                      tags$head(tags$style("#container * {
                                           display: inline;
                                           }")),

                      tags$head(tags$style("#mytext p{font-weight: 500;font-size: 17px;line-height: 1.5;color: white;
                                           position: static;}
                                           #mytext a{color: orange;}"
                      )),
                      
                      div(
                        id = "container",
                        p("You have chosen the following comparisons"),
                        htmlOutput("contvenn"),
                        p("for a total of"),
                        htmlOutput("totalgenes"),
                        p("genes  with a P-value and FC treshold respectively set to "),
                        htmlOutput("myPVALvenn"),
                        p("and"),
                        htmlOutput("myFCvenn")
                        
                      ),
                      div(
                        id = "container",
                        p("There are"),
                        htmlOutput("venngenes"),
                        p("significant genes"),
                        p("for this intersection"),
                        htmlOutput("continter"),
                        p("if you click on the top DE genes button you will plot the top"),
                        htmlOutput("topgenesdf"),
                        p("rows the of the previous table")
                      )
                      
                      ),
                    
                    
                    tabPanel(strong("Venn GO enrichment"),
                             value = "venngopanel",
                             useShinyjs(),
                             
                             fluidRow( column(6 ,
                                              downloadButton(
                                                "saveclusterchoose",
                                                "Download the graph" ,
                                                style =  "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                              ),
                                              column( 3,
                                                      selectInput(
                                                        "formvennclus",label = NULL,
                                                        choices = c("png", "eps", "pdf")))
                                              
                             )),
                             
                             
                             plotOutput("clusterPlot", width = "100%", height = "700px"),
                             br(),br(),br(),
                             dataTableOutput("debug")
                    )
                    
              )
      )
              ),
      div(id="pass",style = "word-wrap: break-word;",
          column(width=3,
                 box(id="boxpassvenn",title = strong("Venn settings", style ="font-size:25px;"), width = NULL, background = "light-blue",height = "100%",
                     inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
                     uiOutput("contout"),
                     actionButton(
                       inputId = "allCont",
                       label = "Select all",
                       icon = icon("check-square-o"),
                       style =
                         "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     ),
                     
                     actionButton(inputId = "noCont",label = "Clear selection",
                                  icon = icon("square-o"),
                                  style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     ),
                     fluidRow(column(6,
                                     selectInput("methodforvenn","Statistical method",
                                                 choices = c("adj.p.val (FDR)"= "FDR", "p.value (raw)" = "None")
                                     )),
                              column(6,
                                     selectInput("regulation", #  Create a select list that can be used to choose a single or multiple items from a list of values.
                                                 "Choose your regulation",
                                                 choices = c("both","up", "down")))),
                     div(id = "mytext",
                         p("A comma-separated list of ",
                           tags$a(href = "https://stat.columbia.edu/~tzheng/files/Rcolor.pdf",target="_blank",
                                  "x11"),
                           "or",
                           tags$a(href = "https://en.wikipedia.org/wiki/Web_colors#Hex_triplet",target="_blank",
                                  "hex colors."))),
                     
                     textInput(inputId = "fill",label = NULL,value = c( "green,blue,red,purple,orange,brown"),
                               placeholder = "grey70, white, steelblue4",width = "100%"
                     ),
                     
                     fluidRow( column(6,
                                      sliderInput("pvalvenn","P-value treshold",
                                                  min = 0.01,max = 0.05,
                                                  value = 0.05,step = 0.01
                                      )),
                               
                               column(6,
                                      sliderInput("fcvenn","FC treshold",min = 1, max = 10,
                                                  value = 1,step = 1
                                      ))),
                     br(),
                     
                     fluidRow(
                       column(12,
                              selectInput("dispvenn", #  Create a select list that can be used to choose a single or multiple items from a list of values.
                                          "Choose if you want to display probes or genes",
                                          choices = c("probes", "genes"))),
                       column(6,
                              checkboxInput("Notanno","Remove the genes that are not annotated",FALSE)),
                       column(6,
                              checkboxInput("Allcont","Show the logFC for all comparisons",FALSE))),
                     
                     br(),
                     shiny::actionButton(
                       "toggleAdvancedJvenn",
                       "Advanced Jvenn Options",
                       href = "#",
                       style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     ),
                     br(),
                     
                     shinyjs::hidden(div(
                       id = "advancedjvenn",
                       br(),
                       
                       fluidRow(
                         column(6,
                                p("Select your Type of Venn",style="color:white; font-weight: 700; font-size: 14px;"),
                                includeHTML("HTML/jvenntype.html")),
                         column(6,
                                p("Display the stat",style="color:white; font-weight: 700; font-size: 14px;"),
                                includeHTML("HTML/displaystat.html"))),
                       br(),
                       p("Police's size", style="color:white; font-weight: 700; font-size: 14px;"),
                       includeHTML("HTML/fontsize.html"),
                       br(),
                       fluidRow(
                         column(6,
                                p("Find an element in list(s)",style="color:white; font-weight: 700; font-size: 14px;"),
                                includeHTML("HTML/seekgene.html")),
                         column(6,
                                p("Display switch",style="color:white; font-weight: 700; font-size: 14px;"),
                                includeHTML("HTML/dispswitch.html")))
                       
                       
                     )),
                     
                     br(), 
                     # shiny::actionButton(
                     #   "togglefiltertabvenn",
                     #   "Advanced Filter Options",
                     #   href = "#",
                     #   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                     # ),
                     # br(),
                     # 
                     # shinyjs::hidden(div(
                     #   id = "advancedfilter",
                     #   br(),br(), br(),
                     #   numericInput("filtertopjvenn", "Top n genes", value = 50),    
                     #  selectInput("filtermethjvenn", "Based on", choices = c("adj.p.val (FDR)"= "FDR", "p.value (raw)" = "None")),
                     # uiOutput("filtercompjvenn"))),
                     br(),
                     
                     
                     strong("Functional Annotation Clustering",style = "font-family: 'times'; font-size:20px; font-style: strong; "),
                     
                     br(),br(),
                     fluidRow(column(6, sliderInput("clusterNumber",label = "Cluster",
                                                    value = 1, min = 1,max = 5
                     )),
                     column(6,
                            selectInput("Speciesvenn", "Choose your Species:", selected = "Mus musculus",
                                        c("Mouse" = "Mus musculus", "Human" = "Homo sapiens", "Rat" = "Rattus norvegicus", "C. elegans" = "Caenorhabditis elegans",
                                          "Zebrafish" = "Danio rerio",  "Pig" = "Sus scrofa",
                                          "Chicken" = "Gallus gallus", "Chimpanzee" = " Pan troglodytes" )))),
                     fluidRow(
                       column(6,selectInput("catvenn", "Choose your category", selected ="BP", c("BP","MF","CC"))),
                       
                       column(6,br(),
                              actionButton("GOvenn", "Run Analysis",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                     
                 ))))),
      
      ###############################
      ######## Heatmap Page         #
      ###############################
      
      
      tabItem(tabName = "Heatmap",
              tags$style(type='text/css', ".well { max-width: 25em; }"),
              
              
              fluidRow(column(
                width = 8,
                div(
                  tabsetPanel(
                    id = "heatmapmainp",
                    tabPanel(
                      strong("Visualize the Heatmap"),value = "hmmainpan",
                      div(style="display:inline-block",
                          fluidRow( column(1 ,style="width:9%;",
                                           downloadButton(
                                             "savehm",
                                             "Save your plot" ,
                                             style =  "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                           )),
                                    column(2),
                                    column( 3,
                                            selectInput("formhm", label = NULL,
                                                        choices = c("png", "eps", "emf")))
                                    
                          )),
                      includeCSS("./css/style.css"),
                      conditionalPanel(condition = '!output.heatmbool',  verbatimTextOutput("warningsheat")
                      ),
                      
                      conditionalPanel(
                        condition = "input.col1 =='blue' && input.col3 =='red' && input.submit == 0 ",
                        wellPanel(style = "position: absolute; width: 30%; left: 35%; top: 40%;
                                  box-shadow: 10px 10px 15px grey;",
                                  selectInput("text", "Choose your intermediate color:", choices = c("yellow", "white")),
                                  actionButton("submit", "Submit"))
                      ),
                      
                      #plotOutput("warningsheat")
                      conditionalPanel(condition = 'output.heatmbool',
                                       plotOutput("distPlot", width = "100%" , height = 1300)
                                       
                      ),
                      
                      h1("Here's a tracker for your different selections:"),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # add style.css in order to add better police
                      ),
                      
                      tags$head(tags$style("
                                           #container * {display: inline;}")),
                      
                      div(
                        id = "container",p("There are"),htmlOutput("myNUM"),
                        p("significant genes"),
                        p("for the following comparison(s)"),
                        htmlOutput("testtt")
                      ),
                      div(
                        id = "container",
                        p('The selected rows for your heatmap are based on the '),
                        textOutput("myMET"),
                        p("method, with a P-value and FC treshold respectively set to "),
                        textOutput("myPVAL"),
                        p('and'),
                        textOutput("myFC")
                      ),
                      
                      conditionalPanel(condition = "input.maxgen != null",
                                       div(
                                         id = "container",
                                         p("You have chosen to regulate your comparison to "),
                                         textOutput("maxGen"),
                                         p(" genes maximum"))),
                      
                      div(
                        id = "container",
                        p('The'),
                        textOutput("myMAT"),
                        p(
                          "method was used to compute the matrix distance with a number of clusters for the significant genes equal to",
                          textOutput("myCLUST")
                        )
                      ),
                      div(
                        id = "container",
                        p('The advanced color settings choosen for the following groups :'),
                        textOutput("indivcol"),
                        p("are respectively correlated to the following colors"),
                        htmlOutput("myPAL")
                      ),
                      div(
                        id = "container",
                        p(
                          'The legend size, row size, col size are respectively equals to ',
                          textOutput("myLEG"),
                          p(','),
                          textOutput("myROW"),
                          p(','),
                          textOutput("myCOL")
                        )
                      )
                      ),
                    tabPanel(
                      strong("Heatmap clusters"),
                      value = "dfhmclu",
                      downloadButton('downloadcut', "Download the data",
                                     style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      column(
                        12,
                        h3("Table summarizing the heatmap"),
                        helpText(
                          "Heatmap's cluster are upside down in order to match the genes with the heatmap from top to bottom"
                        )
                        ,
                        dataTableOutput("clustering"),
                        
                        
                        h3("This table summarizes the number of significant probes and genes by cluster"),
                        helpText(
                          "For the number of genes by cluster the duplicated genes are removed"
                        ),
                        dataTableOutput("totalgenbyc")
                      )),
                    tabPanel(
                      strong("(GO) enrichment-based cluster analysis"),value="maingo",
                      downloadButton("savegohmdavxlsx", "Save your enrichment as xlsx" , style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                      
                      conditionalPanel(condition = "input.GO",
                                       div(class= "highvenn" , style="font-size:24px; text-align: center;",
                                           htmlOutput("titlegomain")),
                                       DT::dataTableOutput("davidgo"),
                                       
                                       verbatimTextOutput("printmessage"),
                                       verbatimTextOutput("printselected"),
                                       div(class= "highvenn" , style="font-size:24px; text-align: center;",
                                           htmlOutput("titlegotop")),
                                       tags$script(src="libraries/highcharts.js"),
                                       tags$script(src="libraries/highcharts-more.js"),
                                       
                                       tags$script(src="https://code.highcharts.com/modules/exporting.js"),
                                       tags$script(src="https://code.highcharts.com/modules/export-data.js"),
                                       
                                       
                                       tags$div(id="highChart")  ,
                                       checkboxInput("addlabelhigh", "add label", FALSE),
                                       tags$script(src="bubble.js")
                                       
                      )),
                    tabPanel(
                      strong("Cut heatmap"),#icon("table"),
                      bsAlert("alert"),value = "cuthmmainpan",
                      plotlyOutput(outputId = "cutheatmap", height = 600),
                      
                      
                      br(),br(),br(),br(),br(),br(),br(),br(),br(),
                      br(),br(),br(),br(),br(),br(),br(),br(),br(),
                      br(),br()
                      
                    )
                  )
                  )
                ),
                column(width=4,
                       
                       div(id="mypanheat", style="color: white;",
                           tabBox(
                             
                             title = "",
                             id = "heatmapanel",
                             width = NULL,
                             
                             tabPanel("Heatmap",id= "heatmpan", ##ADD8E6
                                      style="background-color: #3c8dbc;",
                                      value="widgetheat",
                                      strong("Heatmap settings", style="font-size:25px;") ,
                                      br(),
                                      
                                      actionLink("resetAll",  label = ("reset all"), style="color:orange;float:right;font-size: 18px;"),
                                      br(),
                                      uiOutput("grpselhm"),
                                      actionButton(
                                        inputId = "allgrphm",
                                        label = "Select all",
                                        icon = icon("check-square-o"),
                                        style =
                                          "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                      ),
                                      actionButton(
                                        inputId = "nogrphm",
                                        label = "Clear selection",
                                        icon = icon("square-o"),
                                        style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                      ),
                                      div(
                                        id = "form",
                                        # Creates a panel with a slightly inset border and grey background
                                        uiOutput("comphm"),
                                        actionButton(
                                          # Action button that automatically react when triggered
                                          inputId = "allcomphm",label = "Select all",icon = icon("check-square-o"),
                                          style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                        ),
                                        actionButton(
                                          inputId = "nocomphm",label = "Clear selection",icon = icon("square-o"),
                                          style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                        ),
                                        br(),br(),
                                        fluidRow( column(6,
                                                         numericInput(
                                                           # Create an input control for entry of numeric values
                                                           'maxgen',
                                                           'Maximal number of genes by comparison(s)',
                                                           NULL,min = 100,max = 2500
                                                         )),column(6,
                                                                   br(),
                                                                   selectInput(
                                                                     "decidemethod",
                                                                     "Choose your statistical method",
                                                                     choices = c("adj.p.val (FDR)"= "FDR", "p.value (raw)" = "None")
                                                                   ))),
                                        
                                        br(),
                                        fluidRow( column(6,
                                                         sliderInput(
                                                           "pval",
                                                           "P-value treshold",
                                                           min = 0.01,
                                                           max = 0.05,
                                                           value = 0.05,
                                                           step = 0.01
                                                         )),
                                                  column(6,
                                                         sliderInput(
                                                           "fc",
                                                           "FC treshold",
                                                           min = 1,
                                                           max = 10,
                                                           value = 1,
                                                           step = 1
                                                         ))),
                                        br(),
                                        div(id = 'center', strong("Advanced settings",style = "font-family: 'times'; font-size:20px; font-style: strong; ")),
                                        br(),
                                        shiny::actionButton(
                                          "toggleAdvanced",
                                          "Advanced Computational Options",
                                          href = "#",
                                          style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                        ),
                                        br(),
                                        #includeHTML("HTML/tooltip.html"),
                                        shinyjs::hidden(div(
                                          id = "advanced",
                                          fluidRow(
                                            column(6,
                                                   numericInput('clusters', 'Cluster count', 3,
                                                                min = 1, max = 15)),
                                            column(6,
                                                   #addTooltip(session, id, title, placement = "bottom", trigger = "hover", options = NULL),
                                                   #div(id = "mytext",
                                                   #    p("Choose your matrix distance",includeHTML("HTML/tooltip.html")),
                                                   selectInput(
                                                     "dist","Choose your matrix distance",choices = c("correlation", "euclidian","manhattan", "cosine")),
                                                   div(id = "tooltipelem",
                                                       bsTooltip(id = "dist", title = "correlation:\n dist = 1-corr", placement = "left", trigger="hover"))
                                                   
                                            )),
                                          fluidRow(
                                            column(6,
                                                   checkboxInput("meangrp",
                                                                 "Compute the mean for the different groups",
                                                                 FALSE)),
                                            column(6,
                                                   selectInput(
                                                     "algomet","Choose your hierarchical clustering method",choices = c("ward.D2", "single","complete","average")))
                                            
                                          )))
                                        ,br(),
                                        shiny::actionButton("toggleAdvancedcolors","Advanced graphical Settings",
                                                            href = "#",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                        ),
                                        
                                        br(),
                                        
                                        shinyjs::hidden(div(
                                          id = "advancedcol",
                                          fluidRow(
                                            column(6,
                                                   colourpicker::colourInput(
                                                     "col1","Select colour for downregulated genes",firstcol,palette = "limited"
                                                   )
                                            ),
                                            column(
                                              6,
                                              colourpicker::colourInput("col3", "Select colour for upregulated genes", lastcol,
                                                                        palette = "limited")
                                            )
                                          ),
                                          fluidRow(column(4,
                                                          numericInput(
                                                            'rowsize','Row size',0.9,
                                                            min = 0.2,max = 1.5,step = 0.1
                                                          )
                                          ),
                                          column(4,
                                                 numericInput(
                                                   'colsize','Col size',0.9,
                                                   min = 0.2,max = 1.5,step = 0.1
                                                 )
                                          ),
                                          column(4,
                                                 numericInput(
                                                   'legsize','Legend size',0.8,
                                                   min = 0.2,max = 1.5,step = 0.1
                                                 )
                                          )
                                          
                                          ),
                                          
                                          fluidRow(column(6,
                                                          radioButtons("rowname",
                                                                       "show/hide rowname",
                                                                       c("hide", "show"))
                                                          
                                          ),
                                          column(6,
                                                 radioButtons("colname",
                                                              "show/hide colnames",
                                                              c("show", "hide"))
                                          )),
                                          
                                          uiOutput('myPanel'),
                                          br()
                                        ))), #end of the div "form"
                                      br(),
                                      shinyjs::hidden(div(
                                        # Hide some widgets between the tags
                                        id = "advancedgo",
                                        wellPanel(
                                          
                                        )
                                      )),
                                      
                                      br(),
                                      div(id = 'center', strong("Print Heatmap",style = "font-family: 'times'; font-size:20px; font-style: strong; ")),
                                      br(),
                                      
                                      fluidRow(column(6, uiOutput("button")
                                      ),
                                      column(6,
                                             checkboxInput("reactheat",
                                                           "Add reactivity",
                                                           FALSE))
                                      ),
                                      
                                      helpText("Note: It is highly advised to check this box if you're working with a set of genes close to 1000.",style="color:White; font-size:15px;"),
                                      conditionalPanel(condition = 'output.heatmbool',
                                                       
                                                       div(id = 'center', strong("Functional enrichment analysis",style = "font-family: 'times'; font-size:20px; font-style: strong; ")),
                                                       br(),
                                                       fluidRow(column( 4,
                                                                        selectInput("Species", "Choose your Species:", selected = "Mus musculus",
                                                                                    c("Mouse" = "Mus musculus", "Human" = "Homo sapiens", "Rat" = "Rattus norvegicus", "C. elegans" = "Caenorhabditis elegans",
                                                                                      "Zebrafish" = "Danio rerio",  "Pig" = "Sus scrofa",
                                                                                      "Chicken" = "Gallus gallus", "Chimpanzee" = " Pan troglodytes" ))),
                                                                column(4,
                                                                       uiOutput("cutgo")),
                                                                column(3,
                                                                       selectInput('catinfo','Category: ',
                                                                                   choices =  c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY"),
                                                                                   selected=  c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY"),
                                                                                   multiple = TRUE
                                                                       )
                                                                )),
                                                       fluidRow(
                                                         column(4,br(),
                                                                actionButton("GO", "Run Analysis",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                                         column(4,br(),
                                                                uiOutput("DAVID"))
                                                       ),br(),
                                                       helpText("Run Gene enrichment analysis, results are obtained by querying DWS (DAVID Web Services)", style="font-size:15px; color:white;")
                                                       
                                      ),br(),br()
                                      
                             ),
                             
                             tabPanel(
                               "Heatmap clustering",
                               value="cutheatmainp",
                               strong("Cut heatmap settings", style="font-size:25px;") ,
                               br(),
                               uiOutput("cutcluster"),
                               selectizeInput('cutinfo', 'Choose your types of plots',
                                              choices = cutheatmlist),
                               # cutheatmlist is a variable defined in the global environment
                               br(),
                               
                               selectInput("formcut","Choose your file format",choices = c("pdf", "png", "eps")
                               ), br(),
                               verbatimTextOutput("event"),
                               
                               br(),
                               downloadButton("savecut", "Save your plot" , style =
                                                "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                               
                             ))
                       )
                )
                
              ))
      )
  )
  )

###############################
######## END dashboardbody    #
###############################

shinyUI(
  dashboardPage(skin="blue",title = "MATRiX app",
                dbHeader,
                sidebar,
                body
  )
)
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


require(dplyr)
require(RColorBrewer)


#' distcor is a function that computes the distance correlation 
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

distcor<-function(x) {as.dist(1-cor(t(x),use="pairwise.complete.obs"))}


#' disteuc is a function that computes the euclidean correlation
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

disteucl<-function(x) {dist(x,method="euclidian")}


#' hclustfun is a function that performs a hiearchical cluster analysis with the method of Ward
#'
#' @param d 
#'
#' @return an object of class hclust
#' 
#' @export

hclustfun=function(d) {hclust(d,method="ward.D2")} 

require("Biobase")  
require("marray") 


#' num2cols is a function that gives a character vector of color names for a given numeric vector
#'
#' @param numVector a numeric vector
#' @param colp a character vector
#'
#' @return a matrix object 
#' 
#' @export

num2cols=function(numVector,colp=palette()){
  
  gpcol=as.data.frame(numVector)

  numCols=length(unique(gpcol[,1]))
  mycol <- sort(unique(gpcol$numVector))
  if(length(colp) <numCols) warning("number of color names < number of levels! Give a color palette with at least ",numCols," terms to 'col' argument")
  cols=as.data.frame(matrix(c(mycol,colp[1:(numCols)]),nrow=numCols)) ## couleurs pour les groupes
  myColFun=function(x){
    as.character(cols[cols[,1]==x[1],2])
  }
  apply(gpcol,1,FUN=myColFun)
  
}



############################################################################
##   plotHeatmaps() function                                              
############################################################################ 


#' plotHeatmaps is a function that create a false color image with a dendogram added to the left side and the top
#'
#' @param exprData a data frame with all the individuals selected
#' @param geneSet a data frame with the indexes corresponding to the sigificant genes
#' @param groups a data frame with the corresponding groups 
#' @param workingPath an access path
#' @param k a numeric value which aim is to defined the treshold value to cut the dendogram
#' @param fileType a character value which extension corresponds to different file types
#' @param cexcol  a positive numbers, used as cex.axis in for the row or column axis labeling
#' @param cexrow a positive numbers, used as cex.axis in for the row or column axis labeling
#' @param colOrder a positive numbers, used as cex.axis in for the row or column axis labeling
#' @param labrow a character vectors with row and column labels to use
#' @param na.color color to use for missing value 
#' @param scale a character indicating if the values should be centered and scaled in either the row direction or the column direction, or none
#' @param hclustGenes a function used to compute the hierarchical clustering when Rowv or Colv are not dendrograms
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#' @param plotRowSideColor a boolean value that colors or not the rows side
#' @param RowSideColor a character vector of length nrow containing the color names for a vertical side bar that may be used to annotate the rows of x.
#' @param Rowdistfun a function used to compute the distance for the rows
#' @param Coldistfun a function used to compute the distance for the columns
#' @param palette.col NULL
#' @param margins a numeric vector of length 2 containing the margins (see par(mar= *)) for column and row names, respectively.
#' @param my_palette a character vetor of length 17
#' @param mycex a numeric value which aim is to change the size of the legend
#' @param mypal a character vetor of length 17 
#' @param colid a character vectors with row and column labels to use; default NULL
#' @param showcol a boolean value used to hide or show the colnames 
#' @param showrow a boolean value used to hide or show the rownames
#' @param genename a data frame list corresponding to the gene names 
#'
#' @return an heatmap object
#' 
#' @export


plotHeatmaps=function(exprData,geneSet,groups,workingPath=getwd(),k=3,fileType="png",cexcol=1.5,cexrow=1.5,
                      colOrder=NULL,labrow=F,na.color="black",scale="row",hclustGenes=T,meanGrp=F,plotRowSideColor=T,#col.hm=greenred(75),
                      RowSideColor=c("gray25","gray75"), Rowdistfun="correlation",Coldistfun="correlation" ,palette.col=NULL, 
                      margins=c(8,8),my_palette=colorRampPalette(c("green", "black", "red"))(n = 75),mycex = 0.6,mypal=test,colid = NULL, showcol = T ,showrow =F, genename=NULL, notplot = F){
  
  #RowSideColor: color palette to be used for rowSide cluster colors
  # can also be gray.colors(k, start = 0.2, end = 0.9) to get k colors of gray scale
  

  if(is.null(showcol))
     showcol = F
  
  if(is.null(showrow))
    showrow = F

  if(showcol == T)
    colid = NA
  else
    colid = NULL
  
  if(showrow == F)
    rowIds = NA
  else
    rowIds = genename[geneSet]

  
  if(is.null(mypal))
    mypal = brewer.pal(8,"Dark2") %>%
      list(brewer.pal(10,"Paired")) %>%
      unlist()
    
    
    # mypal =c ("#0072c2", "#D55E00", "#999999", "#56B4E9", "#E69F00", "#CC79A7","lightblue", "#F0E442",
    #          "lightgreen", "deepskyblue4", "darkred", "#009E73", "maroon3","darkslategray",
    #          "burlywood1","darkkhaki", "#CC0000" )
  
  
  if(!is.null(palette.col)){
    palette(palette.col);
  }else  palette(mypal)


  if( any(rownames(exprData) != rownames(exprData)[order(as.numeric(rownames(exprData)))])) stop("Error: 'exprData' must have rownames in numerical ascending order!");
  if(length(RowSideColor)==1) RowSideColor=gray.colors(k, start = 0.2, end = 0.9)
  if(!Rowdistfun %in% c("correlation","euclidian")) stop("Rowdistfun must be one of 'cor' or 'euclidian'!")
  if(!Coldistfun %in% c("correlation","euclidian")) stop("Coldistfun must be one of 'cor' or 'euclidian'!")
  
  library(gplots)
  library(marray)
  cl=palette(mypal);
  
  exprData=exprData[geneSet,]

  ##-----------------------##
  ## Row dendrogram
  ##-----------------------##
  cat("\n -> Hierarchical clustering on genes... \n")
  if(Rowdistfun=="correlation"){	
    hc=hclustfun(distcor(exprData))
    subdist="dist method: 1-cor";
    distfunTRIX= distcor;
  } 
  if(Rowdistfun=="euclidian"){
    hc=hclustfun(disteucl(exprData))
    subdist="dist method: euclidian";
    distfunTRIX = disteucl;
  }
  
  rowv=as.dendrogram(hc)
  if(meanGrp){
    cat("\n -> calculating groups means... \n")
    suffix=paste("meanGrp",suffix,sep="_")
    exprData=t(apply(exprData,1,FUN=function(x){tapply(x,groups,mean,na.rm=T)}))
    cat("    Done \n")
    groups=factor(levels(groups),levels=levels(groups))

  }
  
  ##**********
  ## Rownames
  
  ##-----------------------##
  ## Col dendrogram
  ##-----------------------##
  
  gpcol=num2cols(as.numeric(groups))


  ##**********
  ## RowDendrogram
  
  # build dendrogram

  
  if(Coldistfun=="correlation")
    ColvOrd = exprData %>%
      t() %>%
      distcor()%>%
      hclustfun()%>%
      as.dendrogram()

  if(Coldistfun=="euclidian")
    ColvOrd = exprData %>%
    t() %>%
    disteucl()%>%
    hclustfun()%>%
    as.dendrogram()
  
  # re-order dendrogram
  
  if(!is.null(colOrder)){ # reorder col dendrogram
    if(length(colOrder)==ncol(exprData)){
      ord=colOrder	#vector of order values
    } else ord=1:ncol(exprData); # TRUE: create default 1:ncol
    ColvOrd=reorder(ColvOrd,ord,agglo.FUN = mean) # reorder by mean values
  }else {ColvOrd=ColvOrd} # no reordering
  
  
  #### heatmap  genes 
  
  #useRasterTF=F;
  
  ##-----------------------##
  ## plot Row dendrogram
  ##-----------------------##
  
  if(hclustGenes){
    cat("\n -> Plotting Dendrogram... \n")
    plot(hc,hang=-1,labels=FALSE,sub=paste("hclust method: ward2\n", subdist),xlab="",main="")
    hcgp=rect.hclust(hc,k=k,border="red")
    
    
    #hts=rev( tail( hc$height,15))
    #bp=barplot(hts,names.arg=1:length(hts))
    #text(x=bp,y=hts,label= formatC(hts,1,format="f"),pos=3,cex=0.8) 
    cat("    Done \n")
  }
  
  ##-----------------------##
  ## plotRowSideColor
  ##-----------------------##
  
  if(plotRowSideColor){
    if(!hclustGenes){
      #png(".tmp")
      plot(hc,hang=-1,labels=FALSE,xlab="",main="")
      hcgp=rect.hclust(hc,k=k,border="red")
      #file.remove(".tmp")
    }
    if(length(RowSideColor) == length(geneSet)){
      gpcolr=RowSideColor;
    }else {
      if(length(RowSideColor) != length(geneSet)){
        gphcc=as.data.frame(matrix( c(hc$labels[hc$order], rep(1:k,times=sapply(hcgp,length))),nrow=length(hc$labels)),stringsAsFactors=F)
        colnames(gphcc)=c("probe","cluster")
        gphccOrd=gphcc[order(as.numeric(gphcc[,1])),]
        hcgp=factor(paste("c",gphccOrd[,2],sep=""),levels=paste("c",rep(1:k),sep=""))
        gpcolr=num2cols(as.numeric(hcgp),c(rep(RowSideColor,20)[1:(k)]))
        print(RowSideColor)
      }else gpcolr=NULL
    }
  }else gpcolr=rep("white",nrow(exprData))
  
  
  ##-----------------------##
  ## plot Heatmap
  ##-----------------------##
  cat("\n -> Plotting HeatMap... \n")
  
  #print(gphcc)
  #View(exprData)

  #return(gphcc)
  par("mar")
  par(mar=c(5,5,1,1.10))
  hmp02 = heatmap.2(exprData,na.rm=T,dendrogram="both",labRow = rowIds,labCol=colid,scale=scale, RowSideColors=gpcolr, ColSideColors=gpcol,key=T,
                    keysize=1, symkey=T, trace="none",density.info="density",distfun=distfunTRIX, hclustfun=hclustfun,cexCol=cexcol,
                    Colv=ColvOrd,Rowv=rowv,na.color=na.color,cexRow=cexrow,useRaster=T,margins=margins,layout(lmat =rbind(4:3,2:1),lhei = c(0.05,1), lwid = c(0.1,1)),col=my_palette,key.par = list(cex=0.6))
  mtext(side=3,sort(levels(groups)),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[(1:length(levels(groups)))],cex=mycex,line=-1)
  if(notplot)
    dev.off()

  return(hmp02)
}
library(shiny)

# @ Author: Joe Cheng
# Also uses parallel, shinyjs, tools
# Create a long-running task, executed in a forked process. (Doesn't work on Windows)
# 
# The return value is a promise-like object with three
# methods:
# - completed(): FALSE initially, then TRUE if the task succeeds,
#   fails, or is cancelled. Reactive, so when the state changes
#   any reactive readers will invalidate.
# - result(): Use this to get the return value. While execution is
#   in progress, performs a req(FALSE). If task succeeded, returns
#   the return value. If failed, throws error. Reactive, so when
#   the state changes any reactive readers will invalidate.
# - cancel(): Call this to prematurely terminate the task.
create_forked_task <- function(expr) {
  makeReactiveBinding("state")
  state <- factor("running",
                  levels = c("running", "success", "error", "cancel"),
                  ordered = TRUE
  )
  
  result <- NULL
  
  # Launch the task in a forked process. This always returns
  # immediately, and we get back a handle we can use to monitor
  # or kill the job.
  task_handle <- parallel::mcparallel({
    force(expr)
  })
  
  # Poll every 100 milliseconds until the job completes
  o <- observe({
    res <- parallel::mccollect(task_handle, wait = FALSE)
    if (is.null(res)) {
      invalidateLater(100)
    } else {
      o$destroy()
      if (!is.list(res) || length(res) != 1 || !inherits(res[[1]], "try-error")) {
        state <<- "success"
        result <<- res[[1]]
      } else {
        state <<- "error"
        result <<- attr(res[[1]], "condition", exact = TRUE)
      }
    }
  })
  
  list(
    completed = function() {
      state != "running"
    },
    result = function() {
      if (state == "running") {
        # If running, abort the current context silently.
        # We've taken a reactive dependency on "state" so if
        # the state changes the context will invalidate.
        req(FALSE)
      } else if (state == "success") {
        return(result)
      } else if (state == "error") {
        stop(result)
      } else if (state == "cancel") {
        validate(need(FALSE, "The operation was cancelled"))
      }
    },
    cancel = function() {
      if (state == "running") {
        state <<- "cancel"
        o$destroy()
        tools::pskill(task_handle$pid, tools::SIGTERM)
        tools::pskill(-task_handle$pid, tools::SIGTERM)
        parallel::mccollect(task_handle, wait = FALSE)
      }
    }
  )
}### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



#' cutHeatmaps if a function that takes as input an heatmap object and depending on the cut height and the cluster
#' choosen render a ggplot object or an heatmap object
#'
#' @param hmp an heatmap object
#' @param height a numeric value to cut the dendogram
#' @param exprData a data frame with specific columns depending on the user's choices
#' @param DEGres a data frame corresponding to the xxx_topTableAll
#' @param groups a data frame of the choosen groups
#' @param cexcol a positive numbers, used as cex.axis in for the row or column axis labeling
#' @param cexrow a positive numbers, used as cex.axis in for the row or column axis labeling
#' @param labrow a character vectors with row and column labels to use
#' @param fileType a character to select the plot to display heatmap, boxplot or stripchart
#' @param scale a character indicating if the values should be centered and scaled in either the row direction or the column direction, or none
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#' @param col.hm a character vector
#' @param type a character to select the plot to display heatmap, boxplot or stripchart
#' @param las a numeric value
#' @param distfun function used to compute the distance (dissimilarity) between both rows and columns.
#' @param palette.col a character vector
#' @param num an item of the heatmap object corresponding to a specific cluster choosen by the user
#' @param ...
#'
#' @return a ggplot object or heatmapply object
#'
#' @export



cutHeatmaps = function(hmp, height, exprData, groups, cexcol = 1, cexrow = 1, labrow = F, genename = NULL ,
                       fileType = "png",scales = "row",meanGrp = F,mypal = NULL,
                       type = "None",las = 2,distfun = "cor",palette.col = NULL,num = 4,...)
{


  require(ggplot2)
  require(grid)
  require(gridExtra)

  plot.boxplot = ifelse(type == "Boxplot", T, F)
  plot.stripchart = ifelse(type == "LB" | type == "WB", T, F)
  hmp.plot = ifelse(type == "Heatmap", T, F)
  probes.boxplot = ifelse(type == "WB", T, F)

  if(is.null(mypal))
    mypal = brewer.pal(8,"Dark2") %>%
    list(brewer.pal(10,"Paired")) %>%
    unlist()

  if(!is.null(palette.col)){
    palette(palette.col);
  }else  palette(mypal)

  cl=palette(mypal);
  colid = colnames(exprData)
  rownames(exprData) = rownames(genename)
  exprData = exprData[labels(hmp$rowDendrogram), ]
  wdt = 900
  wdte = 12
  exprData0 = exprData
  groups0 = groups
  if (meanGrp) {
    cat("\n -> calculating groups means \n")
    exprData = t(apply(
      exprData,
      1,
      FUN = function(x) {
        tapply(x, groups, mean, na.rm = T)
      }
    ))
    groups = factor(levels(groups))
    colid = NA
    wdt = 600
    wdte = 8
  }

  if (distfun == "cor") {
    distfunTRIX = distcor
  } else {
    distfunTRIX = function(x, method = distfun) {
      dist(x, method = method)
    }
  }

  ###=======================
  ## cut the heatmap
  ###=======================
  
  # Cut the dendogram in 2 part with the desired height

  cut02 = cut(hmp$rowDendrogram, h = height)
  cat("\n -> size of", length(cut02$lower), "sub-dendrograms\n")
  print(sapply(cut02$lower, function(x)
    length(labels(x))))
  #To check the sub group population of genes

  cat(" ->export result tables for each subgroup \n")
  gpcol=num2cols(as.numeric(groups))



    # Row names of each clsuter
    HCgroupsLab=lapply(cut02$lower,function(x)labels(x))

    # Expression of each cluster
    HCgroupsLabExrs=lapply(HCgroupsLab,function(x)exprData0[x,])

    ## scaling
#		HCgroupsLabExrsCenterScale <- ifelse(scales=="row",lapply(HCgroupsLabExrs,function(y){t(scale(t(y),center=T,scale=T))}),HCgroupsLabExrs)
    
    if(scales=="row"){
      HCgroupsLabExrsCenterScale <- lapply(HCgroupsLabExrs,function(y){t(scale(t(y),center=T,scale=T))})
    }else HCgroupsLabExrsCenterScale <- HCgroupsLabExrs


    # mean expression for each group and each cluster
    grps=groups0
    HCgroupsLabExrsCenterScaleMean = lapply(HCgroupsLabExrsCenterScale, function(y) {
    t(apply( y,1,
      FUN = function(x) {
        tapply(x, grps, mean)
      }
    ))
  })


  if (plot.boxplot & !plot.stripchart) {

    ###=======================
    ## boxplot de la heatmap
    ###=======================

    cat(" ->Expression boxplot for each subgroup \n")

    library(Hmisc)
    library(reshape2)
    library(plotly)
    myplots <- list()

    for (i in 1:length(HCgroupsLabExrsCenterScaleMean)) {
      local({
        i <- i

        dataCentS = HCgroupsLabExrsCenterScaleMean[[i]]
        isplotable = apply(simplify2array(dataCentS), 1:2, sum, na.rm = TRUE)
        nProbes = nrow(dataCentS)

        ## boxplotggplot2
        footnote <-
          paste("Average expression Z-score over replicates; ",
                nProbes,
                " probes",
                sep = "")
        test <- footnote
        dataCentSm = melt(dataCentS)
        colnames(dataCentSm) = c("Probe", "Group", "Expression")
        ggbplot = ggplot(dataCentSm, aes(x = Group, y = Expression)) +
          theme_bw() + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank()
          ) +
          theme(axis.line = element_line(colour = "#888888")) +
          scale_fill_manual(values = cl[(1:length(levels(groups)))]) +
          geom_jitter(
            position = position_jitter(0.16) ,
            colour = "#888888",
            alpha = 0.4,
            shape = 16,
            size = 3
          ) +


          labs(title = paste("Cluster", i),
               subtitle = paste(caption = footnote)) +
          theme(
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.caption = element_text(size = 10, hjust = 0.5),
            axis.title.x = element_text(size = 10),
            axis.title.y = element_text(size = 10) ,
            axis.text.x = element_text(
              size = 10,
              colour = "#888888",
              angle = 360,
              hjust = 0.5
            ),
            axis.text.y = element_text(size = 12, colour = "#888888")
          )

        if (nProbes > 2)
          myplots[[i]] <<-
          (ggbplot +
             geom_violin(aes(fill = Group),alpha = 0.3,width=0.3)+
             geom_boxplot(width = 0.1, aes(fill = Group),alpha = 0.3))
        else
          myplots[[i]] <<- (ggbplot)

      })
    }
    return(myplots[[as.numeric(num)]])
  }

  #############"
  ### ggplot2
  #############"

  if (plot.stripchart) {
    cat(" ->Plotting Expression as stripchart for each subgroup \n")

    numProbes = lapply(HCgroupsLabExrsCenterScale, nrow)
    myplotsstrip <- list()
    for (i in 1:length(HCgroupsLabExrsCenterScale)) {
      local({
        i <- i

        dataStacked = as.vector(HCgroupsLabExrsCenterScale[[i]])

        dataStackeddt = cbind.data.frame(
          Expression = dataStacked,
          factor.trace = rep(grps, each = nrow(HCgroupsLabExrsCenterScale[[i]])),
          probeName = rep(
            rownames(HCgroupsLabExrsCenterScale[[i]]),
            ncol(HCgroupsLabExrsCenterScale[[i]])
          ),
          indName = rep(
            colnames(HCgroupsLabExrsCenterScale[[i]]),
            each = nrow(HCgroupsLabExrsCenterScale[[i]])
          )
        )
        dataStackeddt$Grp = dataStackeddt$factor.trace
        nindiv = table(dataStackeddt$Grp) / numProbes[[i]]

        if (all(nindiv == nindiv[1])) {
          nindiv = as.character(nindiv[1])

        } else
          nindiv = paste(nindiv, collapse = ", ")

        footnote <-
          paste("mean expression Z-score +/- 95%CI; N=",
                nindiv,
                "; ",
                numProbes[[i]],
                " probes",
                sep = "")

        if (!probes.boxplot) {
          
          ##=============
          ## plot stripchart
          ##
          #### jitter classique



          ggstrip = ggplot(dataStackeddt, aes(x = Grp, y = Expression)) +
            theme_bw() + theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()
            ) +
            theme(axis.line = element_line(colour = "#888888")) +
            geom_violin() +
            geom_jitter(
              position = position_jitter(0.16) ,
              colour = "#888888",
              alpha = 0.05,
              shape = 16,
              size = 3
            ) +

            stat_summary(
              fun.data = mean_cl_normal,
              geom = "errorbar",
              colour = "red",
              width = 0.1,
              size = 1
            ) +
            stat_summary(
              fun.y = mean,
              geom = "point",
              color = "red",
              size = 2
            )  +
            labs(
              title = paste("Cluster", i),
              y = "Expression Z-score",
              caption = footnote
            ) + 
            theme(
              plot.title = element_text(size = 20, hjust = 0.5),
              plot.caption = element_text(size = 10, hjust = 0.5),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10) ,
              axis.text.x = element_text(
                size = 10,
                colour = "#888888",
                angle = 360,
                hjust = 0.5
              ),
              axis.text.y = element_text(size = 12, colour = "#888888")
            )

          myplotsstrip[[i]] <<- (ggstrip)
        }

        else {
          ##### jitter classique avec boxplots par probe

          ggstrip = ggplot(data = dataStackeddt, aes(
            x = Grp,
            y = Expression,
            color = probeName
          )) +
            theme_bw() + theme(
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank()
            ) +
            theme(axis.line = element_line(colour = "#888888")) +
            geom_boxplot(outlier.shape = 1, notch = F) +
            theme(legend.position = "none") +
            stat_summary(
              fun.data = mean_cl_normal,
              geom = "errorbar",
              colour = "red",
              width = 0.1,
              size = 1
            ) +
            stat_summary(
              fun.y = mean,
              geom = "point",
              color = "red",
              size = 2
            )  +
            labs(
              title = paste("Cluster", i),
              y = "Expression Z-score",
              caption = footnote
            ) + 
            theme(
              plot.title = element_text(size = 24, hjust = 0.5),
              axis.title.x = element_text(size = 10),
              axis.title.y = element_text(size = 10) ,
              axis.text.x = element_text(
                size = 10,
                colour = "#888888",
                angle = 360,
                hjust = 0.5
              ),
              axis.text.y = element_text(size = 12, colour = "#888888")
            )
          myplotsstrip[[i]] <<- (ggstrip)
        }

      })
    }
    return(myplotsstrip[[as.numeric(num)]])
  }

  ####################"" END ggplot2


  ###=======================
  ## plot de la heatmap
  ###=======================


      # View(as.matrix(exprData[labels(cut02$lower[[num]]),]))
        # useRasterTF = T
        # hm02gp = heatmaply(
        # heatmaply(
        #   as.matrix(exprData[labels(cut02$lower[[num]]),]),
        #   height=900,col = col.hm,distfun = distfunTRIX,hclustfun = hclustfun,
        #   scale = scale, Colv = hmp$colDendrogram
        #   )%>%
        #   layout(margin = list(l = 130, b = 100))


        if(hmp.plot){
           cat(" ->plot heatmap for each subgroup \n")
          for(i in 1:length(cut02$lower)){
            if(length(labels(cut02$lower[[i]]))>1){
            rowIds=NA;

            # hm02gp = heatmaply(
            #   as.matrix(exprData[labels(cut02$lower[[2]]),]),
            #   height=900,distfun = distfunTRIX,hclustfun = hclustfun,
            #   scale = scale, Colv = hmp$colDendrogram
            #   )%>%
            #   layout(margin = list(l = 130, b = 100))
          #    if(length(labrow)>1){ rowIds=labrow[labels(cut02$lower[[i]])]
          #    }else if(labrow==T) rowIds=DEGres$ResTable$GeneName[labels(cut02$lower[[i]])]
          #if(length(labrow)>1){ rowIds=labrow[labels(cut02$lower[[i]])]
            # if(labrow==T) rowIds=DEGres$ResTable[labels(cut02$lower[[1]]),"GeneName"]
            
            useRasterTF=T;
            hm02gp=heatmap(exprData[labels(cut02$lower[[1]]),], Rowv=str(cut02$lower[[1]]),
            Colv=hmp$colDendrogram,
            distfun=distfunTRIX,
            hclustfun=hclustfun,
            labRow =rowIds,
            labCol=colid,
            ColSideColors=gpcol,
            cexCol=cexcol,
            cexRow=cexrow,
            scale=scales,
            na.rm=T,
            margins=c(8,8),
            useRaster=useRasterTF)
  }
  }
    return(hm02gp)
}
}
### Author: Yannick Lippi
### Modified: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0



#' decTestTRiX is a function 
#'
#' @param adj a data frame with the "adj.P.Val"
#' @param logfc a data frame with the "logFC"
#' @param pval a data frame with the "P.value"
#' @param DEGcutoff a numeric value for tresholding the data on the pvalue
#' @param FC a numeric value for tresholding the data on the FC value
#' @param cutoff_meth a character to choose the method appropriate, "FDR" in order to cut off with the "adj.P.Val" and "None" for the "P.value"; default is set to "FDR"
#' @param maxDE a numeric value that gives a maximal number of genes to display for all the differents contrasts
#' @param contrast a numeric value representing the length of the data frame
#'
#' @return \DEsel a matrix of double values containing the signficant genes
#' 
#' @export

decTestTRiX <- function(adj,logfc,pval, DEGcutoff = 0.05 ,FC = 1,cutoff_meth = "FDR",maxDE = NULL,contrast = 1:ncol(adj))

{
  ## select probes with cutoff_meth<= DEGcutoff and FoldChange > FC and nbr of selected probes < maxDE (if nb FC selected >maxDE)
  

  if (length(contrast) == 1)
    contrast = c(contrast, contrast)

  
  if (is.na(maxDE) || is.null(maxDE))
    maxDE = nrow(adj)

  
  if (cutoff_meth == "FDR") 
    pList = adj[, contrast]

  
  if(cutoff_meth=="None")
    pList= pval[,contrast]

  
  ## select on pvalue
  DEp = pList <= DEGcutoff
  
  
  ## select on FC
  DEFC = 2 ** abs(logfc[, contrast]) >= FC
  
  
  ## reduce selection to maxDE
  if (any(colSums(DEFC) > maxDE)) {
    
    
    DEmax = pList
    
    for (i in 1:ncol(DEFC))
      
    {
      if (maxDE > sum(DEFC[, i])) {
        maxDEi = sum(DEFC[, i])
      } else
        maxDEi = maxDE
      
      ord = order(DEmax[, i])
      msi = max(DEmax[ord, i][DEFC[ord, i]][1:maxDEi])

      DEmax[, i] = DEmax[, i] <= msi

    }
    
    DEsel = DEp & DEFC & DEmax
  }
  
  else{
    DEsel = DEp & DEFC
  }

  DEsel = which(rowSums(DEsel, na.rm = T) > 0)
  elements= list(DEsel, length(DEsel))
  
  
  return(elements)
  
}
# Author: Kevin Blighe
# Title: Publication-ready volcano plots with enhanced colouring and labelign
# Github: https://github.com/kevinblighe/EnhancedVolcano
# Modified by Franck Soubès (add 74-85;  modify 98-107, add 3 parameters displayLab, findfamily, topgenes)

EnhancedVolcano <- function(
    toptable,
    lab,
    x,
    y,
    selectLab = NULL,
    displaylab = NULL,
    findfamily = NULL,
    topgenes = NULL,
    regulationvolc = NULL,
    xlim = c(min(toptable[,x], na.rm=TRUE),
        max(toptable[,x], na.rm=TRUE)),
    ylim = c(0, max(-log10(toptable[,y]), na.rm=TRUE) + 5),
    xlab = bquote(~Log[2]~ "fold change"),
    ylab = bquote(~-Log[10]~italic(P)),
    axisLabSize = 16,
    pCutoff = 0.05,
    pLabellingCutoff = pCutoff,
    FCcutoff = 2.0,
    title = "",
    titleLabSize = 16,
    transcriptPointSize = 2.8,
    transcriptLabSize = 5.0,
    col = c("grey30", "forestgreen", "royalblue", "red2"),
    colAlpha = 1/2,
    legend = c("NS","Log2 FC","P","P & Log2 FC"),
    legendPosition = "top",
    legendLabSize = 10,
    legendIconSize = 3.0,
    DrawConnectors = FALSE,
    widthConnectors = 0.5,
    colConnectors = "black",
    cutoffLineType = "longdash",
    cutoffLineCol = "black",
    cutoffLineWidth = 0.4)
    
{
    if(!requireNamespace("ggplot2")) {
        stop("Please install ggplot2 first.", call.=FALSE)
    }

    if(!requireNamespace("ggrepel")) {
        stop("Please install ggrepel first.", call.=FALSE)
    }

    if(!is.numeric(toptable[,x])) {
        stop(paste(x[i], " is not numeric!", sep=""))
    }

    if(!is.numeric(toptable[,y])) {
        stop(paste(x[i], " is not numeric!", sep=""))
    }

    requireNamespace("ggplot2")
    requireNamespace("ggrepel")
    requireNamespace("dplyr")
    i <- xvals <- yvals <- Sig <- NULL
    
    toptable <- as.data.frame(toptable)
    toptable$GeneName <- sapply(toptable$GeneName, function(v) {
      if (is.character(v)) return(toupper(v))
      else return(v)
    })

    toptable$Sig <- "NS"
    toptable$Sig[(abs(toptable[,x]) > FCcutoff)] <- "FC"
    toptable$Sig[(toptable[,y]<pCutoff)] <- "P"
    toptable$Sig[(toptable[,y]<pCutoff) &
        (abs(toptable[,x])>FCcutoff)] <- "FC_P"
    toptable$Sig <- factor(toptable$Sig,
        levels=c("NS","FC","P","FC_P"))
    
  
    
    if(is.na(topgenes) && !is.na(displaylab) ){
      selectLab <- as.character(displaylab)
    }
    else if(is.na(topgenes) && is.na(displaylab) && !is.na(findfamily) )
      selectLab <- as.character(findfamily)
    else if(is.na(topgenes)&& is.na(displaylab)&& is.na(findfamily)){
      selectLab <- ""
    }
    else{
      if(regulationvolc == "both")
        toptable$abs <-  unlist(abs(toptable[x]))
      else if(regulationvolc == "up"){
        toptable$abs <- unlist((toptable[x]))
      }
      else{
        toptable$abs <- unlist((toptable[x]))
      }
        
      toptable$X <- rownames(toptable)
      myval <- toptable %>%   dplyr::filter(Sig =="FC_P") %>% dplyr::select(GeneName,X,abs)  %>%  
      {if (regulationvolc == "down") top_n(.,-topgenes) else top_n(.,topgenes)}
      myvalueind <- myval$X
      selectLab <- as.character(myval$GeneName)
      
    }
  
      
    if (min(toptable[,y], na.rm=TRUE) == 0) {
        warning("One or more P values is 0. Converting to minimum possible value...", call. = FALSE)
        toptable[which(toptable[,y] == 0), y] <- .Machine$double.xmin
    }
    
    toptable$lab <-  sapply(toptable$GeneName, function(v) {
      if (is.character(v)) return(toupper(v))
      else return(v)
    })
    
    
    
    toptable$xvals <- toptable[,x]
    toptable$yvals <- toptable[,y]
  
    
   if (!is.null(selectLab)) {
    if(!is.na(topgenes) && is.na(displaylab)&& is.na(findfamily)){
    names.new <- rep("", length(toptable$lab))
    indices <- which(toptable$X %in% myvalueind)
    names.new[indices] <- as.character(toptable$GeneName[indices])
    toptable$lab <- names.new
    }
    else {
        names.new <- rep("", length(toptable$lab))
        indices <- which(toptable$GeneName %in% selectLab)
        names.new[indices] <- as.character(toptable$GeneName[indices])
        toptable$lab <- names.new
      }
    }
    
    subdata = subset(toptable,
                     toptable[,y]<pLabellingCutoff &
                       abs(toptable[,x])>FCcutoff) 

    plot <- ggplot2::ggplot(toptable,
            ggplot2::aes(x=xvals, y=-log10(yvals))) +

        ggplot2::geom_point(ggplot2::aes(color=factor(Sig)),
            alpha=colAlpha, size=transcriptPointSize) +

        ggplot2::scale_color_manual(values=c(NS=col[1],
            FC=col[2],
            P=col[3],
            FC_P=col[4]),
            labels=c(NS=legend[1],
            FC=paste(legend[2], sep=""),
            P=paste(legend[3], sep=""),
            FC_P=paste(legend[4], sep=""))) +

        ggplot2::theme_bw(base_size=24) +

        ggplot2::theme(
            legend.background=ggplot2::element_rect(),
            plot.title=ggplot2::element_text(angle=0,
                size=titleLabSize,
                face="bold",
                vjust=1),

            panel.grid.major=ggplot2::element_blank(),
            panel.grid.minor=ggplot2::element_blank(),

            axis.text.x=ggplot2::element_text(angle=0,
                size=axisLabSize,
                vjust=1),
            axis.text.y=ggplot2::element_text(angle=0,
                size=axisLabSize,
                vjust=1),
            axis.title=ggplot2::element_text(size=axisLabSize),

            legend.position=legendPosition,
            legend.key=ggplot2::element_blank(),
            legend.key.size=ggplot2::unit(0.5, "cm"),
            legend.text=ggplot2::element_text(
                size=legendLabSize),
            title=ggplot2::element_text(
                size=legendLabSize),
            legend.title=ggplot2::element_blank()) +

        ggplot2::guides(colour = ggplot2::guide_legend(
            override.aes=list(size=legendIconSize))) +

        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +

        ggplot2::xlim(xlim[1], xlim[2]) +
        ggplot2::ylim(ylim[1], ylim[2]) +

        ggplot2::ggtitle(title) +

        ggplot2::geom_vline(xintercept=c(-FCcutoff, FCcutoff),
            linetype=cutoffLineType,
            colour=cutoffLineCol,
            size=cutoffLineWidth) +

        ggplot2::geom_hline(yintercept=-log10(pCutoff),
            linetype=cutoffLineType,
            colour=cutoffLineCol,
            size=cutoffLineWidth)
   
    
    
    if (DrawConnectors == TRUE) {
        plot <- plot + ggrepel::geom_text_repel(max.iter = 100,
            data=subdata ,
            ggplot2::aes(label=subset(toptable,
                toptable[,y]<pLabellingCutoff &
                    abs(toptable[,x])>FCcutoff)[,"lab"]),
                size = transcriptLabSize,
                segment.color = colConnectors,
                segment.size = widthConnectors,
                vjust = 1.0)
    } else if (DrawConnectors == FALSE && !is.null(selectLab)) {
        plot <- plot + ggplot2::geom_text(data=subdata,
            ggplot2::aes(label=subset(toptable,
                toptable[,y]<pLabellingCutoff &
                    abs(toptable[,x])>FCcutoff)[,"lab"]),
                size = transcriptLabSize,
		check_overlap = T,
                vjust = 1.0)
    } else if (DrawConnectors == FALSE && is.null(selectLab)) {
        plot <- plot + ggplot2::geom_text(data=subdata,
            ggplot2::aes(label=subset(toptable,
                toptable[,y]<pLabellingCutoff &
                    abs(toptable[,x])>FCcutoff)[,"lab"]),
                size = transcriptLabSize,
                check_overlap = F,
                vjust = 1.0)
    }
    
    if(!is.na(topgenes)) subdata <- filter(subdata, lab != "")
    mylist = list(plot, subdata)
    return(mylist)
}
##### Formating function for microarray data #### 
##### Franck Soubès


#' formating is a function alpha version of the higher elaborate decTestTRiX function 
#'
#' @param adj
#' @param pval 
#'
#' @return
#' @export
#'
#' 
#' @export

formating = function( adj, pval){
  
  
  passingval = adj %>%
    apply(2,FUN = function(x){return(x < pval)}) %>%
    apply(1,sum) 
  
  passingval = which( passingval > 0)
  
  cat("Il y a",length(passingval),"gène significatifs")

  return(passingval)

}


#' transform a dataframe containing factor for different levels not optimal tho
#'
#' @param dataframe 
#'
#' @return
#' 
#' @export


transform <- function(dataframe,toast){
  
  myl = list()
  cpt = 1
  for (i in toast) {
    command <- paste0(i, "<-subset(dataframe, Grp=='", i, "')")
    test = eval(parse(text=command))
    X = test$X
    Grp = test$Grp
    myl[[cpt]] = data.frame(X ,Grp)
    cpt = cpt+1
    dyn_grp <- Reduce(function(x, y) merge(x, y, all=TRUE), myl, accumulate=FALSE)
  }
  
  return(dyn_grp)
}





#' This function returns an integer for the number of significant genes
#'
#' @param adj a data frame
#' @param elem a list
#' @param pv a list
#'
#' @return \grp1 of class data frame
#' 
#' @export

evaluatesign = function(adj,elem,pv){
  

  grp1 = adj[,c(elem)] %>%
    sapply( FUN = function(x){return(x < pv)}) %>%
    data.frame() %>%
    filter(. == T) %>%
    nrow()
  
  return(grp1)
}


#' This function returns an integer for the number of significant genes using parallelism
#' 
#' @param adj 
#' @param elem 
#' @param pv 
#'
#' @return \grp1 of class data.frame
#' 
#' @export




evaluatesignpar = function(adj,elem,pv) { ### for benchmarking 
  
  
  grp1 = foreach(i = iter(adj[elem], by = "col"), .combine = c) %dopar%  
    (sign= {i < pv}) %>%
    as.data.frame() %>%
    filter(. == T) %>%
    nrow()
  
 return(grp1) 
  
}


#' Create a data frame containing the number of signficant genes for different conditions pval and log fc
#'
#' @param adj a data frame containing the adjusted p-value
#'
#' @return \dtsign a data frame 
#' 
#' @export

createdfsign = function(adj) {


  dtsign = data.frame(matrix(ncol = 2, nrow = length(adj)))
  y <- c("FDR < 0.01", "FDR < 0.05")

  dtsign = data.frame(matrix(ncol <- 2, nrow <- length(adj)))
  y <- c("pvalue(0.01)", "pvalue(0.05)")

  colnames(dtsign) <- y
  rownames(dtsign) <- colnames(adj)
  pvalue = c(0.01, 0.05)
  
  i <- 1
  for (pv in pvalue) {
    for (elem in colnames(adj)) {
      
      if (i %% constmod == 0) {
        i <- 1
      }
      if (pv == 0.05)
      {

        dtsign$`FDR < 0.05`[i] = evaluatesignpar(adj, elem, pv)
        i = i + 1
      }
      else{
        
        dtsign$`pvalue(0.01)`[i] = evaluatesignpar(adj, elem, pv)
        i <- i + 1

      }
    }
  }
  return(dtsign)
}


isimilar <- function(restab, pdata, workingset){
  
  sameids <- all(restab[[1]] == workingset [[1]])
  samedesign <- all(colnames(workingset[-1])== pdata[[1]])
  return(list(sameids,samedesign))
  
}

#' This function returns a data frame of the element which are superior to a vector character 1.2,2,4,6 and 10 and for a defined pvalue
#'
#' @param alltop a data frame
#' @param pval a numeric pvalue
#'
#' @return \fcpval a data frame 
#' 
#' @export

myfinalfc <- function(alltop, pval, testrix) {
  j = 1
  colnames(myfinalfc)
  whatest  = ifelse(testrix == "FDR", T, F)
  if (whatest)
    adj = alltop[, grep("^adj.P.Val", names(alltop), value = TRUE), drop= F]
  else
    adj = alltop[, grep("^P.value", names(alltop), value = TRUE),drop= F]
  
  logfc = alltop[, grep("^logFC", names(alltop), value = TRUE),drop= F]
  myfc = c(1, 1.2, 2, 4, 6, 10)
  fcpval = data.frame(matrix(ncol = length(myfc), nrow = length(adj)))
  mycolnames = c("FC>1.0", "FC >1.2" , "FC >2", "FC >4", "FC >6", "FC >10")
  

  for (fc in myfc) {
    fcpval[j] = cbind.data.frame(colSums(adj < pval &
                                           2 ** abs(logfc) > fc))
    j = j + 1
  }
  
  names(logfc) =  gsub(
    pattern = "^logFC_",
    replacement = "",
    x = names(logfc),
    perl =  TRUE
  )
  
  colnames(fcpval) = mycolnames
  rownames(fcpval) = colnames(logfc)
  return(fcpval)
  
}

#' This function returns a transformed data frame of character type to a data frame of factor type
#'
#' @param datach a data frames
#'
#' @return \datach a data frame 
#' 
#' @export


chartofa = function(datach){
  
  datach[] <- lapply( datach, factor)
  col_names <- names(datach)
  datach[col_names] <- lapply(datach[col_names] , factor)
  
  return(datach)
}


meanrankgenes  <- function(dtsign, stat , rankcomp=NULL, multcomp, regulationvolc=NULL, jvenn = F){
  
  selcomp <-  paste0(stat, multcomp )
  options(datatable.optimize=1)
  
  for (i in selcomp) {
    dtsign[[i]] = as.numeric(as.character(dtsign[[i]]))
  }
  
  summarizetable <- dtsign %>% select(GeneName, paste0(stat, multcomp))  %>% 
    as.data.table() %>% .[,lapply(.SD,function(x) mean=round(mean(x), 3)),"GeneName"] %>% as.data.frame() 
  
  if(!jvenn){
  summarizetable$rank <- summarizetable %>% select(paste0(stat , rankcomp) ) %>% rank(.) 
  summarizetable <- if(regulationvolc == "down") summarizetable %>% arrange( desc(-rank) ) else summarizetable %>% arrange( desc(rank) )  
  }
  
  return(summarizetable)
}


#' This function returns a data frame of the significant genes associated with the corresponding cluster index
#'
#' @param hmp01_All a heatmap object
#' @param exprData a vector of indices for the significant genes who have crossed the treshold pval and fc
#' @param pval a data frame of the alltoptable
#' @param height numeric value where the dendogram is cut off
#'
#' @return a data frame
#' @export
#'

heatmtoclust = function( hmp01_All, exprData, pval ,height= 5){
  
  cut02 = cut(hmp01_All$rowDendrogram, h = height )
  
  HCgroupsLab = lapply(cut02$lower, function(x)
    labels(x))
  id = colnames(pval[1])
  
  final = exprData[rev(hmp01_All$rowInd), hmp01_All$colInd]
  
  my_last= as.integer(lapply(seq(length(HCgroupsLab)), function(x)
  {return(tail(HCgroupsLab[[x]],1))}))
  
  mygen = as.integer(row.names(final))
  pval$X = as.integer(rownames(pval))
  
  heatmclust = pval %>%
    dplyr::select (X,id,GeneName) %>%
    filter( X %in% mygen) %>%
    left_join(data.frame(X=mygen), . , by="X") %>%
    arrange(-row_number())
  
  i = 1
  for(row in 1:nrow(heatmclust)){
    if(heatmclust$X[row] == my_last[i] ){
      heatmclust$cluster[row] = i
      i = i+1
    }
    else
      heatmclust$cluster[row] = i
  }
  
  return(heatmclust)
  
}





#require(goseq)
#require(GO.db)
library(dplyr)


#' gosearch is a function that return a list of data frame containing the go ids for the different clusters
#'
#' @param hm01 data frame object
#' @param species character
#' @param ids package use to perform the enrichment
#' @param clusterlist list
#'
#' @return list of data frames
#' @export
#'

gosearch <- function(hm01, species, ids, clusterlist) {
  library(goseq)
  library(GO.db)
	
  for (i in 1:NROW(unique(hm01$cluster))) {
    genlist <- hm01[!duplicated(hm01$GeneName),]
    genlist <-genlist %>% dplyr::select(cluster, GeneName)   %>% filter(cluster == i)
    final = as.double(matrix(1, length(genlist$cluster)))
    names(final) = (genlist$GeneName)
    
    h <- function(w)
        if (any(grepl("constraints|library", w)))
          invokeRestart("muffleWarning")
    
    e <- function(y)
      if(any(grepl("Rplots.pdf", y)))
        invokeRestart("muffleWarning")

    pwf <- tryCatch({
      withCallingHandlers(nullp(final, species, ids ,plot.fit=FALSE), warning = h, error = e) %>% na.omit()
    }, warning = function(e) {
      warning("40 % of genes are misssing")
      return(NULL)
    })
    
    #pwf <- nullp(final, species, ids,plot.fit=FALSE) %>% na.omit()
    cat(length(row.names(pwf)))
    
    if (!is.null(pwf)) {
      finalons <- goseq(pwf, species , ids, use_genes_without_cat = F)
      clusterlist[[i]] = filter(finalons, numInCat > 1 ) %>%
        arrange(desc(numInCat))
    }
    else
      clusterlist[[i]] = NULL
  }
  return(clusterlist)
}

#' wclust is a function that return a tabular file containing the top n genes for the different clusters, the go ids associated to this cluster, the id's term and the definition of the term (Old function working with go seq package)
#'
#' @param clusterlist list of data frames
#' @param filename name of the output file 
#' @param min GO ids that are not represented significally by default = 2 
#' @param top top go ids to display
#'
#' @return txt file
#' @export

wclust <- function(clusterlist, filename, min, top)  {
  cat("Clustering", file = filename)
  sink(filename)
  sink()
  con <- file(filename, "w")
  for (i in 1:NROW(clusterlist)) {
    if (!i == 1)
      cat("--------------------------------------\n", file = con)
    cat(paste("cluster:", i),  file = con)
    if (!i == 1)
      cat("\n--------------------------------------\n", file = con)
    cat("\n--------------------------------------\n", file = con)
    if (!length(clusterlist[[1]][[i]]) == 0) {
      for (go in top:min) {
        cat(paste("GOID:", as.character(GOID(
          clusterlist[[i]][[1]][[go]]
        ))), file = con)
        cat("\n", file = con)
        cat(paste("Term:", as.character(Term(
          clusterlist[[i]][[1]][[go]]
        ))), file = con)
        cat("\n", file = con)
        cat(paste("Definition:", as.character(Definition(
          clusterlist[[i]][[1]][[go]]
        ))), file = con)
        cat("\n", file = con)
        cat("--------------------------------------\n", file = con)
      }
      cat("\n", file = con)
    }
  }
  
  close(con)
}
#' probnamtoentrez is a function that convert Gene symbols to entrez IDS
#'
#' @param hm01 data frame 
#' @param mypack package specific to the genome
#'
#' @return lists of entrez IDS
#' @export
#'

probnamtoentrez <- function(hm01,  mypack) {
  
  lapply(1:NROW(unique((hm01$cluster))), function(x) {
    entrezids <- hm01 %>%
      filter(cluster == x) %>%
      dplyr::select(GeneName) %>%
      unlist() %>%
      as.character() %>%
      mget(x = .,envir = mypack,ifnotfound = NA) %>%
      unlist() %>%
      unique() %>%
      .[!is.na(.)]
    
    return(entrezids)
  })
}

#' probnamtoentrezvenn is a function that convert Gene symbols to entrez IDS
#'
#' @param venngenes lists of genes
#' @param mypack package specific to the genome
#'
#' @return lists of entrez IDS
#' @export
#'
probnamtoentrezvenn <- function(venngenes, mypack){
  
  entrezids <- venngenes %>%
    unlist() %>%
    as.character() %>%
    mget(x = .,envir = mypack,ifnotfound = NA) %>%
    unlist() %>%
    unique() %>%
    .[!is.na(.)]
  
}

#' entreztosymb is a function which aim is to convert entrez IDS to gene symbols
#'
#' @param myentz lists of genes
#' @param mypack package specific to the genome
#'
#' @return lists of gene symbols
#' @export
#'

entreztosymb <- function(myentz, mypack){
lapply(1:NROW(myentz), function(x)
  as.vector(unlist(mget(myentz[[x]], envir=mypack, ifnotfound=NA))))
}

#' davidquery is a function which aim is to querrying DWS and performing go term enrichment analysis 
#'
#' @param entrezids list of entrez IDS
#' @param species character name of the species whose genes are enriched
#' @param mycat category of the enrichment analysis: MF, CC, BP or KEGG pathway
#'
#' @return list of data frames for each cluster containing 
#' @export
#'
  
davidquery <- function(entrezids, species, mycat) {
  test = lapply(1:NROW(entrezids), function(x) {
    david <- DAVIDWebService$new(email = "get-trix@genotoul.fr", url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
    RDAVIDWebService::setTimeOut(david, 90000)
    result <-
      addList(
        david,
        entrezids[[x]],
        idType = "ENTREZ_GENE_ID",
        listName = "testList",
        listType = "Gene"
      )
    
    selectedSpecie = (species)
    specieLocation = grep(selectedSpecie, RDAVIDWebService::getSpecieNames(david))
    setCurrentSpecies(object = david, species = specieLocation)
    setAnnotationCategories(david, mycat) #c("GOTERM_MF_ALL", "GOTERM_CC_ALL", "GOTERM_BP_ALL"))  "KEGG_PATHWAY"
    mydav = as.data.frame(cbind(getFunctionalAnnotationChart(object=david, threshold=1, count=0L)))  %>%
      filter(Count>1) %>% arrange(desc(Count))  %>% dplyr::select( Category:Count, List.Total:Pop.Total,X.,PValue,Genes,Fold.Enrichment, Bonferroni, Benjamini)
    colnames(mydav)[[7]] = "percent"
    return(mydav)
  })
}


#' davidqueryvenn is a function which aim is to querrying DWS and performing Functional Annotation Clustering
#'
#' @param entrezids list of entrez IDS
#' @param species character name of the species whose genes are enriched
#'
#' @return david object
#' @export
#'

davidqueryvenn <- function(entrezids, species){
  
  david <- DAVIDWebService$new(email = "get-trix@genotoul.fr", url = "https://david.ncifcrf.gov/webservice/services/DAVIDWebService.DAVIDWebServiceHttpSoap12Endpoint/")
  RDAVIDWebService::setTimeOut(david, 90000)
  addList(
    david,
    entrezids,
    idType = "ENTREZ_GENE_ID",
    listName = "myqueryvenn",
    listType = "Gene"
  )
  
  selectedSpecie = (species)
  specieLocation = grep(selectedSpecie, RDAVIDWebService::getSpecieNames(david))
  setCurrentSpecies(object = david, species = specieLocation)
  getClusterReport(david, type = "Term")
  
}


#' mygotavres is a function which aim is to summarise the top 10 for each different cagetogies of the  the DAVID gene set enrichment analysis data table
#'
#' @param davtab data frame
#'
#' @return list of data frames
#' @export
#'

mygotabres <- function(davtab){
  
  lapply(seq(unique(davtab$Category)), function(x){
    return(davtab %>% select(Category, Term,Fold.Enrichment,Benjamini,Count,List.Total,Pop.Hits)%>%
             filter(Category == unique(davtab$Category)[[x]]) %>%
             top_n(10, Fold.Enrichment) %>% arrange(desc(Fold.Enrichment))%>% tibble::rownames_to_column("Top")
    )})
}
  



### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


require(dplyr)
require(RColorBrewer)



## Add jackknife correlation good for false positives with the pval methods add citation ....


#' distcos is a function that computes the cosine similarity for a given matrix
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

distcos <- function(x){ #Jonathan Chang
  as.dist(1 - x%*%t(x)/(sqrt(rowSums(x^2) %*% t(rowSums(x^2))))) 
}



#' distcor is a function that computes the pearson correlation 
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

distcor<-function(x) {as.dist(1-cor(t(x),use="pairwise.complete.obs"))}


#' disteuc is a function that computes the euclidean distance matrices
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

disteucl<-function(x) {dist(x,method="euclidian")}


#' hclustfun is a function that performs a hiearchical cluster analysis with the method of Ward
#'
#' @param d 
#'
#' @return an object of class hclust
#' 
#' @export

hclustfun=function(d,e = "ward.D2") {hclust(d,method=e)} 

#' disteuc is a function that computes the euclidean distance matrices
#'
#' @param x 
#'
#' @return a matrix distance
#' 
#' @export

distman<-function(x) {dist(x,method="manhattan")}

#require("Biobase")  
#require("marray") 


#' num2cols is a function that gives a character vector of color names for a given numeric vector
#'
#' @param numVector a numeric vector
#' @param colp a character vector
#'
#' @return a matrix object 
#' 
#' @export

num2cols=function(numVector,colp=palette()){
  
  gpcol=as.data.frame(numVector)
  numCols=length(unique(gpcol[,1]))
  mycol <- sort(unique(gpcol$numVector))
  if(length(colp) <numCols) warning("number of color names < number of levels! Give a color palette with at least ",numCols," terms to 'col' argument")
  cols=as.data.frame(matrix(c(mycol,colp[1:(numCols)]),nrow=numCols)) ## couleurs pour les groupes
  myColFun=function(x){
    as.character(cols[cols[,1]==x[1],2])
  }
  apply(gpcol,1,FUN=myColFun)
  
}



############################################################################
##   plotHeatmaps() function                                              
############################################################################ 


#' hmbis is an event reactive function that pre-computed hierarchical clustering on microarray data
#'
#' @param exprData  data frame with all the individuals selected
#' @param geneSet  data frame with the indexes corresponding to the sigificant genes
#' @param groups  data frame with the corresponding groups 
#' @param workingPath  access path
#' @param k  numeric value which aim is to defined the treshold value to cut the dendogram
#' @param fileType  character value which extension corresponds to different file types
#' @param colOrder  positive numbers, used as cex.axis in for the row or column axis labeling
#' @param na.color color used for missing values
#' @param hclustGenes  function used to compute the hierarchical clustering when Rowv or Colv are not dendrograms
#' @param meanGrp  boolean value to computes the mean for each groups; default = F
#' @param plotRowSideColor  boolean value that colors or not the rows side
#' @param RowSideColor  character vector of length nrow containing the color names for a vertical side bar that may be used to annotate the rows of x.
#' @param Rowdistfun  function used to compute the distance for the rows
#' @param Coldistfun  function used to compute the distance for the columns
#' @param palette.col NULL
#' @param notplot  boolean
#'
#' @return list of objects which aim is to being passed as argument in the plotHeatmaps function
#' 
#' @export
#' 


truncatedhat=function(exprData,geneSet,groups,workingPath=getwd(),k=3,fileType="png",algo = "ward.D2",
                      colOrder=NULL,na.color="black",hclustGenes=T,meanGrp=F,plotRowSideColor=T,mypal=NULL,
                      RowSideColor=c("gray25","gray75"), Rowdistfun="correlation",Coldistfun="correlation" ,palette.col=NULL , notplot = T,genename=pval){
  
  
  
  
  if( any(rownames(exprData) != rownames(exprData)[order(as.numeric(rownames(exprData)))])) stop("Error: 'exprData' must have rownames in numerical ascending order!");
  if(length(RowSideColor)==1) RowSideColor=gray.colors(k, start = 0.2, end = 0.9)
  if(is.null(mypal))
    mypal = brewer.pal(8,"Dark2") %>%
      list(brewer.pal(10,"Paired")) %>%
      unlist()
  
  if(!is.null(palette.col)){
    palette(palette.col);
  }
  else  
    palette(mypal)
  
  cl=palette(mypal);
  rownames(exprData) = rownames(genename)
  exprData=exprData[geneSet,]
  
  
  
  ##-----------------------##
  ## Row dendrogram
  ##-----------------------##
  cat("\n -> Hierarchical clustering on genes... \n")
  if(Rowdistfun=="correlation"){	
    hc=hclustfun(distcor(exprData),algo)
    subdist="dist method: 1-cor";
    distfunTRIX= distcor;
  } 
  if(Rowdistfun=="euclidian"){
    hc=hclustfun(disteucl(exprData),algo)
    subdist="dist method: euclidian";
    distfunTRIX = disteucl;
  }
  if(Rowdistfun=="manhattan"){
    hc=hclustfun(disteucl(exprData),algo)
    subdist="dist method: manhattan";
    distfunTRIX = distman;
  }
  if(Rowdistfun=="cosine"){
    hc=hclustfun(disteucl(exprData),algo)
    subdist="dist method: cosine";
    distfunTRIX = distcos;
  }
  
  rowv=as.dendrogram(hc)
  if(meanGrp){
    cat("\n -> calculating groups means... \n")
    exprData=t(apply(exprData,1,FUN=function(x){tapply(x,groups,mean,na.rm=T)}))
    cat("    Done \n")
    groups=factor(levels(groups),levels=levels(groups))
    
  }
  
  ##**********
  ## Rownames
  
  ##-----------------------##
  ## Col dendrogram
  ##-----------------------##
  
  gpcol=num2cols(as.numeric(groups))
  
  
  ##**********
  ## RowDendrogram
  
  # build dendrogram
  
  
  if(Coldistfun=="correlation")
    ColvOrd = exprData %>%
    t() %>%
    distcor()%>%
    hclustfun()%>%
    as.dendrogram()
  
  if(Coldistfun=="euclidian")
    ColvOrd = exprData %>%
    t() %>%
    disteucl()%>%
    hclustfun()%>%
    as.dendrogram()
  
  if(Coldistfun=="manhattan")
    ColvOrd = exprData %>%
    t() %>%
    distman()%>%
    hclustfun()%>%
    as.dendrogram()
  
  
  if(Coldistfun=="cosine")
    ColvOrd = exprData %>%
    t() %>%
    distcos()%>%
    hclustfun()%>%
    as.dendrogram()
  
  # re-order dendrogram
  
  if(!is.null(colOrder)){ # reorder col dendrogram
    if(length(colOrder)==ncol(exprData)){
      ord=colOrder	#vector of order values
    } else ord=1:ncol(exprData); # TRUE: create default 1:ncol
    ColvOrd=reorder(ColvOrd,ord,agglo.FUN = mean) # reorder by mean values
  }else {ColvOrd=ColvOrd} # no reordering
  
  
  #### heatmap  genes 
  
  #useRasterTF=F;
  
  ##-----------------------##
  ## plot Row dendrogram
  ##-----------------------##
  
  if(hclustGenes){
    cat("\n -> Plotting Dendrogram... \n")

    plot(hc,hang=-1,labels=FALSE,sub=paste("hclust method: ward2\n", subdist),xlab="",main="")
    hcgp=rect.hclust(hc,k=k,border="red")
    myheight <- rev( tail( hc$height,15))
    
    cat("    Done \n")
  }
  
  ##-----------------------##
  ## plotRowSideColor
  ##-----------------------##
  
  if(plotRowSideColor){
    if(!hclustGenes){
      
      plot(hc,hang=-1,labels=FALSE,xlab="",main="")
      hcgp=rect.hclust(hc,k=k,border="red")

    }
    if(length(RowSideColor) == length(geneSet)){
      gpcolr=RowSideColor;
    }else {
      if(length(RowSideColor) != length(geneSet)){
        gphcc=as.data.frame(matrix( c(hc$labels[hc$order], rep(1:k,times=sapply(hcgp,length))),nrow=length(hc$labels)),stringsAsFactors=F)
        colnames(gphcc)=c("probe","cluster")
        gphccOrd=gphcc[order(as.numeric(gphcc[,1])),]
        hcgp=factor(paste("c",gphccOrd[,2],sep=""),levels=paste("c",rep(1:k),sep=""))
        gpcolr=num2cols(as.numeric(hcgp),c(rep(RowSideColor,20)[1:(k)]))
        print(RowSideColor)
      }else gpcolr=NULL
    }
  }else gpcolr=rep("white",nrow(exprData))
  
  rowIds = genename$GeneName[geneSet]
  
  
  objforheat = list(exprData,distfunTRIX,ColvOrd,rowv,gpcol,gpcolr,rowIds,myheight[[k]])
  
  
  return(objforheat)
}


#' plotHeatmaps is a function that creates a false color image with a dendogram added to the left side and the top
#'
#' @param exprData  data frame with all the individuals selected
#' @param geneSet  data frame with the indexes corresponding to the sigificant genes
#' @param groups  data frame with the corresponding groups 
#' @param workingPath an access path
#' @param k  numeric value which aim is to defined the treshold value to cut the dendogram
#' @param fileType  character value which extension corresponds to different file types
#' @param cexcol   positive numbers, used as cex.axis in for the row or column axis labeling
#' @param cexrow  positive numbers, used as cex.axis in for the row or column axis labeling
#' @param colOrder  positive numbers, used as cex.axis in for the row or column axis labeling
#' @param labrow  character vectors with row and column labels to use
#' @param na.color color to use for missing value 
#' @param scale  character indicating if the values should be centered and scaled in either the row direction or the column direction, or none
#' @param rowv  dendogram object
#' @param ColOrd  positive numbers, used as cex.axis in for the row or column axis labeling
#' @param gpcol  matrix with colors associated to each groups 
#' @param gpcolr  matrix with gray color depending on the clusters
#' @param distfunTRIX function that computes whether euclidian or pearson for Hierarchical Clustering
#' @param RowSideColor a character vector of length nrow containing the color names for a vertical side bar that may be used to annotate the rows of x.
#' @param palette.col NULL
#' @param margins  numeric vector of length 2 containing the margins (see par(mar= *)) for column and row names, respectively.
#' @param my_palette a character vetor of length 17
#' @param mycex  numeric value which aim is to change the size of the legend
#' @param mypal  character vetor of length 17 
#' @param colid  character vectors with row and column labels to use; default NULL
#' @param showcol  boolean value used to hide or show the colnames 
#' @param showrow  boolean value used to hide or show the rownames
#' @param genename  data frame list corresponding to the gene names 
#' @param notplot  boolean calling or not the dev.off method
#'
#' @return  data frame with the cluster and the corresponding genes 
#' 
#' @export

plotHeatmaps=function(exprData,groups,workingPath=getwd(),fileType="png",cexcol=1.5,cexrow=1.5,
                      colOrder=NULL,labrow=F,na.color="black",scale="row", rowv =list4, ColvOrd = list3,
                      gpcol =list5 , gpcolr =list6 , distfunTRIX = list2,
                      RowSideColor=c("gray25","gray75") ,palette.col=NULL, 
                      margins=c(8,8),my_palette=colorRampPalette(c("green", "black", "red"))(n = 75)
                      ,mycex = 0.6,mypal=test,colid = NULL, showcol = T ,showrow =F,
                       notplot = F, geneSet= list7,genename = csvf, height = list8,rastering= myras ){
  
  
  #RowSideColor: color palette to be used for rowSide cluster colors
  # can also be gray.colors(k, start = 0.2, end = 0.9) to get k colors of gray scale

  if(is.null(showcol))
    showcol = F
  
  if(is.null(showrow))
    showrow = F
  
  if(showcol == T)
    colid = NA
  else
    colid = NULL
  
  if(showrow == F)
    rowIds = NA
  else
    rowIds = geneSet
  
  
  
  if(is.null(mypal))
    mypal = brewer.pal(8,"Dark2") %>%
      list(brewer.pal(10,"Paired")) %>%
      unlist()
  
  if(!is.null(palette.col)){
    palette(palette.col);
  }else  palette(mypal)
  
  cl=palette(mypal);

  ##-----------------------##
  ## plot Heatmap
  ##-----------------------##
  
  cat("\n -> Plotting HeatMap... \n")
  par("mar")
  
  par(mar=c(5,5,1,1.10))
  
  hmp02 = heatmap.2(exprData,na.rm=T,dendrogram="both",labRow = rowIds,labCol=colid,scale=scale, RowSideColors=gpcolr, ColSideColors=gpcol,key=T,
                    keysize=1, symkey=T, trace="none",density.info="density",distfun=distfunTRIX, hclustfun=hclustfun,cexCol=cexcol,
                    Colv=ColvOrd,Rowv=rowv,na.color=na.color,cexRow=cexrow,useRaster=rastering,margins=margins,layout(lmat =rbind(4:3,2:1),lhei = c(0.05,1), 
                                                                                                              lwid = c(0.1,1)),col=my_palette,key.par = list(cex=0.6))
  mtext(side=3,sort(levels(groups)),adj=1,padj=seq(0,by=1.4,length.out=length(levels(groups))),col=cl[(1:length(levels(groups)))],cex=mycex,line=-1)
  
  if(notplot)
    dev.off()
  
  cat("    Done \n")

  restoshiny = list(heatmtoclust(hmp02,exprData,genename,height= height),hmp02)


  return(restoshiny)
}










### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


DftoHighjson <- function(data, param) {
  
  foldEnrichment <- param$search 
  tempData <- (data[grep(param$search,colnames(data))])
  colnames(tempData) <- paste0("y",1)
  tempData$id <- data$Category 
  tempData$Term <- data$Term
  tempData$pvalue <- data$Benjamini
  tempData$FE <- data$Fold.Enrichment 
  tempData$Topgenes <-  data$Top
  tempData$percent <- data$percent

  
  unifiedData <- reshape(tempData, varying=paste0("y",1), 
                      direction="long", idvar="Top",sep="",timevar="x")
  
  
  unifiedData <- unifiedData[order(unifiedData$id),]
  
  
  unifiedData$x <- as.numeric(as.character(unifiedData$FE))
  unifiedData$y <- as.numeric(unifiedData$Topgenes)
  unifiedData$z <-  as.numeric(as.character(unifiedData$percent))
  
  
  unifiedData$GO  = sapply(unifiedData$Term, FUN= function(x) if(grepl("^mmu", x)) return(strsplit(as.character(x), ":")%>% unlist() %>% .[1]) 
                        else return(strsplit(as.character(x), "~")%>% unlist() %>% .[1])) 
  
  unifiedData$Term  = sapply(unifiedData$Term, FUN= function(x) if(grepl("^mmu", x)) return(strsplit(as.character(x), ":")%>% unlist() %>% .[2]) 
                        else return(strsplit(as.character(x), "~")%>% unlist() %>% .[2])) 
  
  unifiedData$Pvalue =  format(tempData$pvalue, digits = 3)


  
  return(highchartsConvert(unifiedData))
}


formatPoint <- function(index) {
  return( list( x=index$x, y=index$y,z=index$z, GO=index$GO, term=index$Term, pvalue = index$Pvalue))
}


formatCategory <- function(dframeCategory) {
  
  categoryTemplate <- list(name="",data={})
  categoryTemplate$name <- dframeCategory$id[[1]]
  categoryTemplate$visible <- F

  dataPoints <- dlply(dframeCategory,.(1:nrow(dframeCategory)),formatPoint)
  names(dataPoints) <- NULL 
  categoryTemplate$data <- dataPoints
  
  return(categoryTemplate)
}



highchartsConvert <- function(DftoHighjson) {
  
  json <- dlply (DftoHighjson, .(id), formatCategory)
  names(json) <- NULL   
  return(json)
  
}
### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


require(FactoMineR)
require(factoextra)


#' res.pca is a function that computed a PCA of non-normalized data with the FactoMineR package
#'
#' @param workingset a data frame corresponding to the WorkingSet
#' @param scale a boolean; by default this value is set to False non-normalized data
#'
#' @return \PCAres a data frame with PCA attributes
#' 
#' @export

res.pca <- function(workingset, scale = F) {
  
  myt = transpose(workingset)
  row.names(myt) = colnames(workingset)
  
  PCAres = PCA(myt,
               scale.unit = F,
               graph = F)
  
  return(PCAres)
}


#' eboulis is a function which aim is to display the eigenvalues of the data with the package factoextra
#'
#' @param PCAresa a data frame with PCA attributes
#'
#' @return \p a factoextra object
#' 
#' @export

eboulis <- function(PCAres){
  
  p <- fviz_eig(PCAres, addlabels=TRUE, hjust = -0.3, barfill="white", barcolor ="darkblue", linecolor ="red")
  p + theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank(),panel.border = element_blank(),
            panel.background = element_blank())
  p + labs(title = "Variances - PCA", x = "Principal Components", y = "% of variances")

  return(p)
}

#' PCAplot is a function that return a factoextra object of PCA type
#'
#' @param PCAres a data frame with PCA attributes
#' @param myax a numeric vector of length 2 specifying the dimensions to be plotted
#' @param elips a boolean value to add ellipse to the data distribution  for the different groups; default = False
#' @param rep a boolean value to avoid overlaps between the label points
#' @param mylevel a data frame corresponding to the pData
#' @param mylabsize a numeric value representing the police size to display for the different labels
#' @param dispelip a numeric value representing the ellipsoid dispersion
#' @param labeled a character to display labels and/or points
#' @param pal a color object from the RcolorBrewer package
#'
#' @return
#' 
#' @export

PCAplot <- function(PCAres, myax = c(1,2), elips = T , rep = T , mylevel = groups$Grp,  mylabsize = 4, dispelip = 0.8 , labeled = 'all', pal = brewer.pal(8, "Dark2")){
  
  
  p <- fviz_mca_ind(PCAres, label= labeled , habillage = mylevel, addEllipses= elips ,
                    ellipse.level= 0.8, repel = rep, axes = myax, pointsize = 2 , labelsize = mylabsize)

  
  return(p + scale_color_manual(values=pal))
}


# res.pca <- function(workingset, restab ,scale = F, variable = F) {
#   
#   myt = transpose(workingset)
#   row.names(myt) = colnames(workingset)
#   
#   if(variable)
#     colnames(myt) =  make.names(restab$GeneName, unique=TRUE)
#   
#   PCAres = PCA(myt,
#                scale.unit = F,
#                graph = F)
#   
#   return(PCAres)
# }



### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0

#Intersect, Union and Setdiff (https://stackoverflow.com/questions/23559371/how-to-get-the-list-of-items-in-venn-diagram-in-r)

#' Intersect is a function that takes a list as argument and return the identical elements between those lists 
#'
#' @param x list
#'
#' @return vector
#' @export
#' 
#' @examples
#' x <- c(sort(sample(1:20, 9)))
#' y <- c(sort(sample(3:23, 7)))
#' test = list()
#' test[[1]] = x
#' test[[2]] = y
#' Intersect(test) = 9,19

Intersect <- function (x) {  

  
  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    intersect(x[[1]], x[[2]])
  } else if (length(x) > 2){
    intersect(x[[1]], Intersect(x[-1]))
  }
}

#' Union is a function that takes a list as argument and return an union of those lists
#'
#' @param x list
#'
#' @return vector
#' @export
#' 
#' @examples
#' x <- c(sort(sample(1:20, 9)))
#' y <- c(sort(sample(3:23, 7)))
#' test = list()
#' test[[1]] = x
#' test[[2]] = y
#' Union(test) = 1  2  4  9 11 14 16 17 19  3  6 10 12 21

Union <- function (x) {  

  if (length(x) == 1) {
    unlist(x)
  } else if (length(x) == 2) {
    union(x[[1]], x[[2]])
  } else if (length(x) > 2) {
    union(x[[1]], Union(x[-1]))
  }
}

#' Setdiff is a function that remove the union of the y's from the common x's, x and y are lists of characters.
#'
#' @param x list of characters
#' @param y list of characters
#'
#' @return
#' @export
#' 

Setdiff <- function (x, y) {
  
  xx <- Intersect(x)
  yy <- Union(y)
  setdiff(xx, yy)
}



#' Vennlist is a function which aim is to return a list of signficant genes for a treshold defined by the user
#'
#' @param adj dataframe subset of the alltoptable
#' @param fc dataframe subset of the alltoptable
#' @param regulation character for up both or down
#' @param cutoffpval numeric value
#' @param cutofffc numeric value
#'
#' @return list.s
#' 
#' @export

Vennlist <- function(adj,fc, regulation, cutoffpval, cutofffc){ ## ajout de foreach parallel
  
  if(is.null(adj)) 
    return(NULL)
  
  reguser = ifelse(regulation == "up", T, F)
  reguserboth = ifelse(regulation == "both", T, F)
  lapply(1:ncol(adj), FUN = function(x){
    if(reguser && !reguserboth)
      return(as.character(which(adj[[x]] < cutoffpval & fc[[x]] > log2(cutofffc))))
    else if(!reguser && !reguserboth)
      return( as.character(which(adj[[x]] < cutoffpval & fc[[x]] < -log2(cutofffc))))
    else
      return(as.character(which(adj[[x]] < cutoffpval & abs(fc[[x]]) > log2(cutofffc))))
  })
}



#' Vennfinal is a function which aim is to return an object containing a venn diagram (old function with Venndiagram had been change with Jvenn)
#' 
#' @param myl a list of genes for the different contrasts
#' @param adj dataframe subset of the alltoptable
#' @param cex vector giving the size for each area label (length = 1/3/7/15 based on set-number)
#' @param cutoffpval numeric value
#' @param cutofffc numeric value
#' @param statimet character 
#' @param meandup character 
#' @param pval data frame of the alltoptable
#'
#' @return final draw on the current device of the venn diagram
#' @export
#' 

Vennfinal <- function(myl,adj, cex=1, cutoffpval, cutofffc, statimet, meandup = "probes", pval, mycol= ""){ 
  

  
  palette("default")
  if(meandup == "genes"){
    myl = lapply(seq(length(myl)), function(x){pval %>% select(GeneName, ProbeName) %>% filter( ProbeName %in% myl[[x]]) %>% 
        distinct( GeneName)}) %>%as.matrix()
    
    myl = lapply(1:length(myl),FUN = function(i) as.character(myl[[i]]$GeneName)) 
  }
  metuse = ifelse(statimet == "FDR","DEG BH ", "DEG RAW ")
  
  indexnull = which( sapply(myl ,length) == 0)
  if(length(indexnull)>0) comp = colnames(adj[,-c(indexnull)]) else  comp = colnames(adj)
  myl <- myl[sapply(myl, length) > 0]
  final = length(myl)-1
  if(mycol =="") mycolven= 2:(2+final) else mycolven = mycol
  totgenes =  sum(sapply(myl,length))
  totprobes=  totalvenn(myl, comp)
  mynumb = paste("total ", meandup,  ":", totgenes ,"and total ", meandup,  "crossings :",totprobes, collapse = "")

  futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger")
  mytresh = paste0(metuse, cutoffpval, " and FC " , cutofffc)
  
  
  if(length(myl)==2){
     if (length(myl[[2]])> length(myl[[1]]))
       mynames = rev(colnames(adj))
     else
       mynames = comp
  }
  else
    mynames = comp

  if(length(indexnull)>0){
    if(length(myl)==5){
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1, cat.just= list(c(0.6,1) , c(0,0) , c(0,0) , c(1,1) , c(1,0)),
                       category.names = mynames,fill = list(mycolven) , alpha = 0.3, sub=mynumb, cex=1, 
                       fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
    }
    else{
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,
                       category.names = mynames,fill = mycolven, alpha = 0.3, sub=mynumb, cex=1, 
                       fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
    }
  }
  else{
      if(length(myl)==5){
      g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,cat.just=  list(c(0.6,1) , c(0,0) , c(0,0) , c(1,1) , c(1,0)) ,
                     category.names = mynames,fill = mycolven  , alpha = 0.3, sub=mynumb, cex=1, 
                     fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop4
      }
      else{
        g = venn.diagram(x = myl, filename = NULL, scaled = F,lty =1,
                         category.names = mynames,fill = mycolven, alpha = 0.3, sub=mynumb, cex=1, 
                         fontface = 2, cat.fontface = 1, cat.cex = cex, na="stop")# na= stop
      }
  }
  
  final = grid.arrange(gTree(children=g), top="Venn Diagram", bottom= mytresh)
  
  
  
  return(final)
}




#' myventocsv is a function that create a csv file of the signficant genes for the different contrasts for a cutoff of 5%
#'
#' @param myven a list of genes for the different contrasts
#' @param adj a data frame
#'
#' @return
#' @export
#' 

myventocsv <- function(myven, adj){
  
  max.length <- max(sapply(myven, length))
  myven %>%
    lapply(function(v){ c(v, rep("", max.length-length(v)))}) %>%
    setNames(names(adj)) %>%
    as.data.frame()

}


mysetventocsv <- function(myven){

  max.length <- max(sapply(myven, length))
  myven %>%lapply(function(v){ c(v, rep("", max.length-length(v)))}) %>% as.data.frame()
}


#' totalvenn is a function which aim is to return the total element of each interesections for the venn diagram
#'
#' @param vennlist a list of genes for the different contrasts
#' @param adj dataframe subset of the alltoptable
#'
#' @return numeric value
#' @export
#' 

totalvenn <- function(vennlist,adj){

  names(vennlist) = adj
  elements <- 1:length(vennlist) %>% lapply(function(x)
      combn(names(vennlist), x, simplify = FALSE)) %>%
    unlist(recursive = F) %>% setNames(., sapply(., function(p)
      paste0(p, collapse = ""))) %>%
    lapply(function(i)Setdiff(vennlist[i], vennlist[setdiff(names(vennlist), i)])) %>% 
    .[sapply(., length) > 0]
  
  n.elements <- sapply(elements, length)

  
  return(sum(n.elements))
}

#' setvglobalvenn is a function which aim is to return lists of each probes for the different set of intersections 
#'
#' @param vennlist a list of genes for the different contrasts
#' @param adj dataframe subset of the alltoptable
#'
#' @return list of probes
#' @export

setvglobalvenn <- function(vennlist,adj, dll = F ){
  
  names(vennlist) = colnames(adj)
  elements <- 1:length(vennlist) %>% lapply(function(x)
    combn(names(vennlist), x, simplify = FALSE)) %>%
    unlist(recursive = F) %>% setNames(., sapply(., function(p){
      if(dll)
        paste0(p, collapse = "vs")
      else paste0(p, collapse = "") })) %>%
    lapply(function(i)
      Setdiff(vennlist[i], vennlist[setdiff(names(vennlist), i)])) %>% .[sapply(., length) > 0]
  
  return(elements)
}

#' rowtoprob is a function that return the probe names for the corresponding indexes
#'
#' @param myven a list of index for the different contrasts
#' @param pval dataframe of the alltoptable
#' @param adj dataframe subset of the alltoptable
#'
#' @return list
#' @export

rowtoprob <- function(myven,pval,adj) {
  
  names(myven) = colnames(adj)
  probesel = lapply(
    names(myven),
    FUN = function(x) 
      return( pval %>%filter( rownames(.)%in% myven[[x]]) %>%
                select(colnames(pval[1])) %>%unlist() %>%
                as.character())
  )
  
  genesel = lapply(
    names(myven),
    FUN = function(x) 
    return( pval %>%filter(rownames(.)%in% myven[[x]]) %>%
                select(GeneName) %>%unlist() %>%as.character())
  )

  return(list(probesel, genesel))
}

filterjvenn <- function(jvennlist, selcontjv, restab, idcol,  usersel, venngeneslist = NULL){

  ifelse (usersel == "genes",resfinal <- restab %>%
    filter(GeneName %in% jvennlist) %>%
    filter(.[[1]]  %in% venngeneslist) %>%
    select( GeneName, paste0("logFC_",  selcontjv)) %>%
    mutate_if(is.numeric, funs(format(., digits = 3))), resfinal <- restab %>%
    filter(.[[1]] %in% jvennlist) %>%
    select(idcol, GeneName, paste0("logFC_",  selcontjv)) %>%
    mutate_if(is.numeric, funs(format(., digits = 3))))
  
  return(resfinal)
}


getDegVennlfc <- function(selcontjv, filtgenes, restab, idcol, allcol, nonannot, usersel){
  
  reslist = list()
  
  resfinal <- restab %>%
    filter( .[[1]]  %in% filtgenes) %>%
    select(  GeneName, paste0("logFC_",  selcontjv)) %>%
    mutate_if(is.numeric, funs(format(., digits = 3)))
  
  if(nonannot){
    resfinal <- resfinal %>%  filter(., !grepl("^chr[A-z0-9]{1,}:|^ENSMUST|^LOC[0-9]{1,}|^[0-9]{4,}$|^A_[0-9]{2}_P|^NAP[0-9]{4,}|[0-9]{7,}",GeneName)) %>% as.data.frame()
  }
  
  reslist[[1]] <- resfinal
  
  if(!allcol)
    mycont = paste0("logFC_",selcontjv)
  else
    mycont = paste0("logFC_",selcontjv) ## allcont
  
  if(usersel == "genes"){
    
    options(datatable.optimize=1)
    
    for (i in mycont) {
      resfinal[[i]] = as.numeric(as.character(resfinal[[i]]))
    }
    
    reslist[[2]] <- resfinal %>% as.data.table() %>% .[,lapply(.SD,function(x) mean=round(mean(x), 3)),"GeneName"] %>% as.data.frame()  
  }
  
  
  return(reslist)
  
}


toJvenn <- function(myven, adj){
  
  
  names(myven) = colnames(adj)
  name   <- rep(names(myven), sapply(myven, FUN=function(x)return(length(x))))
  names(myven) <- NULL
  data <- sapply(myven, FUN=function(x)return(x)) %>% unlist()
  restab  <- data.frame(name,data)
  
  return(restab %>% group_by(name) %>% 
           summarise(data = list(as.character(data))) %>% 
           jsonlite::toJSON())
  
}


#' topngenes is a function to plot the top n genes for a defined intersection between comparison.s
#'
#' @param dfinter list of intersection.s
#' @param mycont character Vector 
#' @param inputtop numeric value
#' @param meandup character
#'
#' @return ggplot2 barplot
#' @export
#'
#'

topngenes <- function(dfinter, mycont, inputtop, meandup = "probes", mean = F )  {
  
  
  if(meandup == "probes")
    dfinter$GeneName = make.names(dfinter$GeneName, unique = T)
  
  if(mean == T){
    
    logval <- "logFC_" %>%
      grepl(colnames(dfinter))%>%
      which(.==T)
    
    for (i in mycont) {
      dfinter[[i]] = as.numeric(as.character(dfinter[[i]]))
    }
    
    dfinter <- dfinter[,-1] %>% as.data.table() %>% .[,lapply(.SD,mean),"GeneName"] 
    dfinter = as.data.frame(dfinter)
    
    }
    

  mycont = gsub("-"," vs logFC_" ,mycont)
  colnames(dfinter)= lapply(colnames(dfinter),function(x){
    
    if(grepl("-",x))
      x = gsub("-"," vs logFC_" , x)
    return(x)})
  
  
  reshp <-melt(dfinter[1:inputtop, ],
  id.vars = "GeneName",measure.vars = c (mycont),
  variable.name = "Comparisons",value.name = "logFC") %>% na.omit()
  reshp <- droplevels(reshp)
  reshp$GeneName <-factor(reshp$GeneName, levels = unique(as.character(reshp$GeneName)))

  p <- ggplot(reshp, aes(
    x = GeneName,
    y = as.numeric(as.character(formatC(as.double(logFC), digits = 1, format = "f"))),
    fill = Comparisons
  )) +
    geom_bar(stat = "identity", position = "dodge") +
    
    
    scale_fill_manual(values = c("red","blue",'purple',"green","black")) + 
    
    xlab("Gene Names") + ylab("Log Fold-Change") +

    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "white"),
      plot.title = element_text(size = 20, hjust = 0.5),
      plot.caption = element_text(size = 10, hjust = 0.5),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10) ,
      axis.text.x = element_text(
        size = 11,
        colour = "#808080",
        angle = 80,
        hjust = 1
      ),
      axis.text.y = element_text(size = 8, colour = "#808080"),
      legend.position="top"
    ) 
  
  print(p)
  return( p)
  
}

