### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
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
