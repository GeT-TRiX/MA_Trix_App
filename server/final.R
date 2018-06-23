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

#' Reactive function that return a character color
#'
#' @param choix_col1 character color for the lowest values
#'
#' @return \choix_col1 a reactive value
#'

choix_col1 <- reactive({
  return(input$col1)
})

#' Reactive function that return a character color
#'
#' @param choix_col3 character color for the lowest values
#'
#' @return \choix_col3 a reactive value   
#' 

choix_col3 <- reactive({
  return(input$col3)
})


#' Reactive function that return a character color
#'
#' @param choix_col1 character color for the lowest values 
#' @param choix_col2 character color for the highest values
#'
#' @return \my_intermediata a reactive character intermediate color between the lowest and the highest values
#'


my_intermediate <- reactive({
  
  if (choix_col1() == "green" & choix_col3() == "red")
    inter = "black"
  
  else if (choix_col1() == "orange" & choix_col3() == "red")
    inter = "yellow"
  
  else if (choix_col1() == "blue" & choix_col3() == "red")
    inter = "white"
  
  else if (choix_col1() == "blue" & choix_col3() == "yellow")
    inter = "black"
  
  else
    inter= NULL
  
  return(inter)
  
})##################################
######## Modify buttons          #  
##################################


#' ReactiveValues function 
#'
#' @param clicked bool set to FALSE
#'
#' @return \bool TRUE or FALSE
#'
global <- reactiveValues(clicked = FALSE)

observe({
  if(length(input$heatm)){ # giving a length once it's clicked
    if(input$heatm) global$clicked <- TRUE
  }
})


output$button <-  renderUI({ # if button is clicked changed his style.css
  if(!is.null(input$heatm) & global$clicked){
    shiny::actionButton("heatm", "Update Heatmap", icon = icon("repeat"), style = "color: #fff; background-color: #b77033; border-color: #b77033")
  }
  else{
    shiny::actionButton("heatm", "Print Heatmap", style = "color: #fff; background-color: #337ab7; border-color: #337ab7")
  }

})#################################
######## Select the comparisons #
#################################

# Render in the UI.R the levels for the pData Group 
observe({
  
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)  

output$testout <- renderUI(
  checkboxGroupInput(
    inputId = "test" ,
    label =  "Choose your comparison",
    choices =  colnames(adjusted()[[1]][,-1]),
    #,selected = colnames(adjusted()[, -1])
    inline = groupinline
  )
)
})

#Select all the contrasts

observeEvent(input$allTests, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(
    session,
    "test",
    label = "Choose your comparison",
    choices = colnames(adjusted()[[1]][,-1]),
    selected = colnames(adjusted()[[1]][,-1]),
    inline = groupinline
  )
})

#Unselect all the contrasts
observeEvent(input$noTests, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(session,
                           "test",
                           label = "Choose your comparison",
                           choices = colnames(adjusted()[[1]][, -1]),
                           inline= groupinline
                           )
})

#' choix_test is a reactive function in the aim of selecting different comparison 
#'
#'
#' @return \input`$test` a reactive value of type character for the different comparisons selected 
#'


#choix_test <- eventReactive(input$heatm, {
choix_test <- reactive({
  return(input$test)
})
#, ignoreNULL = F)


# choix_test <- reactive({
#   return(input$test)
# })

  
# output$test <- renderText({
#   my_final <<- paste(choix_test(),as.character(),  sep=",") 
# 
# })




#################################
######## Select the groups      #
#################################


# Render in the UI.R the levels for the pData Group


observe({
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  
  output$individusel <- renderUI(
    checkboxGroupInput(
      inputId = "indiv" ,
      label =  "Choose your group to visualize",
      # choices =  colnames(csvf()[[1]][,-1]),
      # selected = colnames(csvf()[[1]][,-1])
      choices =  levels(csvf()[[2]]$Grp),
      selected = levels(csvf()[[2]]$Grp),
      inline = groupinline
    )
  )
  
})

# Select all groups

  
observeEvent(input$allIndividus, {
    
    groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
    
    updateCheckboxGroupInput(
      session,
      "indiv",
      label = "Choose your group to visualize",
      #choices = colnames(csvf()[[1]][,-1]),
      #selected = colnames(csvf()[[1]][,-1])
      choices =  levels(csvf()[[2]]$Grp),
      selected = levels(csvf()[[2]]$Grp),
      inline = groupinline
    )
  })


  # Unselect all groups
  observeEvent(input$noIndividus, {
    groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
    updateCheckboxGroupInput(session,
                             "indiv",
                             label = "Choose your group to visualize",
                             #choices = colnames(csvf()[[1]][,-1]))
                             choices =  levels(csvf()[[2]]$Grp),
                             inline = groupinline )
    
  })

#' choix_grp is a reactive function which aim is to select/unselect groups
#'
#' @param input'$indiv' specific of the individuals data frame
#'
#' @return a reactive  character value for the different individuals selected
#'

choix_grp <- reactive({
  req(input$indiv)
  
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  
  return(input$indiv)
})



#' list_ind is a reactive function in the aim of having selected groups in a list
#'
#' @param input specific of the individuals data frame
#'
#' @return a reactive list for the different individuals selected
#'


list_ind <- reactive({
  return(list(input$indiv))
})



#' new_group is an eventreactive function that select specific groups in the data frame
#'
#' @param csvf Data frame corresponding to the pData table
#'
#' @return \newgroup an eventreactive factor with the corresponding groups selected
#'

#new_group <- eventReactive(input$heatm, {
new_group <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(), ]
})
#}, ignoreNULL = F)





#' new_data is a reactive function that aim is to select specific individuals in the data frame
#'
#' @param \csvf Data frame corresponding to the Workingset
#'
#' @return \newdata a reactive data frame with specific columns depending on the user's choices
#'


new_data <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  #subset(csvf()[[1]],select = choix_individus())
  select(csvf()[[1]], as.character(factor(new_group()$X)))
})
#########################################
######## Colors for the  PCA groups     #
#########################################

#' colspca is a reactive function which aim is to dynamically create widgets in function of the number of groups
#'
#' @param brewer.pal a local list defined in the RcolorBrewer package
#' @param mycolgrppca a dataframe representing the selected groups
#'
#' @return \colspca a reactive number of widget-s
#'

colspca <- reactive({
    
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


colorfluidpca <- reactive({
  
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
  #colspca()
  colorfluidpca()
})



#' colorspca is a reactive function which aim is to create as many variables as groups
#'
#' @param mycolgrppca  a reactive data frame 
#'
#' @return \colorspca a reactive  list containing the different variable names
#'


colorspca <- reactive({
  lapply(seq_along(unique(mycolgrppca())), function(i) {
    input[[paste("colpca", i, sep = "_")]]
  })
})

#' mycolgrppca is a reactive function which aim is to display the total number of groups
#'
#' @param csvf a datafrale
#'
#' @return \mycolgrp a reactive reorder dataframe
#'

mycolgrppca <- reactive  ({
  req(csvf())
  mygrpcol <- new_grouppca()$Grp %>%
    sort() %>%
    unique() %>%
    droplevels()
  
  return(mygrpcol)
})###############################
######## Adding mean by group #
###############################

#' Reactive function which aim is to return the user's input
#'
#' @param input$meangrp character depending on the user's choice to compute the mean for each different groups in the heatmap
#'
#' @return a reactive value of type string/char 
#'


output$value <- renderText({
  input$meangrp
})

#' mean_grp is a reactive function which aim is to return the output user in order to show this input in the UI
#'
#' @param ouput$value character depending on the user's choice
#'
#' @return \mean_grp a reactive value of type character depending on the user's input
#'

mean_grp <- reactive({
  return(output$value)
})


###############################
######## Load the csv files   #
###############################

showmark <- T # Boolean uses to hide or show the mardkwon serving to load data


#' Reactive function returned to the tab1.R 
#'
#' @return \showmark a reactive value of type boolean corresponding to the loading status by default it is set to True
#'

output$boolmark <- reactive({
  showmark
})

observe({
  print(showmark)
})


  
outputOptions(output,"boolmark",suspendWhenHidden=F) 

#' Reactive function in the aim of loading csv files
#'
#' @param inFile loaded files
#'
#' @return \csvf a reactive value of type list containing three data frames toptable and workingset and the pData 
#'


csvf <- reactive({

  
  inFile <- input$file
  if (is.null(inFile)) {
    createAlert(
      session,
      "alert",
      style = "info",
      "entryalert",
      title = "First Step",
      content = "You need to import 3 csv files in the browser widget",
      dismiss = FALSE
      
    )
  
  #Sys.sleep(1.5)
  closeAlert(session, "entryalert")
  return(NULL)
  }
  
  req(input$file)
  print(inFile)
  
  
  
  data <- as.list(inFile$datapath)
  csvtest = list()
  name = inFile$datapath
  iscsv = grep(pattern = '.csv$', name, value = T)
  
  if (length(iscsv) == 0) {
    createAlert(
      session,
      "alert",
      "exampleAlert",
      style = "danger",
      title = "Oops Error",
      content = "Are you sure you're importing csv files ?",
      append = FALSE
    )
    #return(NULL)
    return()
  }
  
  else{
    if (length(data) > 3)
    {
      createAlert(
        session,
        "alert",
        "exampleAlert",
        style = "danger",
        title = "Oops Error",
        content = "Are you sure it's the good number of files? you  have imported more than 3 files,
        you need to import 3 csv files
        Tips: Use ctrl+left click then choose your files ",
        append = FALSE
      )
      
      return (NULL)
    }
    
    else if (length(data) < 3) {
      createAlert(
        session,
        "alert",
        "exampleAlert",
        style = "danger",
        title = "Oops Error",
        content = "Are you sure it's the good number of files? you have imported less than
        3 files, you need to import 3 csv files
        Tips: Use ctrl+left click then choose your files ",
        append = FALSE
        
      )
      
      return (NULL)
    }
    
    else{
      for (i in 1:length(data)) {
        for (elem in input$file[[i, 'datapath']]) {
          cat("loading file number" , i, "\n")
        }
        csvtest[i] = elem
      }
    }
    
    #csv <- lapply(csvtest, read.csv2, check.names = F) # benchmark read.csv wrapper
    
    csv <- lapply(
      csvtest,
      
      #' apply the fread method for each element in the csvtest list
      #'
      #' @return \csv a data frame object
      #'
      FUN = function (x)
        
        # read.table( # benchmark read.table
        #   x,
        #   sep = ";" ,
        #   dec = ",",
        #   header = T,
        #   check.names = F # good col names
        # )
        
        fread(
          x,
          data.table = F,
          check.names = F,
          header = T,
          sep = ";",
          dec = ","
        ) #benchmark fread memory speed
    )
    
    csvord = list()
    print("ok")
    for (i in 1:length(csv)) {
      if (colnames(csv[[i]][2]) == "Grp") {
        csvord[[2]] = csv[[i]]
        
      }
      else if (any(grepl("adj.P.Val" , colnames(csv[[i]]))))
      {
        csvord[[3]] = csv[[i]]
        
      }
      else
        csvord[[1]] = csv[[i]]
    }
    
    csvord[[2]] = chartofa(csvord[[2]]) # transform dataframe containing characters to factors
    row.names(csvord[[1]]) = csvord[[1]][, 1]
    colnames(csvord[[3]])[1] = "X"
    colnames(csvord[[2]])[1] = "X"
    
  }
  
  observe({showmark <<-F
  print(showmark)
  }) # modify and lock the bool value to false
  
  output$boolmark <- reactive({
    showmark
  })
  
  
  
  #' Reactive function returned to the tab1.R 
  #'
  #' @return \showmark a reactive value of type boolean set to False
  #'
  
  
  
  createAlert(
    session,
    "alert",
    "succeeded",
    style = "success",
    title = "Sucess",
    content = " Your files have been loaded, you can choose your data now",
    append = FALSE
    
  )

  Sys.sleep(1)
  closeAlert(session, "succeeded")
  

  return (csvord)
})#########################################
######## Cut heatmap Part               #
#########################################


#' p is a reactive function that return an heatmap gplots object
#'
#' @param heatmapfinal a function
#'
#' @return \p an object
#'

p <- eventReactive(input$updateheatm,{
  isolate(hmobj$obj)
})

#' PCAres is a reactive function that change the rownames values
#'
#' @param csvf a data frame 
#'
#' @return \rownamtoX a data frame with the row.names index corresponding to the first column index
#'

rownamtoX <- reactive({
  mycsv = csvf()[[3]]
  row.names(mycsv) = mycsv$X
  
  return(rownamtoX)
})

#' cutfinal is a reactive function that ....
#'
#' @param p an heatmap object
#' @param input$cutheight a numeric value to cut the dendogram 
#' @param new_data a data frame with specific columns depending on the user's choices
#' @param rownamtoX a data frame
#' @param groups a data frame of the choosen groups
#' @param input$cutcluster an heatmap object
#' @param input$cutinfo a character to select the plot to display heatmap, boxplot or stripchart
#'
#' @return \cutfinal a ggplot object or heatmapply object
#'

cutfinal <- reactive({
    req(hmobj$obj)
  
    pdf(NULL)
    cutHeatmaps(
      hmobj$obj,
      height =  hmsize$cut,
      exprData = data.matrix(new_data()),
      groups = droplevels(new_group()$Grp),
      DEGres =  rownamtoX()[, -1],
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
  if (is.null(d))
    "Hover on a point!"
  else {
    
    round(sort(d),digits=2)
  }
})

observe({ 
  req(hmobj$obj)
  if (req(input$cutinfo) == "Heatmap") {
    output$cutheatmap <- renderPlotly({ # Plot/Render an object of class plotly
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
    #plot(PCAplot())
    dev.off()
  })


###############################
######## Summarise data       #
###############################

#' reactive function that return the indexes for the signficant genes
#'
#' @param csvf data frame
#' @param intput$fc numeric pvalue

#' @return \datasummary a reactive data frame with the indexes corresponding to the sigificant genes for 5 Fold change 1.2,2,4,6,10
#'

data_summary <- reactive({
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  myfinalfc(csvf()[[3]], input$pval1, input$method)
})#' Reactive function that return a list containing multiple data frames
#' with the adj.P.val, logFC and P.value selected for the corresponding groups
#'
#' @param csvf Data frame corresponding to the Alltoptable
#'
#' @return \adjusted a reactive list of data frames 
#'

adjusted <- reactive({
  
  df <- csvf()
  if (is.null(df))
    return(NULL)
  
  myrpl = c("^adj.P.Val_","^logFC_","^P.value_")
  grepdf = c("X|^adj.P.Val","X|^logFC","X|^P.value")
  
  adj = csvf()[[3]][, grep("^X|^adj.P.Val",
                           names(csvf()[[3]]),
                           value = TRUE)]
  
  logfc = csvf()[[3]][, grep("^X|^logFC",
                             names(csvf()[[3]]),
                             value = TRUE)]
  
  pval = csvf()[[3]][, grep("^X|^P.value",
                            names(csvf()[[3]]),
                            value = TRUE)]
  
  
  mygrep = list(adj,logfc,pval)
  
  
  for(i in 1:length(mygrep))
    names(mygrep[[i]]) = gsub(
      pattern = myrpl[i],
      replacement = "",
      x = names(mygrep[[i]]),
      perl = T
    )
  
  return(mygrep)
  
})#########################################
######## Colors for the  groups         #
#########################################

#' mycolgrp is a reactive function which aim is to display the number of groups selected 
#'
#' @param new_group a derivate data frame of the pData
#'
#' @return \mycolgrp a reactive data frame 
#'

mycolgrp <- reactive  ({
  mygrpcol <- new_group()$Grp %>%
    sort() %>%
    unique() %>%
    droplevels()
  
  
  return(mygrpcol)
})

#' cols is a reactive function which aim is to dynamically create widgets in function of the number of groups
#'
#' @param palette a local list defined in the environment
#' @param mycolgrp a dataframe representing the selected groups
#' @mypaletA a list which contaings the colors values corresponding to the different groups
#'
#' @return \cols a reactive number of widget-s
#'

cols <- reactive({

  if (is.null(mypal()) )
    lapply(seq_along(mycolgrp()), function(i) {
      
      # fluidRow(
      #   column(6,
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
#' @return \mypaletA a reactive list of colors attributed by ranking order to the different groups
#'

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
#' @return \mypal a reactive  that unlist the colors attributed to the different groups
#'

mypal <- reactive({
  unlist(colors())
})



colorfluidhm <- reactive({

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




# output$myPanel1 <- renderUI({ # display the colourInput in the UI
#   cols()[1:2]
# })
# 
# output$myPanel2 <- renderUI({ # display the colourInput in the UI
#   cols()[3:4]
# })

#' colors is a reactive function which aim is to create as many variables as groups
#'
#' @param mycolgrp  a reactive data frame 
#'
#' @return \colors a reactive  list containing the different variable names
#'

colors <- reactive({
  lapply(seq_along(mycolgrp()), function(i) {
    input[[paste("col", i, sep = "_")]]
  })
})###############################
########heatmap function & co #
###############################

boolhm <- F


#hmneed <- T # Boolean uses to hide or show the mardkwon serving to load data


output$heatmbool <- reactive({
  boolhm
})


outputOptions(output, "heatmbool", suspendWhenHidden = F)

observe({
  req(csvf(),length(choix_test()) >0,input$reactheat == T| global$clicked)
  
  observe({boolhm <<-T}) # modify and lock the bool value to false
  
  output$heatmbool <- reactive({
    boolhm
  })
  
})


#' rowname is a reactive function which aim is to hide or show the rownames
#'
#' @param input$rowname  a boolean radio button input
#'
#' @return \rowname a reactive boolean value
#'

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
#' @return \colname a reactive  reactive boolean value
#'

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
  #' @param heatmapobj[[1]] a data frame with all the individuals selected
  #' @param formated  a data frame with the indexes corresponding to the sigificant genes
  #' @param new_group  a data frame with the corresponding groups 
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
        choix_col1(), my_intermediate(), choix_col3()
      ))(n = 75))
    
    
    plotHeatmaps(
      isolate(hmbis()[[1]]),
      geneSet =  isolate(hmbis()[[7]]),
      droplevels(new_group()$Grp),
      workingPath = wd_path,
      my_palette = (colorRampPalette(
        c(choix_col1(), my_intermediate(), choix_col3())
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
  
  
  
  output$warningsheat <- renderPrint({#renderPlot({
    validate(need(
      csvf(),
      'You need to import data to visualize to plot the Heatmap' ) %next% 
      need(length(choix_test()) >0, 'You need to select a contrast(s), then click on the heatmap button down below the heatmap settings')

    )
  })
  
  
  heatid <- input$side
  if (grepl("Heatmap", heatid)) {
    if (input$reactheat == T)
      source(file.path("server", "plotreact.R"), local = TRUE)$value #
    else
      source(file.path("server", "plotreact2.R"), local = TRUE)$value #
    
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
    
    if (!is.null(formated()[[1]]))
      withProgress(message = 'Saving heatmap:',
                   value = 0, {
                     n <- NROW(formated()[[1]])
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
    
    if (input$method2 == "FDR")
      met = "adj.P.Val_"
    else
      met = "P.value_"
    
   
    
    mycont = paste0(met, choix_test())
    ordered = csvf()[[3]] %>% filter(ProbeName %in% hmobj$hm$ProbeName)  %>%
      select(ProbeName,  mycont) %>%
      full_join(hmobj$hm[,-1], ., by = "ProbeName") %>%
      select(ProbeName, GeneName, mycont, cluster) %>%
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
        return(length(which(mydfhmgen$cluster ==x))))) %>% as.data.frame()%>%
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




# cutedhm <- reactive({
#   req(hm())
#   cut02 = cut(hm()$rowDendrogram, h = input$cutheatm)
#   return(cut02)
# })


# cluster <-  reactive({

#cut02 = cut(hm()$rowDendrogram, h = input$cutheatm)
#   dfclust = heatmtoclust( hm(), formated(), data.matrix(new_data()), csvf()[[3]], input$cutheatm)
#
#   return(dfclust)
# })###############################
######## Hide buttons         #
###############################


shinyjs::onclick("toggleAdvanced",
                 shinyjs::toggle(id = "advanced", anim = TRUE)) ## hide and show event


shinyjs::onclick("toggleAdvancedcolors",
                 shinyjs::toggle(id = "advancedcol", anim = TRUE))

# shinyjs::onclick("toggleAdvancedgo",
#                  shinyjs::toggle(id = "advancedgo", anim = TRUE))#' a reactive function that return the indexes for the signficant genes
#'
#' @param user_group list of three data frame with rows selected according to the contrasts selected
#' @param intput$fc numeric FC selected
#' @param input$method2 character method, default = BH
#' @param input$pval numeric pvalue
#' @param input$maxgen numeric maxgen, default = NULL
#'
#' @return \formated a reactive data frame with the indexes corresponding to the sigificant genes
#'


formated <- reactive({
  
  req(user_group())
  
  #print(intput$method2)
  
  df <- csvf()
  if (is.null(df))
    return(NULL)
  
  else
    treated = decTestTRiX(
      user_group()[[1]],
      user_group()[[2]],
      user_group()[[3]],
      DEGcutoff = input$pval,
      FC = input$fc,
      cutoff_meth = input$method2,
      maxDE = input$maxgen
      
    )
  

  
  return(treated)
  
})




observe({
  req(input$tabset25)
  if (grepl("hmpan", input$tabset25)) {
    updateTabsetPanel(session, "mainhmtabset",
                      selected = "hmmainpan")
  }
  else if (grepl("cutpan", input$tabset25)) {
    updateTabsetPanel(session, "mainhmtabset",
                      selected = "cuthmmainpan")
  }
})

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
######## citation packages              #
#########################################

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
    inline= groupinline
  )
})

# Unselect all groups
observeEvent(input$noIndividuspca, {
  groupinline = ifelse(length(levels(csvf()[[2]]$Grp)) > 6, T, F)
  updateCheckboxGroupInput(session,
                           "indivpca",
                           label = "Choose your group to visualize",
                           choices =  levels(csvf()[[2]]$Grp),
                           inline = groupinline
                           )
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
        width =1200,
        height = 1200,
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
  
})


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
  
  plot(PCAplot()+ theme_minimal())
  
})


output$savepca <- downloadHandler(
  
  filename <- function() {
    paste0(basename(file_path_sans_ext(projectname())), '_pca.',input$formpca, sep='')    
  },
  content <- function(file) {
    if (input$formpca == "pdf")
      
      pdf(file,
          width = 12,
          height = 12,
          pointsize = 12)
    
  
    else if (input$formpca == "png")

    png(file,
        width =1200,
        height = 1200,
        units = "px",
        pointsize= 12,
        res=100
    )
    else
      eps(
        file,
        width = 12,
        height = 12,
        pointsize = 12
      )
    
    
    plot(PCAplot())
    dev.off()
  })




###############################
########  PCA function        #
###############################


#' PCAplot is a function that return a factoextra object of PCA type
#'
#'@param brew.pal a color object from the RcolorBrewer package
#'@param PCAres a data frame with PCA attributes
#'@param label a boolean value depending of the user's choice to display or not the labels
#'@param habillage a data frame corresponding to the pData
#'@param addEllipseda a boolean value to add ellipse to the data distribution  for the different groups
#'@param ellipse.level a numeric value set to 0.8
#'@param repel a boolean value to avoid overlaps between the label points
#'@param axes a numeric vector of length 2 specifying the dimensions to be plotted
#'@param labelsize a numeric value representing the police size to display for the different labels
#'@param pointsize a numeric value representing the diameter of each points displayed in the graph
#'
#'@return \p a factoextra object
#' 


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
    habillage = droplevels(new_grouppca()$Grp),
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





###############################
######## not Reactive side    #
###############################


shinyjs::enable("heatm")



#' hmbis is an event reactive function that pre-computed hierarchical clustering on microarray data
#'
#' @param new_data a data frame with all the individuals selected
#' @param formated  a data frame with the indexes corresponding to the sigificant genes
#' @param new_group  a data frame with the corresponding groups
#' @param workingPath the current user's repository
#' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
#' @param Rowdistfun a function used to compute the distance for the rows
#' @param Coldistfun a function used to compute the distance for the columns
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#'
#' @return  a list of objects which aim is to being passed as argument in the plotHeatmaps function
#'
#' @export
#'

#hmbis <- eventReactive(input$heatm,{
hmbis <- reactive({
  withProgress(message = 'Performing the hierarchical clustering:', # Add sliderbar when loading heatmap
               value = 0,
               {
                 n <- NROW(formated()[[1]]) #number of row in the formated dataframe
                 for (i in 1:n) {
                   incProgress(1 / n, detail = "Please wait...")
                 }
                 
                 truncatedhat(
                   data.matrix(new_data()),
                   formated()[[1]],
                   droplevels(new_group()$Grp),
                   workingPath = wd_path,
                   k = input$clusters,
                   mypal = unlist(colors()),
                   Rowdistfun = input$dist ,
                   Coldistfun = input$dist,
                   meanGrp = input$meangrp,
                   genename =  csvf()[[3]]
                 )
                 
               })
})


observeEvent(input$heatm, {
  if (is.null(my_intermediate())) {
    heatmapfinal(isplot = F)
    shinyjs::alert("your choice color are not fit to be together!!")
    
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
                       n <- NROW(formated()[[1]]) #number of row in the formated dataframe
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
  


#' #' heatmapfinal is an isolate function that only react to a user's click on the heatmap button
#' #'
#' #' @param heatmapobj[[1]] a data frame with all the individuals selected
#' #' @param formated  a data frame with the indexes corresponding to the sigificant genes
#' #' @param new_group  a data frame with the corresponding groups
#' #' @param workingPath the current user's repository
#' #' @param my_palette a vector of colors
#' #' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
#' #' @param Rowdistfun a character value set by the user to defined the method to calculate the dendogram matrix distance for the genes input$dist
#' #' @param Coldistfun a character value set by the user to defined the method to calculate the dendogram matrix distance for the contrasts input$dist
#' #' @param mycex a numeric value which aim is to change the size of the legend in the heatmap defined by the user input$legsize
#' #' @param cexrow  a numeric value to change the size of the police legend for the rows input$rowsize
#' #' @param cexcol a numeric value to change the size of the police legend for the columns input$colsize
#' #' @param meanGrp a boolean value to compute or not the mean of each contrasts in the heatmap input$meangrp
#' #' @param mypal a list of values
#' #' @param showcol a boolean value used to hide or show the colnames input$colname
#' #' @param showrow a boolean value used to hide or show the rownames input$rowname
#' #' @param genename a data frame
#' #' @param notplot a boolean value for applying dev.off or not on the heatmap
#' #' @param rowv  dendogram object
#' #' @param ColOrd  positive numbers, used as cex.axis in for the row or column axis labeling
#' #' @param gpcol  matrix with colors associated to each groups
#' #' @param gpcolr  matrix with gray color depending on the clusters
#' #' @param distfunTRIX function that computes whether euclidian or pearson for Hierarchical Clustering
#' #'
#' #' @return  a data frame with the cluster and the corresponding genes
#' #'
#' #' @export
#' #'
#'
#' shinyjs::enable("heatm")
#'
#'
#'
#' heatmapfinal <- function(isplot  = T) {
#'   if (is.null(my_intermediate()))
#'     mypal = (colorRampPalette(c("green", "black", "red"))(n = 75))
#'   else
#'     mypal = isolate((colorRampPalette(c(
#'       choix_col1(), my_intermediate(), choix_col3()
#'     ))(n = 75)))
#'
#'   plotHeatmaps(
#'     heatmapobj[[1]],
#'     geneSet =  hmbis()[[7]],
#'     droplevels(new_group()$Grp),
#'     workingPath = wd_path,
#'     my_palette = colorRampPalette(c(
#'       choix_col1(), my_intermediate(), choix_col3()
#'     ))(n = 75),
#'     mycex = input$legsize ,
#'     cexrow = input$rowsize ,
#'     cexcol = input$colsize ,
#'     mypal =  unlist(colors()),
#'     showcol = colname(),
#'     showrow = rowname(),
#'     genename =  csvf()[[3]],
#'     notplot = isplot,
#'     rowv = heatmapobj[[4]],
#'     ColvOrd = heatmapobj[[3]],
#'     gpcol = heatmapobj[[5]],
#'     gpcolr = heatmapobj[[6]],
#'     distfunTRIX = heatmapobj[[2]],
#'     height = heatmapobj[[8]]
#'   )
#' }
#'
#'
#' # heatmapfinal <- function(isplot  = F) {
#' #   if(is.null(my_intermediate()))
#' #     mypal = (colorRampPalette(c(
#' #       "green", "black", "red"))(n = 75))
#' #   else
#' #     mypal= (colorRampPalette(c(
#' #       choix_col1(), my_intermediate(), choix_col3()))(n = 75))
#' #
#' #
#' #   plotHeatmaps(
#' #     hmbis()[[1]],
#' #     geneSet =  hmbis()[[7]],
#' #     droplevels(new_group()$Grp),
#' #     workingPath = wd_path,
#' #     my_palette = (colorRampPalette(c(
#' #       choix_col1(), my_intermediate(), choix_col3()))(n = 75)),#mypal,
#' #     mycex = input$legsize ,
#' #     cexrow = input$rowsize ,
#' #     cexcol = input$colsize ,
#' #     mypal =  unlist(colors()),
#' #     showcol = colname(),
#' #     showrow = rowname(),
#' #     genename =  csvf()[[3]],
#' #     notplot = isplot,
#' #     rowv = hmbis()[[4]],
#' #     ColvOrd = hmbis()[[3]],
#' #     gpcol = hmbis()[[5]],
#' #     gpcolr = hmbis()[[6]],
#' #     distfunTRIX = hmbis()[[2]],
#' #     height = hmbis()[[8]]
#' #   )
#' #
#' # }
#'
#'
#'
#' #' hmbis is an event reactive function that pre-computed hierarchical clustering on microarray data
#' #'
#' #' @param new_data a data frame with all the individuals selected
#' #' @param formated  a data frame with the indexes corresponding to the sigificant genes
#' #' @param new_group  a data frame with the corresponding groups
#' #' @param workingPath the current user's repository
#' #' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
#' #' @param Rowdistfun a function used to compute the distance for the rows
#' #' @param Coldistfun a function used to compute the distance for the columns
#' #' @param meanGrp a boolean value to computes the mean for each groups; default = F
#' #'
#' #' @return  a list of objects which aim is to being passed as argument in the plotHeatmaps function
#' #'
#' #' @export
#' #'
#'
#' hmbis <- eventReactive(input$heatm, {
#'   withProgress(message = 'Performing the hierarchical clustering:', # Add sliderbar when loading heatmap
#'                        value = 0,
#'                        {
#'                          #n <- NROW(formated()[[1]]) #number of row in the formated dataframe
#'                          n = 500
#'                          for (i in 1:n) {
#'                            incProgress(1 / n, detail = "Please wait...")
#'                          }
#'
#'                          truncatedhat(
#'                            data.matrix(new_data()),
#'                            formated()[[1]],
#'                            droplevels(new_group()$Grp),
#'                            workingPath = wd_path,
#'                            k = input$clusters,
#'                            mypal = unlist(colors()),
#'                            Rowdistfun = input$dist ,
#'                            Coldistfun = input$dist,
#'                            meanGrp = input$meangrp,
#'                            genename =  csvf()[[3]]
#'                          )
#'                        })
#'
#'   #dev.off()
#'   # isolate in order to avoid that reactive values update the heatmap)
#' })
#'
#' heatmapobj <<- hmbis() # a static variable
#'
#'
#' observe(if (is.null(my_intermediate())) {
#'   isolate(heatmapfinal(isplot = F))
#'   shinyjs::alert("your choice color are not fit to be together!!")
#' }
#' else
#'   output$distPlot <- renderPlot({
#'
#'     validate(
#'       need(csvf(), 'You need to import data to visualize this plot!'))
#'
#'     isolate({
#'       if (!is.null(formated()[[2]]))
#'         withProgress(message = 'Plotting heatmap:', # Add sliderbar when loading heatmap
#'                      value = 0,
#'                      {
#'
#'                        #n <- NROW(formated()[[1]]) #number of row in the formated dataframe
#'                        n <- 500
#'                        for (i in 1:n) {
#'                          incProgress(1 / n, detail = "Please wait...")
#'                        }
#'
#'
#'
#'
#'                        observe({boolhm <<-T})
#'
#'                        output$heatmbool <- reactive({
#'                          boolhm
#'                        })
#'
#'                        #hmbis()
#'
#'                        hmobj$hm = heatmapfinal(isplot = F)
#'                        hmobj$hm
#'
#'                      })
#'     })
#'   }, width = 900 , height = 1200, res = 100))


###############################
######## Reactive side        #
###############################

shinyjs::disable("heatm")


#' hmbis is an event reactive function that pre-computed hierarchical clustering on microarray data 
#'
#' @param new_data a data frame with all the individuals selected
#' @param formated  a data frame with the indexes corresponding to the sigificant genes
#' @param new_group  a data frame with the corresponding groups 
#' @param workingPath the current user's repository 
#' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
#' @param Rowdistfun a function used to compute the distance for the rows
#' @param Coldistfun a function used to compute the distance for the columns
#' @param meanGrp a boolean value to computes the mean for each groups; default = F
#'
#' @return  a list of objects which aim is to being passed as argument in the plotHeatmaps function
#' 
#' @export
#' 


hmbis <- reactive( {

      truncatedhat(
        data.matrix(new_data()),
        formated()[[1]], 
        droplevels(new_group()$Grp),
        workingPath = wd_path,
        k = input$clusters,
        mypal = unlist(colors()),
        Rowdistfun = input$dist ,
        Coldistfun = input$dist,
        meanGrp = input$meangrp,
        genename =  csvf()[[3]]
      )
    
    
  
})

output$distPlot <- renderPlot({
    
    validate(need(
      csvf(),
      'You need to import data to visualize to plot the Heatmap' ) %next%
        need(length(choix_test()) >0, 'You need to select a contrast(s) with reactivity triggered you dont need to click on the update heatmap button')
    )
    
    if(is.null(my_intermediate())){
      isolate(heatmapfinal(isplot = F))
      shinyjs::alert("your choice color are not fit to be together!!")
    }
    
    
    if ( input$reactheat == T){
      
      hmbis()
      observe({boolhm <<-T})
      output$heatmbool <- reactive({
        boolhm
      })

      hmboth$tot <- heatmapfinal()
      hmobj$hm <- hmboth$tot[[1]]
      hmobj$obj <-hmboth$tot[[2]]
      
    }
    else{
      #isolate(heatmapfinal(isplot = F))
      #isolate(heatmapfinal())
      #isolate(isisolatehm())
      print("ok")
      validate(
          need(input$heatm, 'You are not in reactive mod anymore, please click on the heatmap button in order to update the heatmap' )
      )
      NULL
    }
      

})





#########################################
######## Plot the data frame wiht input #
#########################################

output$new_test <- renderDataTable(csvf()[[2]]) # Data frame corresponding to the pData

#output$new_data <- renderDataTable(head(csvf()[[1]][2:6])) # Head of the WorkingSet data 

#output$new_group <- renderDataTable(new_group()) # a data frame corresponding to the selected groups

output$data_summary <- renderDataTable(data_summary()) # Summary of the significant genes depending on the pvalue with FC set to (1.2,2,4,6,10)

output$vennresinter <- DT::renderDataTable(DT::datatable(vennfinal(), list(lengthMenu =  c('5', '15', '50'))), server = F)

output$davidgo <- DT::renderDataTable(DT::datatable(davidwebservice()[[as.numeric(input$cutgo)]][, -9] , options = list(scrollX = TRUE) ) )

#output$totalgenbyc <- renderDataTable(grouplength())

#opuput$resumetopgoandkegg <- 

myrenderedtop <- reactive({
  req(csvf())

  select( csvf()[[3]], ProbeName:SystematicName, everything() ) %>%
    mutate_if(is.numeric, funs(format(., digits = 3)))


})

output$new_group <- DT::renderDataTable(DT::datatable(myrenderedtop()[,-c(4:9)] , options = list(scrollX = TRUE) ) )

#' Reactive function that return a list of data frame depending on the comparisons
#'
#' @param adjusted list of three data frame corresponding to the grep of respectively Adj.pval, P.val and logFC columns
#' @param choix_test character corresponding to the defined contrast set by the user
#'
#' @return \usergroup a reactive list containing three data frame for each contrast selected
#'

user_group <- reactive({ 
  
  req(choix_test())
  
 inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  
  myfinal = list()
  for (i in 1:3)
    myfinal[[i]] = (subset(adjusted()[[i]],
                           select = choix_test()))
  
  return(myfinal)
})



#' Reactive function that select specific groups in the data frame
#'
#' @param csvf Data frame of the pData
#' @param choix_grp() character corresponding to the defined groups set by the user
#' 
#' @return \new_group a reactive new factor with the corresponding groups 
#'


new_group <- reactive({ 
  req(choix_grp())
  
  inFile <- input$file
  if (is.null(inFile))
    return(NULL)
  csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(),]
})


#new_group <-reactive(csvf()[[2]][csvf()[[2]]$X %in% choix_individus(),])#obsC <- observe(quote({ print(hmobj$hm) }), quoted = TRUE)

#gores <- reactiveValues()

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

# observe({
#   req(clustergrep())
#   print(length(clustergrep()))
#
#   if (length(clustergrep()) > 400)
#     shinyjs::disable("DAVID")
#
# })


observe({
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




# observe({ TODOOOO
#   req(input$mainhmtabset)
#   if (grepl("hmmainpan",  input$mainhmtabset)) {
#     updateTabsetPanel(session, "tabset25",
#                       selected = "hmpan")
#   }
#   else if (grepl("cuthmmainpan",  input$mainhmtabset)) {
#     #|dfhmclu|maingo
#     updateTabsetPanel(session, "tabset25",
#                       selected = "cutpan")
#   }
# })


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

davidwebservice <-
  eventReactive(input$GO, {
    #Warning: Error in .jcall: org.apache.axis2.AxisFault: Read timed out
    
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
    
    final = lapply(1:NROW(mygodavid), function(x)
      return(format(mygodavid[[x]], digits = 3)))
    
    return(final)
  })


observe({
  req(davidwebservice())
  print(colnames(davidwebservice()))
})


davidurl <- reactive({
  req(clustergrep())
  
  source_python('./python/add.py')
  mydavurl = enrichmentdav(clustergrep())
  mygloburl <- paste(`mydavurl`, ",", "'_blank')")
  
  return(mygloburl)
})


observe({
  req(davidurl())
  url$myurl = davidurl()
})


output$clustgo <- renderPrint({
  validate(
    need(csvf(), 'You need to import data to visualize the data!') %next%
      need(
        input$cutgo,
        'You need to click on the heatmap button! then on the run GO button'
      )
  )
  gores$obj <- isolate(testad())
  
  req(input$cutgo, input$slidergo)
  x <- input$cutgo
  if (!is.null(testad()[[as.integer(x)]])) {
    for (go in input$slidergo[[1]]:input$slidergo[[2]]) {
      if (Ontology(testad()[[as.integer(x)]][[1]][[go]]) == input$onto) {
        cat(paste("GOID:", (GOID(
          gores$obj[[as.integer(x)]][[1]][[go]]
        ))))
        cat("\n")
        cat(paste("Term:", (Term(
          gores$obj[[as.integer(x)]][[1]][[go]]
        ))))
        cat("\n")
        cat(paste("Ontology:", (Ontology(
          gores$obj[[as.integer(x)]][[1]][[go]]
        ))))
        cat("\n")
        cat(paste("Definition:", (Definition(
          gores$obj[[as.integer(x)]][[1]][[go]]
        ))))
        cat("\n")
        cat(paste("Synonym:", (Synonym(
          gores$obj[[as.integer(x)]][[1]][[go]]
        ))))
        cat("\n")
        
        cat("--------------------------------------\n")
      }
    }
  }
  else
    print("Sorry, no enriched genes for this cluster")
  
})



mytransf <- reactive({
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
  
  req(mytransf())  
  # cat("You can select the rows in the table above in order to display the gene names")
  # cat("\n")
  # cat("\n")
    for(i in 1:length(mytransf())){
      cat(paste("GOID and Term: " , unique(mytransf()[[i]]$Term)))
      cat("\n")
      cat("Genes: ")
      cat(paste( mytransf()[[i]]$Genes, collapse = " ,"))
      cat("\n")
      cat("\n")
    }

})



output$savegohmdav = downloadHandler( paste0(basename(file_path_sans_ext(projectname())), '_go.',"xlsx", sep = ''),
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

# output$savegofiltered = downloadHandler( paste0(basename(file_path_sans_ext(projectname())),
#                                         '_go_filtered.',
#                                         "xlsx",
#                                         sep = ''),
#                                  content = function(file) {
#                                    
#                                    withProgress(message = 'Creation of the xlsx table:',
#                                                 value = 0, {
#                                                   n <- NROW(50)
#                                                   for (i in 1:n) {
#                                                     incProgress(1 / n, detail = "Please wait...")
#                                                   }
#                                                   
#                                                   
#                                                   library(xlsx)
#                                                   
#                                                   for (i in 1:length(davidwebservice())) {
#                                                     if (i == 1)
#                                                       write.xlsx(file = file,
#                                                                  davidwebservice()[[i]],
#                                                                  sheetName = paste("Cluster", i))
#                                                     else
#                                                       write.xlsx(
#                                                         file = file,
#                                                         davidwebservice()[[i]],
#                                                         sheetName = paste("Cluster", i),
#                                                         append = TRUE
#                                                       )
#                                                   }
#                                                 })
#                                    
#                                    
#                                    
#                                  }
# )
# 
# 
# output$downloadvennset = downloadHandler(
#   'venns-filtered.csv',
#   content = function(file) {
#     s = input$davidgo_rows_all
#     write.csv2(vennfinal()[s, , drop = FALSE], file)
#   }
# )


#gores$obj <- NULL

#Species <- eventReactive(input$DAVID,{


Species <- reactive({
  if (input$Species == "Homo sapiens" || input$Speciesvenn == "Homo sapiens") {
    # human
    library("org.Hs.eg.db")
    mypack = list(org.Hs.egALIAS2EG, org.Hs.egSYMBOL)
    return(mypack)
  }
  else if (input$Species == "Mus musculus" || input$Speciesvenn == "Mus musculus" ) {
    # Mouse
    library("org.Mm.eg.db")
    mypack = list(org.Mm.egALIAS2EG, org.Mm.egSYMBOL)
    return(mypack)
  }
  else if (input$Species == "Danio rerio" || input$Speciesvenn == "Danio rerio") {
    #Zebra fish
    library("org.Dr.eg.db")
    mypack = list(org.Dr.egALIAS2EG, org.Dr.egSYMBOL)
    return(mypack)
  }
  else if (input$Species == "Gallus gallus" || input$Speciesvenn == "Gallus gallus") {
    # chicken
    library("org.Gg.eg.db")
    mypack = list(org.Gg.egALIAS2EG, org.Gg.egSYMBOL)
    return(mypack)
  }
  else if (input$Species == "equCab2" || input$Speciesvenn == "equCab2") {
    # horse
    library("org.Gg.eg.db")
    mypack = org.Mm.egALIAS2EG
    return(mypack)
  }
  else if (input$Species == "Caenorhabditis elegans" || input$Speciesvenn == "Caenorhabditis elegans") {
    # cC elegans
    library("org.Ce.eg.db")
    mypack = list(org.Ce.egALIAS2EG, org.Ce.egSYMBOL)
    return(mypack)
  }
  else if (input$Species == "Rattus norvegicus" || input$Speciesvenn == "Rattus norvegicus") {
    # Rat
    library("org.Rn.eg.db")
    mypack = list(org.Rn.egALIAS2EG, org.Rn.egSYMBOL )
    return(mypack)
  }
  else if (input$Species == "Sus scrofa" || input$Speciesvenn == "Sus scrofa") {
    # Pig
    library("org.Ss.eg.db")
    mypack = list(org.Ss.egALIAS2EG, org.Ss.egSYMBOL)
    return(mypack)
  }
  
})
output$myNUM <- renderPrint({ # number of signficant genes in the heatmap produced
  if(is.null(formated()[[1]]))
    return("X")
  else
    cat(length(formated()[[1]]))
})


output$indivcol <-  renderText({ # Groups selected
  my_final <<- paste(choix_grp(),as.character(),  sep=",") 
})


output$testtt <- renderText({ #Contrast selected
  my_final <<- paste(choix_test(),as.character(),  sep=",") 
})


output$myPVAL <- renderText({ #pvalue selected
  input$pval
})


output$myFC <- renderText({ #Fold change value selected, default =1
  input$fc
})

output$myMET <- renderText({ #Method for choosing the signficant genes, default = FDR (BH method)
  input$method2
})

output$myCLUST <- renderText({ #number of clusted selected, default = 3
  input$clusters
})

output$myMAT <- renderText({ #Method for the matrix distance, default = correlation method (pearson)
  input$dist
})

output$myPAL <- renderText({ #Colors selected for the different groups, default see palette in the global environment
  if(is.null(mypal()))
    palette[1:length(choix_grp())]
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


output$venngenes <- renderPrint({ # number of signficant genes in the heatmap produced
  req(vennfinal)
  cat(length(vennfinal()$ProbeName))
})


output$contvenn <- renderText({ #Contrast selected
  my_final <<- paste(colnames(user_cont()),as.character(),  sep=",") 
})

output$continter <- renderText({ #Contrast selected
  my_final <<- paste(vennchoice(),as.character(),  sep=",") 
})

output$totalgenes <- renderText({
  req(vennlist())
  sum(sapply(vennlist(),length))
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

vennchoice <- reactive({
  if (is.null (input$intscol))
    return(NULL)
  else
    return(input$intscol)
})

output$myselvenn <- renderUI({
  req(user_cont())
  #intscol <- names(user_cont())#names(adjusted()[[1]][,-1])
  selectInput(
    'intscol',
    'Specify your interaction(s):',
    choices = names(user_cont()),
    multiple = TRUE
  )
})

venninter <- reactive({
  req(vennlist(), user_cont())
  myelist <- setvglobalvenn(vennlist(), user_cont())
  return(myelist)
})


vennfinal <- reactive({
  req(vennchoice())
  if (is.null(vennchoice))
    return(NULL)
  
  
  reordchoice <- vennchoice() %>%
    factor(levels = names(adjusted()[[1]][,-1])) %>%
    sort() %>%
    paste(collapse = "")
  
  
  resfinal = csvf()[[3]] %>%
    filter(ProbeName %in% venninter()[[reordchoice]]) %>%
    select(ProbeName, GeneName, paste0("logFC_", vennchoice())) %>%
    mutate_if(is.numeric, funs(format(., digits = 3)))
  #mutate_if(is.numeric, funs(formatC(., format = "f")))
  
  
  return(resfinal)
})


output$topgenesvenn <- renderUI({
  req(vennfinal(), vennchoice())
  
  numericInput('topgenes',
               'Top genes',
               50,
               min = 1,
               max = length(vennfinal()$ProbeName))
})


venntopgenes <- reactive({
    if (is.null (input$topgenes))
      return(NULL)
    else
      return(input$topgenes)
  })

output$downloadvennset = downloadHandler(
  'venns-filtered.csv',
  content = function(file) {
    s = input$vennresinter_rows_all
    write.csv2(vennfinal()[s, , drop = FALSE], file)
  }
)


plottopgenes <- eventReactive(input$topdegenes, {
  req(vennfinal(), vennchoice(), venntopgenes())
  mycont = paste0("logFC_", vennchoice())
  myplot <-
    topngenes(vennfinal()[input$vennresinter_rows_all, , drop = FALSE], mycont, venntopgenes(), input$meandup)
  return(myplot)
  
})

observeEvent(input$topdegenes, {
  isolate(output$barplotvenn <- renderPlot({
    req(plottopgenes())
    plotOutput(plottopgenes())
    
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
  
})observe({
  req(Venncluster())
  updateSliderInput(session, "clusterNumber", max = nrow(summary(Venncluster())))
})

output$clusterPlot <- renderPlot({
  req(Venncluster())
  if(input$clusterNumber == 1)
    shinyjs::alert("There's not enough genes in your interaction(s)")
  plot2D(Venncluster(), input$clusterNumber)
})

output$debug <- renderPrint({
  req(Venncluster())
  summary(Venncluster())
})


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
                     mygodavid = probnamtoentrezvenn(vennfinal()$GeneName , Species()[[1]]) %>%
                     davidqueryvenn(input$Speciesvenn) %>% withCallingHandlers(error = timeoutdav)
                   }, warning = function(e) {
                     
                     shinyjs::alert("David's server is busy")
                     warning("David's server is busy")
                     return(cbind("David's server is busy") %>% as.data.frame() %>% setNames("Error"))
                     
                   })
                 })
    
    
    return(mygodavid)
  })
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
  req(adjusted())
  
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

myindex<- reactive({
  
  myl = lapply(seq(ncol(adjusted()[[1]])),function(x)
    return(which(adjusted()[[1]][[x]] < 0.05)))
    
  indexnull = which( sapply(myl ,length) == 0)
  final = colnames(adjusted()[[1]][,-c(indexnull)])
  return(final)
  
})
#########################################
######## Venn part                      #
#########################################


#observeEvent(input$vennd, {
  output$myVenn <- renderPlot({
 
    
    validate(
      need(csvf(), 'You need to import data to visualize this plot!') %next%
      need(length(user_cont()) >0,  'You need to  select your p-value and then some groups!'))

    req(Vennplot())
    
    Vennplot()
  })
  
 
observe({
  
  validate(
    need(csvf(), 'You need to import data to visualize this plot!'))
   
  output$savevenn <- downloadHandler(filename <- function() {
    paste0(basename(tools::file_path_sans_ext(projectname())),
           '_venn_diagram.',
           input$formven,
           sep = '')
  },
  content <- function(file) {
    if (input$formven == "pdf")
      
      pdf(
        file,
        width = 12.5,
        height = 12,
        pointsize = 12
      )
    
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
      cairo_ps(filename=file, width=11, height=11,pointsize = 12)
    
    
    grid.draw(Vennplot())
    dev.off()
  })
  
})