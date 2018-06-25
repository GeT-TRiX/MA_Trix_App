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
#' @param input col1 a character color for the highest values
#'
#' @return col_choice1  a reactive value
#'
#' @export
#'

col_choice1 <- function(input) {
  col_choice1 <- reactive({
    return(input$col1)
  })

}

#' col_choice3 is a reactive function that return a character color
#'
#' @param input col3 a character color for the lowest values
#'
#' @return  col_choice3 reactive value
#'
#' @export
#'

col_choice3 <- function(input) {
  col_choice3 <- reactive({
    return(input$col3)
  })
}

#' my_intermediate is a reactive function that return a character color
#'
#' @param col_choice1 character color for the lowest values
#' @param col_choice3 character color for the highest values
#'
#' @return inter a reactive character intermediate color between the lowest and the highest values
#'
#' @export
#'

my_intermediate <- function(col_choice1, col_choice3) {
  my_intermediate <- reactive({
    if (col_choice1() == "green" & col_choice3() == "red")
      inter = "black"

    else if (col_choice1() == "orange" & col_choice3() == "red")
      inter = "yellow"

    else if (col_choice1() == "blue" & col_choice3() == "red")
      inter = "white"

    else if (col_choice1() == "blue" & col_choice3() == "yellow")
      inter = "green"


    return(inter)

  })
}

##################################
######## Modify buttons          #
##################################


#' global is a ReactiveValues function
#'
#' @param clicked bool set to FALSE
#'
#' @return clicked a boolean which can be TRUE or FALSE
#'

global <- function(clicked){
  global <- reactiveValues(clicked = FALSE)
}

#' transfheatm is an observe function that change the global value after a clickable event
#'
#' @param input heatm a numeric value by default set to 0
#' @param global a reactivevalues boolean value; by default set to FALSE
#'
#' @return global a boolean reactivalues set to TRUE
#'
#' @export


transfheatm <- function(input, global){

  observe({
    if (length(input$heatm)) {
      # giving a length once it's clicked
      if (input$heatm)
        global$clicked <- TRUE
    }
  })

  return(global)
}


output$button <-  renderUI({ # if button is clicked changed his style.css
  if(!is.null(input$heatm) & global$clicked){
    shiny::actionButton("heatm", "Update Heatmap", icon = icon("repeat"), style = "color: #fff; background-color: #b77033; border-color: #b77033")
  }
  else{
    shiny::actionButton("heatm", "Print Heatmap", style = "color: #fff; background-color: #337ab7; border-color: #337ab7")
  }

})
#################################
######## Select the comparisons #
#################################

# Render in the UI.R the levels for the pData Group
output$testout <- renderUI(
  checkboxGroupInput(
    inputId = "test" ,
    label =  "Choose your comparison",
    choices =  colnames(adjusted()[[1]][,-1])
    #,selected = colnames(adjusted()[, -1])

  )
)

#Select all the contrasts
observeEvent(input$allTests, {
  updateCheckboxGroupInput(
    session,
    "test",
    label = "Choose your comparison",
    choices = colnames(adjusted()[[1]][,-1]),
    selected = colnames(adjusted()[[1]][,-1])
  )
})

#Unselect all the contrasts
observeEvent(input$noTests, {
  updateCheckboxGroupInput(session,
                           "test",
                           label = "Choose your comparison",
                           choices = colnames(adjusted()[[1]][, -1]))
})

#' choix_test is an eventreactive function in the aim of selecting different comparison after a clickable event
#'
#' @param input heatm a clickable button
#'
#' @return  a reactive value of type character for the different comparisons selected
#'
#' @export

choix_test <- function(input){

choix_test <- eventReactive(input$heatm, {
  return(input$test)
}, ignoreNULL = F)

  return(choix_test)
}


#################################
######## Select the groups      #
#################################


# Render in the UI.R the levels for the pData Group
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
# Select all groups
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


# Unselect all groups
observeEvent(input$noIndividus, {
  updateCheckboxGroupInput(session,
                           "indiv",
                           label = "Choose your group to visualize",
                           #choices = colnames(csvf()[[1]][,-1]))
                           choices =  levels(csvf()[[2]]$Grp))
})

#' choix_test is a reactive function in the aim of selecting different groups
#'
#'
#' @return  a reactive value of type character for the different groups selected
#'
#' @export

choix_grp <- function(input) {

  choix_grp <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    return(input$indiv)
  })

}



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
#' @param input heatm a clickable button
#' @param csvf a Data frame corresponding to the pData table
#'
#' @return new_group an eventreactive factor with the corresponding groups selected
#'
#' @export

new_group <- function(input, csvf) {

  new_group <- eventReactive(input$heatm, {
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(), ]
  }, ignoreNULL = F)

  return(new_group)
}



#' new_data is a reactive function that aim is to select specific individuals in the data frame
#'
#' @param \csvf Data frame corresponding to the Workingset
#'
#' @return new_data a reactive data frame with specific columns depending on the user's choices
#'
#' @export

new_data <- function(csvf) {

  new_data <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    #subset(csvf()[[1]],select = choix_individus())
    select(csvf()[[1]], as.character(factor(new_group()$X)))
  })

  return(new_data)
}
#########################################
######## Colors for the  PCA groups     #
#########################################

#' colspca is a reactive function which aim is to dynamically create widgets in function of the number of groups
#'
#' @param brewer.pal a local list defined in the RcolorBrewer package
#' @param mycolgrppca a dataframe representing the selected groups
#'
#' @return colspca a reactive number of widget-s
#'
#' @export

colspca <- function(brewer.pal,mycolgrppca ){


  colspca <- reactive({
    pcapal = brewer.pal(8, "Dark2") %>%
      list(brewer.pal(10, "Paired")) %>%
        unlist()


    lapply(seq_along(unique(mycolgrppca())), function(x) {
      colourInput(
        paste("colpca", x, sep = "_"),
        levels(mycolgrppca())[x],
        pcapal[x],
        allowedCols =  pcapal,
        palette = "limited",
        returnName = T
      )
    })
  })
  return(colspca)
}

output$myPanelpca <- renderUI({ # display the colourInput in the UI
  colspca()
})


#' colorspca is a reactive function which aim is to create as many variables as groups
#'
#' @param mycolgrppca  a reactive data frame
#'
#' @return colorspca a reactive  list containing the different variable names
#'
#' @export


colorspca <- function(mycolgrppca) {
  colorspca <- reactive({
    lapply(seq_along(unique(mycolgrppca())), function(i) {
      input[[paste("colpca", i, sep = "_")]]
    })
  })
  return(colorspca)
}


#' colorfluidpca is a reactive function wich aim is to group colors side by side
#' depending of the number of groups odd or even for  the gui.
#' 
#' 
#'
#' @param colspca 
#'
#' @return
#' @export
#'


colorfluidpca <- function(colspca ) {

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

}


#' mycolgrppca is a reactive function which aim is to display the total number of groups
#'
#' @param csvf a dataframe
#' @param new_grouppca a reactive factor with the corresponding groups selected
#'
#' @return mycolgrppca a reactive reorder dataframe
#'
#' @export

mycolgrppca <- function(csvf) {
  mycolgrppca <- reactive  ({
    mygrpcol <- csvf()[[2]]$Grp %>%
      sort() %>%
      unique()
    
  
    return(mygrpcol)
  })
  return(mycolgrppca)
}

###############################
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
#' @param ouput character depending on the user's choice
#'
#' @return mean_grp a reactive value of type character depending on the user's input
#'
#' @export

mean_grp <- function(output){

  mean_grp <- reactive({
    return(output$value)
  })
return(mean_grp)
}


###############################
######## Load the csv files   #
###############################

showmark <- T # Boolean uses to hide or show the mardkwon serving to load data


#' boolmark is a reactive function returned to the tab1.R
#'
#' @return showmark a reactive value of type boolean corresponding to the loading status by default it is set to True
#'
#' @export

boolmark <- function() {

  output$boolmark <- reactive({
    showmark
  })
}

outputOptions(output,"boolmark",suspendWhenHidden=F)

#' Reactive function in the aim of loading csv files
#'
#' @param file html id for files loaded in csv format
#'
#' @return csvf a reactive value of type list containing three data frames toptable, workingset and the pData
#'
#' @export


csvf <- function (input) {
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
      #Sys.sleep(2.5)
      closeAlert(session, "entryalert")

      return(NULL)
    }

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
      return(NULL)
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
        #' @return list of data frame objects
        #'
        #' @export
        
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

    observe({
      showmark <<- F
    }) # modify and lock the bool value to false

    #' Reactive function returned to the tab1.R
    #'
    #' @return \showmark a reactive value of type boolean set to False
    #'

    output$boolmark <- reactive({
      showmark
    })

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
  })
  return(csvf)
}

#########################################
######## Cut heatmap Part               #
#########################################


#' p is a reactive function that return a heatmap gplots object
#'
#' @param updateheatm  clickable input button
#' @param hmobj$obj a reactive value object
#'
#' @return p an heatmap object isolate
#'
#' @export
#'

p <- function(input, heatmapfinal) {
  p <- eventReactive(input$updateheatm, {
    isolate(heatmapfinal())
  })
  return(p)
}

#' rownametoX is a reactive function that change the rownames values
#'
#' @param csvf a data frame
#'
#' @return rownametoX a reactive data frame
#'
#' @export
#'

rownamtoX <- function(csvf) {
  rownamtoX <- reactive({
    mycsv = csvf()[[3]]
    row.names(mycsv) = mycsv$X

    return(rownamtoX)
  })
}

#' cutfinal is a reactive function that return heatmap or ggplot2 object
#'
#' @param p heatmap object
#' @param input a numeric value to cut the dendogram
#' @param new_data a data frame with specific columns depending on the user's choices
#' @param rownamtoX a data frame
#' @param groups a data frame of the choosen groups
#' @param input2 heatmap object
#' @param input3 a character to select the plot to display heatmap, boxplot or stripchart
#'
#' @return \cutfinal a ggplot object or heatmapply object
#'
#' @export

cutfinal <- function(p, input, new_data, rownamtoX, groups, input2 , input3){

  cutfinal <- reactive({
    cutHeatmaps(
      p(),
      height = input$cutheight ,
      exprData = data.matrix(new_data()),
      groups = droplevels(new_group()$Grp),
      DEGres =  rownamtoX()[,-1],
      num = input$cutcluster,
      type = input$cutinfo
    )
  })
}

# render to the ui the number of clusted for a define height in function of the current heatmap object
output$cutcluster <- renderUI({
  req(p())

  cut02 = cut(p()$rowDendrogram, h = input$cutheight)
  selectInput("cutcluster",
              "Choose your cluster",
              choices =  seq(1, length(cut02$lower), by = 1))
})


output$event <- renderPrint({ # interactive cursor that shows the selected points
  d <- event_data("plotly_hover")
  if (is.null(d))
    "Hover on a point!"
  else
    cat("Average expression Z-score over replicates; ",
        length(d$pointNumber),
        " probes")
})


### Add function


observe({
  if (req(input$cutinfo) == "Heatmap") {
    output$cutheatmap <- renderPlotly({ # Plot/Render an object of class plotly
       cutfinal()

    })
  }
  else{
    output$cutheatmap <- renderPlotly({
      ggplotly(cutfinal(), height = 800, width = 1200)

    })
  }
})


###############################
######## Summarise data       #
###############################

#' data_summary is a reactive function that return the indexes for the signficant genes
#'
#' @param csvf data frame
#' @param input a numeric pvalue
#'
#' @return \datasummary a reactive data frame with the indexes corresponding to the sigificant genes for 5 Fold change 1.2,2,4,6,10
#'
#' @export

data_summary <- function(csvf, input) {

  data_summary <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    myfinalfc(csvf()[[3]], input$pval1)
  })

  return(data_summary)

}

#' adjusted is a reactive function that return a list containing multiple data frames
#' with the adj.P.val, logFC and P.value selected for the corresponding groups
#'
#' @param csvf Data frame corresponding to the Alltoptable
#'
#' @return adjusted a reactive list of data frames
#'
#' @export

adjusted <- function(csvf) {

  adjusted <- reactive({
    df <- csvf()
    if (is.null(df))
      return(NULL)

    myrpl = c("^adj.P.Val_", "^logFC_", "^P.value_")
    grepdf = c("X|^adj.P.Val", "X|^logFC", "X|^P.value")

    adj = csvf()[[3]][, grep("X|^adj.P.Val",
                             names(csvf()[[3]]),
                             value = TRUE)]

    logfc = csvf()[[3]][, grep("X|^logFC",
                               names(csvf()[[3]]),
                               value = TRUE)]

    pval = csvf()[[3]][, grep("X|^P.value",
                              names(csvf()[[3]]),
                              value = TRUE)]


    mygrep = list(adj, logfc, pval)


    for (i in 1:length(mygrep))
      names(mygrep[[i]]) = gsub(
        pattern = myrpl[i],
        replacement = "",
        x = names(mygrep[[i]]),
        perl = T
      )

    return(mygrep)

  })

  return(adjusted)

}

#########################################
######## Colors for the  groups         #
#########################################

#' mycolgrp is a reactive function which aim is to display the number of groups selected
#'
#' @param new_group a subset data frame of the pData
#'
#' @return mycolgrp a reactive data frame
#'
#' @export

mycolgrp <- function(new_group) {
  mycolgrp <- reactive  ({
    mygrpcol <- new_group()$Grp %>%
      sort() %>%
      unique() %>%
      droplevels()

    return(mygrpcol)
  })
  return(mycolgrp)
}

#' cols is a reactive function which aim is to dynamically create widgets in function of the number of groups
#'
#' @param palette a local list defined in the environment
#' @param mycolgrp a dataframe representing the selected groups
#' @mypaletA a list which contaings the colors values corresponding to the different groups
#'
#' @return cols a reactive number of widget-s
#'
#' @export


cols <- function(palette, mycolgrp, mypaletA) {
  cols <- reactive({
    if (is.null(mypal()))
      lapply(seq_along(mycolgrp()), function(i) {
        colourInput(
          paste("col", i, sep = "_"),
          levels(mycolgrp())[i],
          palette[i],
          allowedCols =  palette,
          palette = "limited",
          returnName = T
        )
      })

    else
      lapply(seq_along(mycolgrp()), function(i) {
        colourInput(
          paste("col", i, sep = "_"),
          levels(mycolgrp())[i],
          mypaletA()[i],
          allowedCols =  palette,
          palette = "limited",
          returnName = T
        )
      })
  })

}
#' mypaletA is a reactive function which aim is to set colors if the advanced graphical settings are not displays
#'
#' @param colors a list of input for the different user's choice
#'
#' @return mypaletA a reactive list of colors attributed by ranking order to the different groups
#'
#' @export

mypaletA <- function(colors){
  mypaletA <- reactive  ({
    if (is.null(mypal))
      return(NULL)
    else
      mypal = (colors())
    return(mypal)
  })
  return(mypaletA)
}

#' mypal is a reactive function which aim is to unlist the choice of colors
#'
#' @param colors a list of input for the different user's choice
#'
#' @return mypal a reactive  that unlist the colors attributed to the different groups
#'
#' @export

mypal <- function(colors) {
  mypal <- reactive({
    unlist(colors())
  })
  return(mypal)
}


output$myPanel <- renderUI({ # display the colourInput in the UI
  cols()
})

#' colors is a reactive function which aim is to create as many variables as groups
#'
#' @param mycolgrp  a reactive data frame
#'
#' @return colors a reactive  list containing the different variable names
#'
#' @export

colors <- function(mycolgrp) {
  colors <- reactive({
    lapply(seq_along(mycolgrp()), function(i) {
      input[[paste("col", i, sep = "_")]]
    })
  })
  return(colors)
}
###############################
########heatmap function & co #
###############################


#' heatmapfinal is an isolate function that only react to a user's click on the heatmap button
#'
#' @param new_data a data frame with all the individuals selected
#' @param formated  a data frame with the indexes corresponding to the sigificant genes
#' @param new_group  a data frame with the corresponding groups
#' @param workingPath the current user's repository
#' @param my_palette a vector of colors
#' @param k a numeric value which aim is to defined the treshold value to cut the dendogram input$clusters
#' @param Rowdistfun a character value set by the user to defined the method to calculate the dendogram matrix distance for the genes input$dist
#' @param Coldistfun a character value set by the user to defined the method to calculate the dendogram matrix distance for the contrasts input$dist
#' @param keysize
#' @param mycex a numeric value which aim is to change the size of the legend in the heatmap defined by the user input$legsize
#' @param cexrow  a numeric value to change the size of the police legend for the rows input$rowsize
#' @param cexcol a numeric value to change the size of the police legend for the columns input$colsize
#' @param meanGrp a boolean value to compute or not the mean of each contrasts in the heatmap input$meangrp
#' @param mypal a list of values
#' @param showcol a boolean value used to hide or show the colnames input$colname
#' @param showrow a boolean value used to hide or show the rownames input$rowname
#' @param genename a data frame list corresponding to the gene names
#'
#' @return heatmapfinal a heatmap object
#'
#' @export


heatmapfinal <- function() {

  isolate({
    plotHeatmaps(
      data.matrix(new_data()),
      formated(),
      droplevels(new_group()$Grp),
      workingPath = wd_path,
      my_palette = colorRampPalette(c(
        col_choice1(), my_intermediate(), col_choice3()
      ))(n = 75),
      k = input$clusters,
      Rowdistfun = input$dist ,
      Coldistfun = input$dist,
      mycex = input$legsize ,
      cexrow = input$rowsize ,
      cexcol = input$colsize ,
      meanGrp = input$meangrp,
      mypal =  unlist(colors()),
      showcol = colname(),
      showrow = rowname(),
      genename = csvf()[[3]]$GeneName
    )

  })
}

#' rowname is a reactive function which aim is to hide or show the rownames
#'
#' @param input$rowname  a boolean radio button input
#'
#' @return rowname a reactive boolean value
#'
#' @export

rowname <- function(input) {
  rowname <- reactive({
    rowname <- switch(input$rowname,
                      hide = F,
                      show = T,
                      F)
    return(rowname)
  })
  return(rowname)
}

#' colname is a reactive function which aim is to show or hide the colnames
#'
#' @param input$colname  a boolean radio button input
#'
#' @return colname a reactive  reactive boolean value
#'
#' @export

colname <- function(input) {
  colname <- reactive({
    colname <- switch(input$colname,
                      hide = T,
                      show = F,
                      F)
    return(colname)
  })
  return(colname)
}

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



###############################
######## Hide buttons         #
###############################


shinyjs::onclick("toggleAdvanced",
                 shinyjs::toggle(id = "advanced", anim = TRUE)) ## hide and show event

# shinyjs::onclick("toggleAdvancedPCA",
#                  shinyjs::toggle(id = "advancedPCA", anim = TRUE))

shinyjs::onclick("toggleAdvancedcolors",
                 shinyjs::toggle(id = "advancedcol", anim = TRUE))

#' formated is a reactive function that return the indexes for the signficant genes
#'
#' @param user_group a list of three data frame with rows selected according to the contrasts selected
#' @param intput$fc a numeric FC selected
#' @param input$method2 a character method, default = BH
#' @param input$pval a numeric pvalue
#' @param input$maxgen a numeric maxgen, default = NULL
#'
#' @return formated a reactive data frame with the indexes corresponding to the sigificant genes
#'
#' @export

formated <- function (user_group, input, input2, input3, input4) {
  formated <- reactive({
    #req(!is.null(user_group()))
    #treated = formating(new_test(), csvf()[[1]], input$pval)
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
  return(formated)
}

# new_group <- reactive( csvf()[[2]] %>%
#                          filter( X ==  list_ind()))


#' Reactive function that return a comparison data frame with the specific user's selection
#' it's an old function
#'
#' @param adjusted  a reactive list of data frames
#'
#' @return new_data a  data frame with all the individuals selected
#'
#' @export

data_sign <- function(adjusted) {
  data_sign <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    ptv <- c(.01, .05)
    cbind.data.frame("FDR<1%" = colSums(adjusted()[,-1] < ptv[1]),
                     "FDR<5%" = colSums(adjusted()[,-1] < ptv[2]))

  })

}

# data_sign <- reactive({
#   inFile <- input$file
#   if (is.null(inFile))
#     return(NULL)
#   createdfsign(adjusted())
# })

## Checkboxgrp

# choix_individus <- reactive({
#   return(input$indiv)
# })


# choix_grp <- reactive({
#   return(input$indiv)
# })

# output$indiv <-  renderText({
#   choix_individus()
# })

# output$indiv <-  renderText({
#   my_final <<- paste(choix_grp(),as.character(),  sep=",")
# })


#new_group <-reactive(csvf()[[2]][csvf()[[2]]$X %in% choix_individus(),])


# new_group <- reactive({
#   inFile <- input$file
#   if (is.null(inFile))
#     return(NULL)
#   csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(), ]
# })

#########################################
######## PCA part                       #
#########################################

#' PCAres is a reactive function that computed a PCA of non-normalized data
#'
#' @param csvf a data frame corresponding to the WorkingSet
#'
#' @return PCAres a reactive data frame with PCA attributes
#'
#' @export

PCAres <- function(csvf) {
  PCAres <- reactive({
    if (is.null(csvf()[[1]]))
      return(NULL)

    mypca = res.pca(csvf()[[1]][, -1], scale = F)
    return(mypca)
  })
  return(PCAres)
}

#' Scree_plot is a reactive function which aim is to display the eigenvalues of the data
#'
#' @param PCAres a reactive data frame with PCA attributes
#'
#' @return \Screeplot a reactive plot
#'
#' @export

Scree_plot <- function(PCAres) {
  Scree_plot <- reactive({
    mybar = eboulis(PCAres())
    return(mybar)
  })
}

output$eigpca <- renderPlot({
  plot(Scree_plot())

}, width = 1200 , height = 600, res = 100)


#' labeled is a reactive function which aim is to display or not the labels in the PCA render plot
#'
#' @param input$label a boolean
#'
#' @return \labeled a reactive  boolean depending of the user's choice to display or not the labels
#'
#' @export

labeled <- function(input) {
  labeled <- reactive({
    if (input$label == T)
      showlab = "all"
    else
      showlab = "none"

    return (showlab)
  })
  return(labeled)
}

output$PCA <- renderPlot({

  plot(PCAplot())

}, width = 1200 , height = 800, res = 100)


output$savepca <- downloadHandler(

  filename <- function() {
    paste0(basename(file_path_sans_ext("myfile")), '_pca.png', sep='')
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

###############################
########  PCA function        #
###############################

#' PCAplot is a function that return a factoextra object of PCA type
#'
#' @param brew.pal a color object from the RcolorBrewer package
#' @param PCAres a data frame with PCA attributes
#' @param label a boolean value depending of the user's choice to display or not the labels
#' @param habillage a data frame corresponding to the pData
#' @param addEllipseda a boolean value to add ellipse to the data distribution  for the different groups
#' @param ellipse.level a numeric value set to 0.8
#' @param repel a boolean value to avoid overlaps between the label points
#' @param axes a numeric vector of length 2 specifying the dimensions to be plotted
#' @param labelsize a numeric value representing the police size to display for the different labels
#' @param pointsize a numeric value representing the diameter of each points displayed in the graph
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
    habillage = csvf()[[2]]$Grp,
    addEllipses = input$ellipse ,
    ellipse.level = 0.8,
    repel = input$jitter,
    axes = c(as.integer(input$dim1), as.integer(input$dim2)),
    labelsize = input$labelsiize,
    pointsize = input$pointsiize
  )

  return(p + scale_color_manual(values = empty()))
}

observeEvent(input$heatm, {
  output$distPlot <- renderPlot({
    isolate({
      if (!is.null(formated()))
        withProgress(message = 'Plotting heatmap:', # Add sliderbar when loading heatmap
                     value = 0,
                     {
                       n <- NROW(formated()) #number of row in the formated dataframe

                       for (i in 1:n) {
                         incProgress(1 / n, detail = "Please wait...")
                       }
                       heatmapfinal()
                     })
    })
  }, width = 900 , height = 1200, res = 100)


  output$save <- downloadHandler(filename <- function() {
    paste0(basename(file_path_sans_ext("myfile")),
           '_heatmap.',
           input$form,
           sep = '')
  },
  content <- function(file) {
    if (input$form == "emf")

      emf(
        file,
        width = 7,
        height = 7,
        pointsize = 12,
        coordDPI = 300
      )

    else if (input$form == "png")
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
          height = 7)

    if (!is.null(formated()))
      withProgress(message = 'Plotting heatmap:',
                   value = 0,
                   {
                     n <- NROW(formated())

                     for (i in 1:n) {
                       incProgress(1 / n, detail = "Please wait...")
                     }

                     heatmapfinal()
                   })
    dev.off()

  })
})

#########################################
######## Plot the data frame wiht input #
#########################################

output$new_test <- renderDataTable(csvf()[[2]]) # Data frame corresponding to the pData

output$new_data <- renderDataTable(head(csvf()[[1]][2:6])) # Head of the WorkingSet data

output$new_group <- renderDataTable(new_group()) # a data frame corresponding to the selected groups

output$data_summary <- renderDataTable(data_summary())


# user_group is a reactive function that summarise the significant genes depending on the pvalue with FC set to (1.2,2,4,6,10)#' Reactive function that return a list of data frame depending on the comparisons
#'
#' @param adjusted list of three data frame corresponding to the grep of respectively Adj.pval, P.val and logFC columns
#' @param choix_test character corresponding to the defined contrast set by the user
#'
#' @return usergroup a reactive list containing three data frame for each contrast selected
#'
#' @export

user_group <- function(adjusted, choix_test) {

  user_group <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)

    myfinal = list()
    for (i in 1:3)
      myfinal[[i]] = (subset(adjusted()[[i]],
                             select = choix_test()))

    return(myfinal)
  })
  return(user_group)
}


#' new group is a reactive function that select specific groups in the data frame
#'
#' @param csvf a data frame of the pData
#' @param choix_grp a vector character corresponding to the defined groups set by the user
#'
#' @return new_group a reactive new factor with the corresponding groups
#'
#' @export


new_group <- function(csvf, choix_grp) {
  new_group <- reactive({
    inFile <- input$file
    if (is.null(inFile))
      return(NULL)
    csvf()[[2]][csvf()[[2]]$Grp %in% choix_grp(), ]
  })
  return(new_group)
}


#new_group <-reactive(csvf()[[2]][csvf()[[2]]$X %in% choix_individus(),])


output$myNUM <- renderPrint({ # number of signficant genes in the heatmap produced
  if(is.null(formated()))
    return("X")
  else
    cat(length(formated()))
})


output$indivcol <-  renderText({ # Groups selected
  my_final <<- paste(choix_grp(),as.character(),  sep=",")
})


output$test <- renderText({ #Contrast selected
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


#########################################
######## Venn diagram                   #
#########################################

value=T # boolean at t=0

#' bool is a reactive function that return the bool value in the local environment
#'
#' @value a boolean
#'
#' @return bool a reactive boolean outside the reactive environment
#'
#' @export

bool <- function(value) {

  output$bool <- reactive({
    value
  })

}

outputOptions(output,"bool",suspendWhenHidden=F)

#' vennlist is a reactive function which aim is to return a list of signficant genes for a treshold pvalue of 5%
#'
#' @csvf a data frame
#' @user_cont a data frame with the contrast selected
#'
#' @return vennlist a reactive list
#'
#' @export

vennlist <- function(csvf, user_cont) {
  vennlist <- reactive({
    if (is.null(csvf()))
      return(NULL)
    mycont = Vennlist(pval = csvf()[[3]], user_cont())
    return(mycont)
  })
  return(vennlist)
}

#' Vennplot is a reactive function that return a plot object or a link if the user want to display more tha  5sets
#'
#' @param Vennploted a reactive object
#'
#' @return Vennplot a reactive object to be plot
#'

Vennplot <- function (Vennploted) {
  Vennplot <- reactive({
    #' Vennplot is a reactive function that return an object of type venn if the number of set is stricly inferior to 6
    #' or a link to a website if it's not
    #'
    #' @param user_cont a reactive data frame with the selected contrast
    #' @param input$vennsize the police size for the contrasts
    #' @param vennlist a list
    #'
    #' @return Vennploted a reactive object to be plot
    #'
    #' @export

    Vennploted <- function(user_cont, input, vennlist) {
      Vennploted <- reactive({
        if (length(user_cont()) <= 5) {
          g = Vennfinal(vennlist(), user_cont(), cex = input$vennsize)


          observe({
            value <<- T
          }) # listen inside the reactive expression

          #' output$bool is a reactive function that set the bool value to T
          #'
          #' @value a boolean
          #'
          #' @return bool a reactive boolean inside the reactive environment
          #' 
          #' @export

          output$bool <- reactive({
            value
          })

          return(g)
        }
        else {
          observe({
            value <<- F
          }) # listen inside the reactive expression

          #' output$bool is a reactive function that set the bool value to F
          #'
          #' @value a boolean
          #'
          #' @return bool a reactive boolean inside the reactive environment
          #'

          output$bool <- reactive({
            value
          })

          output$image <- renderUI({
            tags$img(src = "https://i.imgur.com/lB5wmMp.png")
          })
          url <-
            a("venntools", href = "http://jvenn.toulouse.inra.fr/app/example.html", target = "_blank")
          output$sorry <-
            renderUI({
              tagList(
                "You're trying to plot more than 5 sets, download the csv file and use the following tool",
                url
              )
            })

        }
      })
    }
    return(Vennploted())
  })

}


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
#' @return indnull a reactive vector
#'
#' @export

indnull <- function(vennlist) {
  indnull <- reactive({
    indexnull = which(sapply(vennlist() , length) == 0)
    return(indexnull)
  })
}


#' choix_cont is a reactive function that return the contrast selected by the user
#'
#' @param input$cont a set of contrasts selected by the user
#'
#' @return choix_cont a set of characters input
#'
#' @export
#'

choix_cont <- function(input){
  choix_cont <- reactive({
    return(input$cont)
  })
}


#' user_cont is a reactive function that  return the contrast selected by the user
#'
#' @param adjusted a data frame corresponding to the contrasts selected
#' @param choix_cont a set of character
#'
#' @return user_cont a reactive data frame with the contrast selected
#'
#' @export
#'

user_cont <- function(adjusted, choix_cont) {
  user_cont <- reactive({
    mysel = (subset(adjusted()[[1]],
                    select = choix_cont()))
    return(mysel)
  })
  return(user_cont)
}

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



#' Title
#'
#' @param adjusted 
#'
#' @return
#' @export
#'
#' @examples
#' 

myindex <-function(adjusted){

myindex<- reactive({
  
  myl = lapply(seq(ncol(adjusted()[[1]])),function(x)
    return(which(adjusted()[[1]][[x]] < 0.05)))
  
  indexnull = which( sapply(myl ,length) == 0)
  final = colnames(adjusted()[[1]][,-c(indexnull)])
  return(final)
  
})
}


#########################################
######## Venn part                      #
#########################################



observeEvent(input$vennd, {
  output$myVenn <- renderPlot({
    Vennplot()
  }, width = 1200 , height = 800, res = 100)


  output$savevenn <- downloadHandler(filename <- function() {
    paste0(basename(file_path_sans_ext("myfile")),
           '_venn_diagram.',
           input$formven,
           sep = '')
  },
  content <- function(file) {
    print(input$formven)
    if (input$formven == "emf")

      emf(
        file,
        width = 7,
        height = 7,
        pointsize = 12,
        coordDPI = 300
      )

    else if (input$formven == "png")
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
          width = 7,
          height = 7)


    plot(Vennplot())
    dev.off()
  })

})


#####################################"


#obsC <- observe(quote({ print(hmobj$hm) }), quoted = TRUE)

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
  
  
#' totaclust is a reactive function which aim is to dynamically return a widget object of selectinput type ranging from 1 to the maximum number of cluster
#'
#' @param hmobj data frame of the significant genes associated with the corresponding cluster index
#'
#' @return selectInput widget
#' @export
#'

  totalclust <- function( cluster ){
  
  totalclust <- reactive({
    req(hmobj$hm)
    
    n <- unique(hmobj$hm$cluster)
    selectInput("cutgo",
                "Choose your cluster",
                choices =  seq(1, NROW(n) , by = 1))
    
  })
  }
  
  
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

#' clustergrep is a reactive function which aim is to return a list of genes for the selected cluster without the non-annotated genes
#'
#' @param hm data frame of the significant genes associated with the corresponding cluster index
#' @param cutgo a numeric input 
#'
#' @return list of genes
#' @export
#'
#' 

clustergrep <- function(hm,  cutgo){

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
}

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


davidwebservice <- function(GO, hm, Species, catinfo){

davidwebservice <- eventReactive(input$GO, {
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
}


observe({
  req(davidwebservice())
  print(colnames(davidwebservice()))
})


#' davidurl is a reactive function that aim is to return an url of grouped genes 
#'
#' @param clustergrep list of genes
#'
#' @return character 
#' @export
#'

davidurl <- function(clustergrep) {
  davidurl <- reactive({
    req(clustergrep())
  
    source_python('./python/enrichmurl.py')
    mydavurl = enrichmentdav(clustergrep())
    mygloburl <- paste(`mydavurl`, ",", "'_blank')")
    
    return(mygloburl)
  })
}


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


#' mytransf is a reactive function which aim is to convert entrez ID to GENE  the selected rows in the output data table
#'
#' @param davidwebservice data frame 
#' @param cutgo a numeric input 
#' @param davidgo_rows_selected selected rows
#' @param Species list of annotated elements 
#'
#' @return a data frame
#' @export
#'


mytransf <- function(davidwebservice, cutgo, davidgo_rows_selected, Species) {
  mytransf <- reactive({
    req(davidwebservice())
    
    
    myselectedrows = (davidwebservice()[[as.numeric(input$cutgo)]][input$davidgo_rows_selected, c("Genes", "Term"),  drop = FALSE])
    
    if (length(myselectedrows["Genes"][[1]]) > 0) {
      myentreztosymb = lapply(1:NROW(myselectedrows), function(x) {
        myselectedrows$Genes[[x]] %>% strsplit(", ") %>% unlist() %>% mget(x = .,
                                                                           envir = Species()[[2]],
                                                                           ifnotfound = NA) %>%  unlist() %>%
          unique() %>% cbind(myselectedrows$Term[[x]]) %>% as.data.frame() %>% setNames(., c("Genes", "Term"))
        
      })
      
      return(myentreztosymb)
    }
    else{
      return(NULL)
    }
    
  })
}

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




#' Species is a reactive function which aim is to return annotated packages for a specific genome
#'
#' @param Species character input
#' @param Speciesvenn character input
#'
#' @return
#' @export
#'

Species <- function(Species, Speciesvenn) {
  
  
  
  
  Species <- reactive({
    if (input$Species == "Homo sapiens" ||
        input$Speciesvenn == "Homo sapiens") {
      # human
      library("org.Hs.eg.db")
      mypack = list(org.Hs.egALIAS2EG, org.Hs.egSYMBOL)
      return(mypack)
    }
    else if (input$Species == "Mus musculus" ||
             input$Speciesvenn == "Mus musculus") {
      # Mouse
      library("org.Mm.eg.db")
      mypack = list(org.Mm.egALIAS2EG, org.Mm.egSYMBOL)
      return(mypack)
    }
    else if (input$Species == "Danio rerio" ||
             input$Speciesvenn == "Danio rerio") {
      #Zebra fish
      library("org.Dr.eg.db")
      mypack = list(org.Dr.egALIAS2EG, org.Dr.egSYMBOL)
      return(mypack)
    }
    else if (input$Species == "Gallus gallus" ||
             input$Speciesvenn == "Gallus gallus") {
      # chicken
      library("org.Gg.eg.db")
      mypack = list(org.Gg.egALIAS2EG, org.Gg.egSYMBOL)
      return(mypack)
    }
    else if (input$Species == "equCab2" ||
             input$Speciesvenn == "equCab2") {
      # horse
      library("org.Gg.eg.db")
      mypack = org.Mm.egALIAS2EG
      return(mypack)
    }
    else if (input$Species == "Caenorhabditis elegans" ||
             input$Speciesvenn == "Caenorhabditis elegans") {
      # cC elegans
      library("org.Ce.eg.db")
      mypack = list(org.Ce.egALIAS2EG, org.Ce.egSYMBOL)
      return(mypack)
    }
    else if (input$Species == "Rattus norvegicus" ||
             input$Speciesvenn == "Rattus norvegicus") {
      # Rat
      library("org.Rn.eg.db")
      mypack = list(org.Rn.egALIAS2EG, org.Rn.egSYMBOL)
      return(mypack)
    }
    else if (input$Species == "Sus scrofa" ||
             input$Speciesvenn == "Sus scrofa") {
      # Pig
      library("org.Ss.eg.db")
      mypack = list(org.Ss.egALIAS2EG, org.Ss.egSYMBOL)
      return(mypack)
    }
    
  })
  
}

