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
  csvf <- callModule(csvFile, "datafile",stringsAsFactors = FALSE) # TODO Module for importing data
  source(file.path("server", "Groupstripshiny.R"), local = TRUE)$value # Utilities method (packages citations, fc step, zipdownload and panel redirection and project name)
  
  
  ##########################################
  ######## Widget update and info         ##
  ##########################################

  source(file.path("server", "Utilities.R"), local = TRUE)$value # Utilities method (packages citations, fc step, zipdownload and panel redirection and project name)

  ##########################################
  ######## HOME page                      ##
  ##########################################

  source(file.path("server", "Datasummary.R"), local = TRUE)$value # Reactive function that return the indexes for the signficant genes
  source(file.path("server", "Rendertable.R"), local = TRUE)$value # TODO All the output csv except for the heatmap page that are in heatmapshiny source
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

  source(file.path("server", "Venn.R"), local = TRUE)$value # Generate the vennlist and select the contrasts
  #source(file.path("server", "Vennrender.R"), local = TRUE)$value # TODO add static Venn
  source(file.path("server", "Venninter.R"), local = TRUE)$value # Selected intersection Venn mean and barplot from the data table with the export. TODO remove not valuable info
  source(file.path("server", "Trackervenn.R"), local = TRUE)$value # Tracker for Venn

  ################################
  ######## Jvenn                ##
  ################################

  source(file.path("server", "Jvenn.R"), local = TRUE)$value # Jvenn reactive expression

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

})
