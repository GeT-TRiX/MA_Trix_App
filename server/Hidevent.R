### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/GeT-TRiX/MA_Trix_App/
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Mining and functional Analysis of TRanscriptomics data
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
