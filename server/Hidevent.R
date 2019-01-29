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
addTooltip(session,id = "savehm", title = "For download larger files, you must use Chrome!", placement = "down", trigger = "hover")
addTooltip(session,id = "savegohmdavxlsx", title = "For download larger files, you must use Chrome!", placement = "down", trigger="hover")

shinyjs::onclick("toggleAdvanced",
                 shinyjs::toggle(id = "advanced", anim = TRUE)) ## hide and show event


shinyjs::onclick("toggleAdvancedcolors",
                 shinyjs::toggle(id = "advancedcol", anim = TRUE))

shinyjs::onclick("toggleAdvancedJvenn",
                 shinyjs::toggle(id = "advancedjvenn", anim = TRUE))
