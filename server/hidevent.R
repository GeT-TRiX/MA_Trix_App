###############################
######## Hide buttons         #
###############################


shinyjs::onclick("toggleAdvanced",
                 shinyjs::toggle(id = "advanced", anim = TRUE)) ## hide and show event

# shinyjs::onclick("toggleAdvancedPCA",
#                  shinyjs::toggle(id = "advancedPCA", anim = TRUE))

shinyjs::onclick("toggleAdvancedcolors",
                 shinyjs::toggle(id = "advancedcol", anim = TRUE))

