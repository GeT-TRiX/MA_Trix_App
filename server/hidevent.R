###############################
######## Hide buttons         #
###############################


shinyjs::onclick("toggleAdvanced",
                 shinyjs::toggle(id = "advanced", anim = TRUE)) ## hide and show event


shinyjs::onclick("toggleAdvancedcolors",
                 shinyjs::toggle(id = "advancedcol", anim = TRUE))

# shinyjs::onclick("toggleAdvancedgo",
#                  shinyjs::toggle(id = "advancedgo", anim = TRUE))