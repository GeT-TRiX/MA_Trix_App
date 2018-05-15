# observeEvent(input$heatm, {
# 
#   
#   output$distPlot <- renderPlot({
#     isolate({
#       if (!is.null(formated()))
#         withProgress(message = 'Plotting heatmap:', # Add sliderbar when loading heatmap
#                      value = 0,
#                      {
#                        n <- NROW(formated()) #number of row in the formated dataframe
#                        
#                        for (i in 1:n) {
#                          incProgress(1 / n, detail = "Please wait...")
#                        }
#                        hmbis()
#                        #hm()
#                        heatmapfinal(isplot = F)
#                      })
#     })
#   }, width = 900 , height = 1200, res = 100)
#   
#   
#   output$save <- downloadHandler(filename <- function() {
#     
#       paste0(basename(file_path_sans_ext("myfile")),
#              '_heatmap.',
#              input$form,
#              sep = '')
#   },
#   content <- function(file) {
#       if (input$form == "emf")
#         
#         emf(
#           file,
#           width = 7,
#           height = 7,
#           pointsize = 12,
#           coordDPI = 300
#         )
#       
#       else if (input$form == "png")
#         png(
#           file,
#           width = 900,
#           height = 1200,
#           units = "px",
#           pointsize = 12,
#           res = 100
#         )
#       else
#         eps(file,
#             width = 7,
#             height = 7)
#     
#       if (!is.null(formated()))
#         withProgress(message = 'Plotting heatmap:',
#                      value = 0,
#                      {
#                        n <- NROW(formated())
#                        
#                        for (i in 1:n) {
#                          incProgress(1 / n, detail = "Please wait...")
#                        }
#                        #hmbis()
#                        heatmapfinal(isplot = F)
# 
#                      })
#       
#       dev.off()
#     
#     
#   })
# })