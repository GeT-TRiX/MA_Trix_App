# Venncluster <- reactive({
#   #mydef goes here
#   
#   davidqueryvenn(vennfinal()$GeneName, )
#   
# })
# 
# observe({
#   req(Venncluster())
#   # retrieve the number of clusters
#   updateSliderInput(session, "clusterNumber", max = nrow(summary(Venncluster())))
# })
# 
# output$clusterPlot <- renderPlot({
#   req(Venncluster())
#   # plot the input$clusterNumber(th) cluster
#   plot2D(Venncluster(), input$clusterNumber)
# })
# 
# output$debug <- renderPrint({
#   req(Venncluster())
#   summary(Venncluster())
# })
