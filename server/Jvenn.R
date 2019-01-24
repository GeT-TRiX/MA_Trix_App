# observe({
# 
#   req(csvf(),vennlist(),user_cont())
  
  # wrongcol <- function(y)
  #   if (any(grepl("col2rgb", y)))
  #     invokeRestart("muffleWarning")
  # 
  # if(input$dispvenn == "genes")
  #   if(input$Notanno){
  #     vennlist <- lapply(vennlist()[[2]], grep, pattern="^chr[A-z0-9]{1,}:|^ENSMUST|^LOC[0-9]{1,}|^[0-9]{4,}$|^A_[0-9]{2}_P|^NAP[0-9]{4,}|[0-9]{7,}", value=TRUE, invert=TRUE)
  #     Rtojs <- toJvenn(vennlist,user_cont())
  #   }
  #   else
  #     Rtojs <- toJvenn(vennlist()[[2]],user_cont())
  # else
  #   Rtojs <- toJvenn(vennlist()[[1]],user_cont())

  # Mymode <-  input$updamod # Mode
  # Myfont <-  input$myfont # Font size
  # Mystat <-  input$mystat # Stat
  # Myswitch <-  input$dispswitch # Stat
  # 
  # col2js =  tryCatch({
  #   col2rgb(mycol()) %>%  lapply(.,function(x)return(x)) %>% withCallingHandlers(error = wrongcol)
  # }, error = function(e) {shinyjs::alert("Wrong color")})
  # 
  # 
  # session$sendCustomMessage(type="updatejvenn", Rtojs)
  # session$sendCustomMessage(type="updatejcol", col2js)

#})



# jvennc_input <- reactive({
#   input$fill
# })
# 
# jvenncol <- shiny::debounce(jvennc_input, 500)
# 
# mycol <- reactive({
#   if(!jvenncol() == ""){
# 
#     mycol = gsub("^\\s+|\\s+$", "", unlist(strsplit(jvenncol(), ",")))
#   }
#   else
#     mycol = ""
# })
