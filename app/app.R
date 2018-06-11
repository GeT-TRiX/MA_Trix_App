library(shiny)
library(magrittr)
library(dplyr)
library(shinyjs)


appCSS <- "
#loading-content {
position: absolute;
background: #182b42;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
top: 30px;
height: 100%;
text-align: center;
color: #FFFFFF;
}
#loading-content-bar {
position: absolute;
background: #182b42;
opacity: 0.9;
z-index: 100;
left: 0;
right: 0;
height: 100%;
text-align: center;
color: #FFFFFF;
}
"

ui <- fluidPage(
  
  inlineCSS(appCSS),
  div(id = "loading-content-bar",
      p()),
  div(
    id = "loading-content",
    br(),
    br(),
    br(),
    h2("Please wait MATRiX app is loading...")),
  
  fluidRow(
    #uncomment in practice
    #tags$style("#link {visibility: hidden;}"),
    tags$script(type="text/javascript", src = "redirect.js"),
    column(3, offset = 4,
           wellPanel(
             h3("app info"),
             tableOutput("app_info")
             )
           )
  ),
  fluidRow(
    column(3, offset = 4,
           wellPanel(
             h3("Redirecting ..."),
             textInput(inputId = "link", label = "", value = "")
             )
           )
  )
)

server <- function(input, output, session) {
  
  hide(
    id = "loading-content",
    anim = F,
    animType = "fade",
    time = 1.5
  )
  hide(
    id = "loading-content-bar",
    anim = F,
    animType = "fade",
    time = 1.5
  )
  
  users <- read.table("/home/franck1337/server/monitor.log", header = TRUE, stringsAsFactors = FALSE)
  app <- data.frame(app = paste0("server/", 1:4), stringsAsFactors = FALSE)
  app <- app %>% left_join(users, by = "app") %>% mutate(app = sub("server/", "", app),
                                                         users = ifelse(is.na(users), "0", as.character(users)))
  link <- paste0("http://147.99.104.189/", app$app[which.min(app$users)])
  
  # info tables
  output$app_info <- renderTable(app)
  
  updateTextInput(session, inputId = "link", value = link)
}

shinyApp(ui = ui, server = server)