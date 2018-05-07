library(shiny)
library(magrittr)
library(dplyr)

ui <- fluidPage(
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