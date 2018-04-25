library(shiny)
fruits <- c("banana","raccoon","duck","grapefruit")

ui <- pageWithSidebar(
  # Application title
  headerPanel("Hello Shiny!"),
  # Sidebar with a slider input
  sidebarPanel(
    tabsetPanel(
      tabPanel("aZ", uiOutput("aToZPlayerList"), htmlOutput("List")),
      tabPanel("byTeam", uiOutput("byTeamPlayerList"))
    )),
  # Show a plot of the generated distribution
  mainPanel()
)

server <- function(input,output){
  
  output$aToZPlayerList <- renderUI({
    selectInput("alphabet", "Players A-Z", choices=c("A","B","C"), selected="A")
  })
  
  output$List <- renderUI({
    HTML(paste(fruits))
  })
  
  output$byTeamPlayerList <- renderUI({
    selectInput("team", "Teams", choices=c("A","B","C"), selected="B")})
}

runApp(list(ui=ui,server=server))