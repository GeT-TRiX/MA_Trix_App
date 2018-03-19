source("compat.R")
source("formating.R")
source("global.R")

options(shiny.maxRequestSize=40*1024^2)

server <- function(input, output, session) {
  
  csvf1 <- reactive({
    
    inFile <- input$file1
    if(is.null(inFile)){
      return(NULL)
    }
    
    name = inFile$datapath
    iscsv = grep(pattern = '.csv$',name, value = T)
    if(length(iscsv) == 0) return(NULL) ### mettre une alerte pas de fichier csv
    csv = read.csv2(inFile$datapath)

    
    return (csv)
  })
  
  output$plot <- renderPlot({
    plot(cars, type=input$plotType)
  })
  
  output$summary <- renderPrint({
    summary(cars)
  })
  
  output$mytable2 <- DT::renderDataTable({
    DT::datatable(csvf1(), options = list(orderClasses = TRUE))
  })
}



library(markdown)

ui <- navbarPage("Navbar!",
           tabPanel("Plot",
                    sidebarLayout(
                      sidebarPanel(
                        radioButtons("plotType", "Plot type",
                                     c("Scatter"="p", "Line"="l")
                        )
                      ),
                      mainPanel(
                        plotOutput("plot")
                        
                      )
                    )
           ),
           tabPanel("Summary",
                    verbatimTextOutput("summary")
           ),
           navbarMenu("More",
                      
                      tabPanel("Table",
                               headerPanel("Pdb statistics"),
                               helpText(a("Bio3D is an R package containing utilities for the analysis of protein structure, sequence and trajectory data."
                                          , href="http://thegrantlab.org/bio3d/index.php")
                               ),
                               sidebarPanel(
                                 fileInput("file1","Choose pdb file", multiple = F), style="font-size:80%; font-family:Arial; border-color: #2e6da4;background-color: #337ab7")
                               ,
                               h2("PDB Information"),
                               conditionalPanel(
                                 'input.dataset === "atom"',
                                 helpText("Click the column header to sort a column.")),
                               mainPanel(
                                 fluidRow(
                                   tabsetPanel(
                                     id = 'dataset',
                                     tabPanel("atom", DT::dataTableOutput("mytable2"))
                                   )
                                 )
                               )),
                      tabPanel("About",
                               fluidRow(
                                 column(6,
                                        includeMarkdown("about.md")
                                 ),
                                 column(3,
                                        img(class="img-polaroid",
                                            src=paste0("http://upload.wikimedia.org/",
                                                       "wikipedia/commons/9/92/",
                                                       "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                                        tags$small(
                                          "Source: Photographed at the Bay State Antique ",
                                          "Automobile Club's July 10, 2005 show at the ",
                                          "Endicott Estate in Dedham, MA by ",
                                          a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                                            "User:Sfoskett")
                                        )
                                 )
                          )
                      )
           )
)

shinyApp(ui = ui , server = server)

