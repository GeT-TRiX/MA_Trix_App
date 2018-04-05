tabPanel(p(icon("line-chart"), "PCA"),
         useShinyjs(),
         sidebarPanel(
           wellPanel(
             fluidRow(column(
               5,
               selectInput(
                 "dim1",
                 label = h6(gettext("x axis")),
                 choices = list("1" = 1,
                                "2" = 2,
                                "3" = 3),
                 selected = firstdim
                 #width = '80%'
               )
             ),
             column(
               5,
               selectInput(
                 "dim2",
                 label = h6(gettext("y axis")),
                 choices = list(
                   "1" = 1,
                   "2" = 2,
                   "3" = 3,
                   "4" = 4,
                   "5" = 5
                 ),
                 selected = secdim
                 #width = '100%'
               )
             )),
             checkboxInput("label", "Add labbels names", FALSE),
             verbatimTextOutput("valuelab"),
             
             checkboxInput("ellipse", "Add ellipses", FALSE),
             verbatimTextOutput("ellipse")
           ),
           
           
          wellPanel(
          
             shiny::actionButton(
               "toggleAdvancedPCA",
               "Show Advanced Options Setup",
               href = "##",
               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
             ),
             
             br(),
             
             shinyjs::hidden(div(
               id = "advancedPCA",
               
               checkboxInput("jitter", "Avoid overlap between points", FALSE),
               verbatimTextOutput("valued")
                 
               )
               
             ))
         ),
         
         mainPanel(tabsetPanel(
           tabPanel(
             p( "Scree plot"),
             tags$style(
               type = "text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
             )
           ),
           
           tabPanel(
             p("PCA plot"),
             tags$style(
               type = "text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
             )
           )
         ))
         
)