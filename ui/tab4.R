tabPanel(
  p(icon("line-chart"), "Venn Diagram"),
  titlePanel("Venn diagram settings"),
  sidebarPanel(
    wellPanel(
      uiOutput("contout")
      ,
      actionButton(
        inputId = "allCont",
        label = "Select all",
        icon = icon("check-square-o"),
        style =
          "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
      ,
      actionButton(
        inputId = "noCont",
        label = "Clear selection",
        icon = icon("square-o"),
        style =
          "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      br(),br(),
      fluidRow(
        column(5,
      selectInput("regulation", #  Create a select list that can be used to choose a single or multiple items from a list of values.
                  "Choose your regulation",
                  choices = c("both","up", "down"))),
      column(5,
      selectInput(
        "formven",
        "Choose your file format",
        choices = c("png", "eps", "pdf"))
      )),
      
      
      br(),
      sliderInput(
        "pvalvenn",
        "P-value treshold",
        min = 0.01,
        max = 0.05,
        value = 0.05,
        step = 0.01
      ),
      br(),
      sliderInput(
        "fcvenn",
        "FC treshold",
        min = 1,
        max = 10,
        value = 1,
        step = 1
      ),
      
      sliderInput(
        "vennsize",
        "Size of the police",
        min = 0.3,
        max = 2,
        value = 1,
        step = 0.1
      ),
      
      br()
      # selectInput(
      #   "formven",
      #   "Choose your file format",
      #   choices = c("png", "eps", "emf")
      # )
    )
  ),
  
  mainPanel(
    bsAlert("alert"),
    
    div(style="display:inline-block",
        
        # shiny::actionButton("vennd", "Print Venn diagram", style =
        #                       "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        downloadButton('downloadvenn', "Download the data",
                       style =
                         "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
        downloadButton("savevenn", "Save your plot" , style =
                         "color: #fff; background-color: #337ab7; border-color: #2e6da4")
       
        
        
    ),
    
    br(),br(),br(),
    conditionalPanel(condition = '!output.bool',
                     uiOutput(outputId = "image")
                     , uiOutput("sorry")),
    
    plotOutput(outputId = "myVenn")
    
  )
  
)