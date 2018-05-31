tabPanel(
  p(icon("line-chart"), "PCA"),
  titlePanel("PCA settings"),
  useShinyjs(),
  sidebarPanel(
    
    wellPanel(
      uiOutput("individuselpca"),
      actionButton(
        inputId = "allIndividuspca",
        label = "Select all",
        icon = icon("check-square-o"),
        style =
          "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      actionButton(
        inputId = "noIndividuspca",
        label = "Clear selection",
        icon = icon("square-o"),
        style =
          "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      )
    ),
    
    
    wellPanel(
      fluidRow(column(
        5,
        selectInput(
          "dim1",
          label = h6(gettext("x axis")),
          choices = list(
            "Dim 1" = 1,
            "Dim 2" = 2,
            "Dim 3" = 3,
            "Dim 4" = 4,
            "Dim 5" = 5
          ),
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
            "Dim 1" = 1,
            "Dim 2" = 2,
            "Dim 3" = 3,
            "Dim 4" = 4,
            "Dim 5" = 5
          ),
          selected = secdim
          #width = '100%'
        )
      )),
      fluidRow(column(
        5,
      checkboxInput("label", "Add labbels names", TRUE)),
      column(
        5,
      verbatimTextOutput("valuelab")),
      column(
        5,
      checkboxInput("ellipse", "Add ellipses", FALSE)),
      verbatimTextOutput("ellipse")),
      # ),
      
      
      # wellPanel(
      
      # shiny::actionButton(
      #   "toggleAdvancedPCA",
      #   "Show Advanced Options Setup",
      #   href = "##",
      #   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      # ),
      
      #br(),
      
      # shinyjs::hidden(
      #   div(
      #     id = "advancedPCA",
      
      checkboxInput("jitter", "Avoid overlap between points", FALSE),
      verbatimTextOutput("valued"),
      
      sliderInput(
        "labelsiize",
        "Label size",
        min = 2,
        max = 6,
        value = 3,
        step = 1
      ),
      
      sliderInput(
        "pointsiize",
        "Point size",
        min = 2,
        max = 6,
        value = 2,
        step = 1
      ),
      
      uiOutput('myPanelpca'),

      br()
      
    
      
    )
  ),
  
  mainPanel(tabsetPanel(
  
    tabPanel(p("Scree plot"),
             downloadButton("savescre", "Save your Scree plot" , style =
                              "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
             br(),br(),br(), 
             plotOutput(outputId = "eigpca")),
    tabPanel(
      p("PCA plot"),
      tags$style(
        type = "text/css",
       # ".shiny-output-error { visibility: hidden; }",
        ".shiny-output-error:before { visibility: hidden; }"
      ),
      downloadButton("savepca", "Save your PCA" , style =
                       "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(),br(),br(),
      plotOutput(outputId = "PCA")
    )
  ))
)