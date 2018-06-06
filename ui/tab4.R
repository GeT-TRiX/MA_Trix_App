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
      
      
      br(),br(),
      fluidRow( column(5,
      sliderInput(
        "pvalvenn",
        "P-value treshold",
        min = 0.01,
        max = 0.05,
        value = 0.05,
        step = 0.01
      )),
      #br(),
      column(5,
      sliderInput(
        "fcvenn",
        "FC treshold",
        min = 1,
        max = 10,
        value = 1,
        step = 1
      ))),
      br(),br(),
      
      fluidRow( column(5,
      sliderInput(
        "vennsize",
        "Size of the police",
        min = 0.3,
        max = 2,
        value = 1,
        step = 0.1
      )),
      
      #br(),
      column(5,
      uiOutput("myselvenn"))
    ),
    uiOutput("topgenesvenn")
    
  )),
  
  mainPanel(tabsetPanel(id = "mainvenntabset",
    tabPanel("Visualize the Venn diagram",
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
    
  ),
  tabPanel("Visualize the intersection table",
           downloadButton('downloadvennset', "Download the filtered data",
                          style =
                            "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
           h3("Table showing the gene names for the intersection(s) selected"),
           helpText(
             "You can directly filtered the table by fold change and save the output table"
           ),
           
           DT::dataTableOutput("vennresinter"),
           br(),
           h1("Here's a tracker for your different selections:"),
           #br(),
           wellPanel(
             tags$head(
               tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # add style.css in order to add better police
             ),
             
             tags$head(tags$style("
                                  #container * {
                                  display: inline;
                                  }")),
        
             div(
               id = "container",
               p("You have chosen the following comparisons"),
               htmlOutput("contvenn"),
               p("for a total of"),
               htmlOutput("totalgenes"),
               p("genes  with a P-value and FC treshold respectively set to "),
               htmlOutput("myPVALvenn"),
               p("and"),
               htmlOutput("myFCvenn")
               
             ),
             div(
               id = "container",
               p("There are"),
               htmlOutput("venngenes"),
               p("significant genes"),
               p("for this interaction"),
               htmlOutput("continter"),
               p("if you click on the top DE genes button you will plot the top"),
               htmlOutput("topgenesdf"),
               p("rows the of the previous table")
             )),
           div(style="display:inline-block",
           actionButton(
             inputId = "topdegenes",
             label = "Plot top DE genes",
             style =
               "color: #fff; background-color: #337ab7; border-color: #2e6da4"
           ),
           downloadButton("savebarplot", "Save your bar plot" , style =
                            "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
           br(),
           plotOutput(outputId ="barplotvenn")
           
  )))
  #plotOutput(outputId ="barplotvenn")
)