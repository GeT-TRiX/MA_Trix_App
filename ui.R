  ###############################
  ######## dashboardsidebar     #
  ###############################  
  
  #182b42
  #182b42
  #1D4D68;

sidebar <- dashboardSidebar( # analyse par microréseau de l'impact transcriptomique des xénobiotiques
  useShinyjs(),
  inlineCSS(appCSS),#background #EFEFEF
  
  tags$style(type="text/css", Errorcss),
  tags$style(type="text/css", inactivity),
  
  
tags$head(
     #tags$script(src = inactivity),   
     tags$script(src = "custom.js")),
  div(id = "loading-content-bar",
      p()),
  div(
    id = "matrixapp",
    sidebarMenu(id = "side",
                
      menuItem("Upload Data", tabName = "Home", icon = icon("upload")),
      menuItem("PCA", tabName = "PCA", icon = icon("line-chart")),
      menuItem("Venn diagram", tabName = "Venn", icon = icon("line-chart")),
      menuItem("Heatmap", tabName = "Heatmap", icon = icon("line-chart")),
      menuItem("About", tabName = "About", icon = icon("info-circle")),
      #bookmarkButton(),
      menuItemOutput("dymMenu"),
      collapsed = TRUE,

      img(
        src = "GeT_logo-RVB.png",
        height = 50,
        width = 180,
        style = "position:absolute;bottom:50px;margin:0 0 15px 10px;"
      ),
      img(
        src = "Logotype-INRA-transparent.png",
        height = 43,
        width = 168,
        style = "position:absolute;bottom:0;margin:0 0 15px 10px;"
      )    
      
      )
  )
)

  ###############################
  ######## dashboardbody        #
  ###############################  
  ###############################
  ######## Upload Page          #
  ############################### 

body <- dashboardBody(
  tags$style(type="text/css", inactivity),
  tags$style(type="text/css", Errorcss),
  
  #tags$head(tags$style(HTML("div.col-sm-10 {padding:1px}"))),
  #tags$head(tags$style(HTML("div.col-sm-2 {padding:0px}"))),
  useShinyjs(),
  extendShinyjs(text = 'shinyjs.hideSidebar = function(params) { $("body").addClass("sidebar-collapse"); 
              $(window).trigger("resize"); }'),
  extendShinyjs(text='shinyjs.showSidebar = function(params) { $("body").removeClass("sidebar-collapse"); 
              $(window).trigger("resize"); }'),
  
  tags$style(HTML("
    .tabbable > .nav > li > a[data-value='hmpan'] {background-color: red;   color:white}
    .tabbable > .nav > li > a[data-value='cutpan'] {background-color: blue;  color:white}
  ")),
  
  
  #tags$style(type="text/css", Errorcss),
  useToastr(),
  
  inlineCSS(appCSS),
  
  includeCSS("./css/style.css"),
  div(
    id = "loading-content",
    br(),
    br(),
    br(),
    h2("Please wait while MATRiX is loading...")),
  div(
    id = "matrixapp",
  tabItems(
    tabItem(tabName = "Home",
            bsAlert("alert"),
            tags$style(type='text/css', ".well { max-width: 2em; }"),
            fluidRow(
              column(width=9,
                div( style = "width:100% ; max-width: 1200px; height: 1050px",
               
              #tabsetPanel( 
              #tabBox(title="Welcome to MATRiX", id="tabset1", width=NULL,
                   # tags$script(type="text/javascript", language="javascript", src="google-analytics.js"),
                   #tabPanel("About", 
                            conditionalPanel(condition = 'output.boolmark', #Hide or Show event depending on the loading data success or failure
                                             includeMarkdown("markdown/help.md")),
                            conditionalPanel(condition = '!output.boolmark',
                            
                            textOutput("myFileName"),
                            
                            column(
                              #Create a column for use within a fluidRow or fixedRow
                              12, h3(
                                "This table summarize the number of significant genes depending on the p-value treshold choosen with the slider bar"
                              ),
                              helpText("Choose your p-value treshold to modify the following data table")
                              ,sliderInput(
                                #Constructs a slider widget to select a numeric value from a range.
                                "pval1",
                                "",
                                min = 0.01,
                                max = 0.05,
                                value = 0.05,
                                step = 0.01,
                                width = "500"
                              ),
                              dataTableOutput("data_summary") # render a renderTable or renderDataTable within an application page
                            ),
                            
                            column(12,
                              h3("This table shows the samples with the corresponding groups"),
                              dataTableOutput("new_test")
                            )
                            # ,
                            # column(12,
                            #   h3("This table shows the head of the working set"),
                            #   dataTableOutput("new_data")
                            # )
                            
                            # column(12,
                            #   h3("Show the actual data frame with the columns selected"),
                            #   helpText(
                            #     "Warning according to the number of NA for a given parameter, the analysis should be strongly biased"
                            #   ),
                            #   dataTableOutput("new_group")
                            # )
                            # )
                   )
              #)
              )
            ),  
            div(id="pass",style = "word-wrap: break-word;",
            column(
            width = 3,
            box(id="boxpass",title = strong("Upload data", style="font-size:25px;"), width = NULL, background = "light-blue",
                inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
              #shinydashboard::box(
              # sidebarPanel(
              #   id = "sideloadpan",
              #   title = "Upload widget",
              #   width = NULL,
              #   status = "primary",
                #div(style = 'overflow-y: scroll; height: 550px',
                #sidebarPanel( #Create a rectangular region containing the different widgets
                # style = " font-size:100%; font-family:Arial;
                # border-color: #2e6da4; background-color: #337ab7, width: 28px; ", #CSS attributes for the sidebarPanel
                # width = 3,
                
                downloadLink("downloadData", label = "download sample data", style="color:white; float:right;"),
                br(),
                br(),
                #csvFileInput("file", "Choose your csv files"),
                fileInput(
                  # browse button (UI)
                  "file",
                  "Choose your csv files",
                  accept = c(
                    "text/csv",
                    #accept only csv and text files
                    "text/comma-separated-values,text/plain",
                    ".csv"
                  ),
                  multiple = T # Attribute to load multiple data at once
                ),
                
                br(),
                # white space (<br> </br> basic HTML)
                
                # selectInput("method2", "Choose your matrix distance:", selected = "FDR",
                #             c("adj.p.val(FDR)" = "FDR", "p.value(raw)" = "None" )),
                
                
                selectInput(
                  "method",
                  #  Create a select list that can be used to choose a single or multiple items from a list of values.
                  "Choose your statistical method",
                  choices = c("adj.p.val (FDR)" = "FDR", "p.value (raw)" = "None")
                )
                
              )
            )
            ),
            conditionalPanel(condition = '!output.boolmark',
            column(12,
                    h3("Show the actual data frame with the columns selected"),
                    helpText(
                      "Warning according to the number of NA for a given parameter, the analysis should be strongly biased"
                    ),
                    dataTableOutput("new_group")
            ))
          
            
            )),
    
  ###############################
  ######## PCA page             #
  ############################### 
    
    tabItem(tabName = "PCA",
            tags$style(type='text/css', ".well { max-width: 20em; }"),
            fluidRow(column(
              width = 9,
              div(
                style = "width:100% ; max-width: 1200px; height: 1050px",
                tabsetPanel(
                  #title = "Principal component analysis",
                  #id = "tabset1",
                  #width = NULL,
                  tabPanel(
                    strong("Scree plot"),
                    downloadButton("savescre", "Save your Scree plot" , style =
                                     "color: #fff; background-color: #337ab7; border-color: #2e6da4"),

                    br(),br(),
                    plotOutput(outputId = "eigpca", height = 700)
                  ),
                  tabPanel(
                    strong("PCA plot"),
                    tags$style(type = "text/css",
                               # ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"),
                    
                    
                    div(style="display:inline-block;",
                        fluidRow( column(1 ,
                                         downloadButton("savepca", "Save your PCA" , style =
                                                          "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                  column(2),
                                  column( 3, 
                                          selectInput(
                                            "formpca",label = NULL,
                                            choices = c("png", "eps", "pdf")))
                                  
                        )),
                    
                    
                    
                    # downloadButton("savepca", "Save your PCA" , style =
                    #                  "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    # selectInput(
                    #   "formpca",
                    #   "",
                    #   choices = c("png", "eps", "pdf")),
                    
                    br(),br(),
                    plotOutput(outputId = "PCA", height = 800)
                  )
                  
                )
              )
            ),
            # column(
            #   width = 3,
            #   sidebarPanel(
            div(id="pass",style = "word-wrap: break-word;",
                column(width=3,
                       box(id="boxpasspca",title = strong("PCA settings",style="font-size:25px;"), width = NULL, background = "light-blue",
                           inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
                           strong("Choose your group to visualize"),
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
                  ),
                
                
                  fluidRow(column(
                    6,
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
                    6,
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
                  fluidRow(
                    column(4,
                           checkboxInput("label", "Add labbels names", TRUE)),
                    column(4,
                           checkboxInput("meanpoint", "Add mean points", TRUE)),
                    column(4,
                           checkboxInput("ellipse", "Add ellipses", FALSE))#,
                  ),

                  fluidRow(column(3),
                  column(8,checkboxInput("jitter", "Avoid overlap between points", FALSE))),
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
            #      )
            ))))), 
  ###############################
  ######## Venn Page            #
  ############################### 
    tabItem(tabName = "Venn",
            tags$style(type='text/css', ".well { max-width: 25em; }"),
            tags$style(type='text/css', ".well { max-height: 50em; }"),
            fluidRow(column(
              width = 9,
              div(
                style = "width:100% ; max-width: 1500px; height: 1500px max-height: 2200px;",
                tabsetPanel(
                # tabBox(
                #   title = "Venn diagramm",
                #   id = "tabset1",
                #   width = NULL,
                  tabPanel(
                    strong("Visualize the Venn diagram"),
                  
                    div(style="display:inline-block",
                        fluidRow(column(4, style= "width:24%;",

                                        downloadButton('downloadvenn', "Download the data",
                                                       style =
                                                         "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                 column(4, style="width:20%;",
                                        downloadButton("savevenn", "Save your plot" , style =
                                                         "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                     column(4,style="width:19%;", selectInput(
                                       "formven",label = NULL,
                                       choices = c("png", "eps", "pdf")))
                        )),
                    
                    
                    br(),br(),
                    conditionalPanel(condition = '!output.bool',
                                     uiOutput(outputId = "image")
                                     , uiOutput("sorry")),
                    div(id="vennerro" , style= "font size: 20px;",
                    plotOutput(outputId = "myVenn", height = 800)
                    )
                  ),
                  tabPanel(
                    strong("Visualize the intersection table"),
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
                    #wellPanel(
                      #column(width=9,
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
                      ),
                      #)),
                    
                    br(),br(),
                    #div.col-sm-4 {padding:0px};
                    #fluidRow(column(4),column(3,strong("top genes"))),
                    div(style="display:inline-block",
                        fluidRow(column(4,br(),style= "width:14%;",
                                        actionButton(
                                          inputId = "topdegenes",
                                          label = "Plot top DE genes",
                                          style =
                                            "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                        )),
                                 column(3, style= "width:15.5%;",br(),
                                        
                                        downloadButton("savebarplot", "Save your barplot" , style =
                                                         "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                 column(2 ,br(),style= "width:15.5%; , padding: 0%;",
                                 selectInput( "formvenbar",label = NULL,
                                   choices = c("png", "eps", "pdf"))),
                                  column(2,style= "width:12%; padding: 0%;", uiOutput("topgenesvenn", style= "padding: 0px;"))
                        )),
                    
                    
                 
                    br(),
                    plotOutput(outputId ="barplotvenn", height = 700)
                  )
                 
                 , tabPanel(strong("Venn GO enrichment"),


                          plotOutput("clusterPlot"),
                          verbatimTextOutput("debug")
                 )
                          
                  
                )
              )
            ),
            div(id="pass",style = "word-wrap: break-word;",
                column(width=3,
                       box(id="boxpassvenn",title = strong("Venn settings", style ="font-size:25px;"), width = NULL, background = "light-blue",
                           inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
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
                           
                           selectInput(
                             "methodforvenn",
                             "Choose your statistical method",
                             choices = c("adj.p.val (FDR)"= "FDR", "p.value (raw)" = "None")
                           ),
                           
                           #fluidRow(
                                    selectInput("regulation", #  Create a select list that can be used to choose a single or multiple items from a list of values.
                                                "Choose your regulation",
                                                choices = c("both","up", "down")),
                             # column(6,
                             #        selectInput(
                             #          "formven",
                             #          "Choose your file format",
                             #          choices = c("png", "eps", "pdf"))
                             # ),
                           #),
                           
                           
                           br(),br(),
                           fluidRow( column(6,
                                            sliderInput(
                                              "pvalvenn",
                                              "P-value treshold",
                                              min = 0.01,
                                              max = 0.05,
                                              value = 0.05,
                                              step = 0.01
                                            )),
                                     #br(),
                                     column(6,
                                            sliderInput(
                                              "fcvenn",
                                              "FC treshold",
                                              min = 1,
                                              max = 10,
                                              value = 1,
                                              step = 1
                                            ))),
                           br(),br(),
                           
                           sliderInput(
                             "vennsize",
                             "Size of the police",
                             min = 0.3,
                             max = 2,
                             value = 1,
                             step = 0.1
                           ),
                           
                           br(),
                           uiOutput("myselvenn"),
                      # uiOutput("topgenesvenn"),
                      
                      strong("Functional Annotation Clustering",style = "font-family: 'times'; font-size:20px; font-style: strong; "),
                      
                      br(),br(),
                      fluidRow(column(8, br(),
                              checkboxInput("meandup",
                                            "Compute the mean for the same duplicated genes",
                                            FALSE)),
                      column(4,
                      selectInput("Speciesvenn", "Choose your Species:", selected = "Mus musculus", 
                                  c("Mouse" = "Mus musculus", "Human" = "Homo sapiens", "Rat" = "Rattus norvegicus", "C. elegans" = "Caenorhabditis elegans",
                                    "Zebrafish" = "Danio rerio",  "Pig" = "Sus scrofa", 
                                    "Chicken" = "Gallus gallus", "Chimpanzee" = " Pan troglodytes" )))),
                      
                      fluidRow(column(8,
                      sliderInput(
                        "clusterNumber",
                        label = "Cluster",
                        value = 1,
                        min = 1,
                        max = 5
                      )), 
                      column(4,br(), br(),
                      actionButton("GOvenn", "Run GO",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                      
                       ))))),
    #id="rawdatatab",
  ###############################
  ######## Heatmap Page         #
  ###############################
  
  
    #tags$style(type='text/css', ".well { max-width: 15em; }"),
    # tags$head(tags$style(HTML(InfoBoxCSS))),
    #tags$head(tags$script("$(function() { $(\"[data-toggle='popover']\").popover(); })")),

  
    tabItem(tabName = "Heatmap",
            tags$style(type='text/css', ".well { max-width: 25em; }"),
          
            
            fluidRow(column(
              width = 8,
              div(
                #style = "width:100% ; max-width: 1200px; height: 1300px; max-height: 1800px;",
                #tabBox(
                tabsetPanel(
                  id = "mainhmtabset",
                  #title = "Heatmap and GO enrichment",
                  #id = "tabset1",
                  #width = NULL,
                  tabPanel(
                     strong("Visualize the Heatmap"),value = "hmmainpan",
                    #"Visualize the Heatmap",
                    div(style="display:inline-block",
                        fluidRow( column(1 ,
                                         downloadButton(
                                           "savehm",
                                           "Save your plot" ,
                                           style =  "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                         )),
                                  column(2),
                                  column( 3, 
                                          selectInput("formhm", label = NULL,
                                                      choices = c("png", "eps", "emf")))
                                  
                        )),
                        
                        conditionalPanel(condition = '!output.heatmbool',  verbatimTextOutput("warningsheat")
                        ),
                        #plotOutput("warningsheat")
                        conditionalPanel(condition = 'output.heatmbool',
                                         plotOutput("distPlot", width = "85%" , height = 1200)
                                         
                                         ### Adding white spaces between the heatmap plot and the tracker
                                         
                                         # br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                         # br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                         # br(),br(),br(),br(),br(),br(),br(),br(),br(),
                                         # br(),br(),br(),br(),br(),br(),br()
                                         ),
                        
                        h1("Here's a tracker for your different selections:"),
                        #br(),
                        #wellPanel(

                          tags$head(
                            tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # add style.css in order to add better police
                          ),
                          
                          tags$head(tags$style("
                             #container * {
                             display: inline;
                             }")),
                          
                          
                          div(
                            id = "container",
                            p("There are"),
                            htmlOutput("myNUM"),
                            p("significant genes"),
                            p("for the following comparison(s)"),
                            htmlOutput("testtt")
                          ),
                          #br(),
                          div(
                            id = "container",
                            p('The selected rows for your heatmap are based on the '),
                            textOutput("myMET"),
                            p("method, with a P-value and FC treshold respectively set to "),
                            textOutput("myPVAL"),
                            p('and'),
                            textOutput("myFC")
                          ),
                          #br(),
                          div(
                            id = "container",
                            p('The'),
                            textOutput("myMAT"),
                            p(
                              "method was used to compute the matrix distance with a number of clusters for the significant genes equal to",
                              textOutput("myCLUST")
                            )
                          ),
                          div(
                            id = "container",
                            p('The advanced color settings choosen for the following groups :'),
                            textOutput("indivcol"),
                            p("are respectively correlated to the following colors"),
                            htmlOutput("myPAL")
                          ),
                          div(
                            id = "container",
                            p(
                              'The legend size, row size, col size are respectively equals to ',
                              textOutput("myLEG"),
                              p(','),
                              textOutput("myROW"),
                              p(','),
                              textOutput("myCOL")
                            )
                          )
                       # )
                  ),
                  tabPanel(
                    strong("Heatmap clusters"),
                    value = "dfhmclu",
                    downloadButton('downloadcut', "Download the data",
                                   style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    column(
                      12,
                      
                      h3("Table summarizing the heatmap"),
                      helpText(
                        "Heatmap's cluster are upside down in order to match the genes with the heatmap from top to bottom"
                      )
                      ,
                      dataTableOutput("clustering"),
                      
                      
                      h3("This table summarize the number of significant probes and genes by cluster"),
                      helpText(
                        "For the number of genes by cluster the duplicated genes are removed"
                      ),
                      dataTableOutput("totalgenbyc")
                    )),
                  tabPanel(
                    strong("(GO) enrichment-based cluster analysis"),value="maingo",
                    downloadButton("savegohmdav", "Save your enrichment clusters" , style =
                                     "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                    DT::dataTableOutput("davidgo"), 
                    verbatimTextOutput("printmessage"),
                    verbatimTextOutput("printselected")
                    
                    
                    #,
                    #verbatimTextOutput("clustgo")
                  ),
                  tabPanel
                  (
                    strong("Cut heatmap"),#icon("table"), 
                    bsAlert("alert"),value = "cuthmmainpan",
                    plotlyOutput(outputId = "cutheatmap", height = 600),
                    
                    
                    br(),br(),br(),br(),br(),br(),br(),br(),br(),
                    
                    br(),br(),br(),br(),br(),br(),br(),br(),br(),
                    br(),br()
                    
                    #verbatimTextOutput("event")
                    )
                  )
            )
            ),
           # div(id="pass",style = "word-wrap: break-word;",
                column(width=4,
  
                       # box(id="boxpassvenn",title = strong("Heatmap settings"), width = NULL, background = "light-blue",
                       #      inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
                       
                       div(id="mypanheat", style="color: white;",
                      tabBox(
                         
                         title = "",
                         id = "tabset25",
                         width = NULL, 
                        
                         #style="color:#F0B2AD;",

                        #div( id="testingu",
                           tabPanel("Heatmap",id= "heatmpan", ##ADD8E6
                             style="background-color: #3c8dbc;",
                             # box(id="boxpassvenn",title = strong("Heatmap settings"), width = NULL, background = "light-blue",
                             #     inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
                             value="hmpan",
                          strong("Heatmap settings", style="font-size:25px;") ,
                          br(),
                             
                           actionLink("resetAll",  label = ("reset all"), style="color:White;float:right;"),
                           br(),
                           #wellPanel(
                             uiOutput("individusel"),
                             actionButton(
                               inputId = "allIndividus",
                               label = "Select all",
                               icon = icon("check-square-o"),
                               style =
                                 "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             ),
                             actionButton(
                               inputId = "noIndividus",
                               label = "Clear selection",
                               icon = icon("square-o"),
                               style =
                                 "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             ),
                           #),
                           div(
                             id = "form",
                             
                             #wellPanel(
                               # Creates a panel with a slightly inset border and grey background
                               uiOutput("testout"),
                               actionButton(
                                 # Action button that automatically react when triggered
                                 inputId = "allTests",
                                 label = "Select all",
                                 icon = icon("check-square-o"),
                                 style =
                                   "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                               ),
                               actionButton(
                                 inputId = "noTests",
                                 label = "Clear selection",
                                 icon = icon("square-o"),
                                 style =
                                   "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                               ),
                               #,
                               #
                               # p("You've selectionned the following test : "),
                               # hr(),
                               # verbatimTextOutput("test")
                               
                             #),
                             br(),
                             br(),
                             fluidRow( column(6,
                                              numericInput(
                                                # Create an input control for entry of numeric values
                                                'maxgen',
                                                'Maximal number of genes by comparison(s)',
                                                NULL,
                                                min = 100,
                                                max = 1500
                                              )),column(6,
                                                        br(),
                                                        selectInput(
                                                          "method2",
                                                          "Choose your statistical method",
                                                          choices = c("adj.p.val (FDR)"= "FDR", "p.value (raw)" = "None")
                                                        ))),
                             
                             br(),
                             fluidRow( column(6,
                             sliderInput(
                               "pval",
                               "P-value treshold",
                               min = 0.01,
                               max = 0.05,
                               value = 0.05,
                               step = 0.01
                             )),
                             column(6,
                             sliderInput(
                               "fc",
                               "FC treshold",
                               min = 1,
                               max = 10,
                               value = 1,
                               step = 1
                             ))),
                             
                             # sliderInput(
                             #   "cutheatm",
                             #   "Choose where you cut the heatmap",
                             #   min = 1,
                             #   max = 15,
                             #   value = 2,
                             #   step = 0.5
                             # ),
                             
                             br(),
                             
                             # selectInput("method2", "Choose your matrix distance:", selected = "FDR", 
                             #             c("adj.p.val(FDR)" = "FDR", "p.value(raw)" = "None" )),
                             
                             div(id = 'center', strong("Advanced settings",style = "font-family: 'times'; font-size:20px; font-style: strong; ")),
                             br(),
                             shiny::actionButton(
                               "toggleAdvanced",
                               "Advanced Computational Options",
                               href = "#",
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             ),
                             br(),
                             shinyjs::hidden(div(
                               # Hide some widgets between the tags
                               id = "advanced",
                               #wellPanel(
                                 fluidRow(
                                   column(6,
                                          numericInput('clusters', 'Cluster count', 3,
                                                       min = 1, max = 15)),
                                   #br(),
                                   column(6,
                                          selectInput(
                                            "dist",
                                            "Choose your matrix distance",
                                            choices = c("correlation", "euclidian"))
                                   )),
                                 fluidRow(
                                   column(6,
                                          checkboxInput("meangrp",
                                                        "Compute the mean for the different groups",
                                                        FALSE)),
                                   #verbatimTextOutput("value"),
                                   column(6,
                                          checkboxInput("scalcol",
                                                        "Apply scaling to columns",
                                                        FALSE))
                                   
                                 )))
                             ,br(),
                             shiny::actionButton(
                               "toggleAdvancedcolors",
                               "Advanced graphical Settings",
                               href = "#",
                               style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             ),
                             
                             br(),
                             
                             shinyjs::hidden(div(
                               id = "advancedcol",
                              # wellPanel(
                                 fluidRow(
                                   column(6,
                                          colourpicker::colourInput(
                                            # Creation of a color button
                                            "col1",
                                            "Select colour for downregulated genes",
                                            firstcol,
                                            palette = "limited"
                                          )
                                   ),
                                   column(
                                     6,
                                     colourpicker::colourInput("col3", "Select colour for upregulated genes", lastcol,
                                                               palette = "limited")
                                   )
                                 ),
                                 fluidRow(column(
                                   4,
                                   numericInput(
                                     'rowsize',
                                     'Row size',
                                     0.9,
                                     min = 0.2,
                                     max = 1.5,
                                     step = 0.1
                                   )
                                 ),
                                 column(
                                   4,
                                   numericInput(
                                     'colsize',
                                     'Col size',
                                     0.9,
                                     min = 0.2,
                                     max = 1.5,
                                     step = 0.1
                                   )
                                 ),
                                 column(
                                   4,
                                   numericInput(
                                     'legsize',
                                     'Legend size',
                                     0.8,
                                     min = 0.2,
                                     max = 1.5,
                                     step = 0.1
                                   )
                                 )
                                 
                                 ),
                              # column(
                              #   6,
                              #    numericInput(
                              #      'legsize',
                              #      'Legend size',
                              #      0.8,
                              #      min = 0.2,
                              #      max = 1.5,
                              #      step = 0.1
                              #    )
                              #   ),
                                 
                                 fluidRow(column(
                                   6,
                                   radioButtons("rowname",
                                                "show/hide rowname",
                                                c("hide", "show"))
                                   
                                 ),
                                 column(
                                   6,
                                   radioButtons("colname",
                                                "show/hide colnames",
                                                c("show", "hide"))
                                 )),
                                 

                                 #uiOutput('myPanel',inline=T),
                              #column(12,uiOutput('myPanel')),  
                              uiOutput('myPanel'),
                              
                              
                                 br()
                               #)
                             ))), #end of the div "form"
                           
                           br(),
                           
                           
                           # shiny::actionButton(
                           #   "toggleAdvancedgo",
                           #   "Advanced enrichment Settings",
                           #   href = "#",
                           #   style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           # ),
                           
                           shinyjs::hidden(div(
                             # Hide some widgets between the tags
                             id = "advancedgo",
                             wellPanel(
                               
                             )
                           )),
                           
                           
                           br(),
                           div(id = 'center', strong("Print Heatmap",style = "font-family: 'times'; font-size:20px; font-style: strong; ")),
                           br(),
                           
                           fluidRow(column(
                             6, uiOutput("button")
                           ),
                           column(
                             6,
                             checkboxInput("reactheat",
                                           "Add reactivity",
                                           FALSE))
                           ),
                           helpText("Note: It is highly advised to check this box if you're working with a set of genes close to 1000.",style="color:White; font-size:15px;"),
                           
                           # fluidRow(
                           #   column(4,
                           # downloadButton(
                           #   "save",
                           #   "Save your plot" ,
                           #   style = ## allowed to download an image
                           #     "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           # )),
                           # 
                           # downloadButton('downloadcut', "Download the data",
                           #                style ="color: #fff; background-color: #337ab7; border-color: #2e6da4")
                           # 
                           # ),
                           
                           conditionalPanel(condition = 'output.heatmbool',
                                            
                                            div(id = 'center', strong("GO Enrichment",style = "font-family: 'times'; font-size:20px; font-style: strong; ")),
                                            br(),
                                            
                                            
                                            fluidRow(column( 4,
                                                             
                                                             selectInput("Species", "Choose your Species:", selected = "Mus musculus", 
                                                                         c("Mouse" = "Mus musculus", "Human" = "Homo sapiens", "Rat" = "Rattus norvegicus", "C. elegans" = "Caenorhabditis elegans",
                                                                           "Zebrafish" = "Danio rerio",  "Pig" = "Sus scrofa", 
                                                                           "Chicken" = "Gallus gallus", "Chimpanzee" = " Pan troglodytes" ))),
                                                     column(
                                                       4,
                                                       uiOutput("cutgo")),
                                                     column(3, 
                                                          
                                                            selectInput(
                                                              'catinfo',
                                                              'Category: ',
                                                              choices =  c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY"),
                                                              selected=  c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY"),
                                                              multiple = TRUE
                                                            )
                                                     )),

                                            fluidRow(
                                              column(4,br(),
                                                     actionButton("GO", "Run GO",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                              column(4,br(),
                                                     #actionButton("DAVID", "Open DAVID",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"))
                                                     uiOutput("DAVID"))
                                            ),br(),
                                            helpText("Run GO results are obtained by querying DWS (DAVID Web Services)", style="font-size:15px; color:white;")
                                            
                                            ),
                           
                           #actionButton("resetAll", "Reset all"),
                           
                           br(),
                           br()
                           
                           
                           # shiny::actionButton("heatm", "Print Heatmap", style =
                           #                       "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                           
                           # Render input from server.R
                           #shinyjs::disabled(actionButton("stop", "Stop"))
                   #)
                   ),
                
                tabPanel(
                  
                  "Heatmap clustering",
                  value="cutpan",
                  # box(id="boxpassvenn",title = strong("Heatmap settings"), width = NULL, background = "light-blue",
                  #     inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
                  
                  # sliderInput(
                  #   "cutheight",
                  #   "Choose where you cut the heatmap",
                  #   min = 1,
                  #   max = 15,
                  #   value = 2,
                  #   step = 0.5
                  # ),
                  strong("Cut heatmap settings", style="font-size:25px;") ,
                  
                  br(),
                  
                  uiOutput("cutcluster"),
                  
                  selectizeInput('cutinfo', 'Choose your types of plots',
                                 choices = cutheatmlist),
                  # cutheatmlist is a variable defined in the global environment
                  br(),
                  
                  selectInput(
                    "formcut",
                    "Choose your file format",
                    choices = c("pdf", "png", "eps")
                  ),
                  
                  br(),
                  verbatimTextOutput("event"),
                  
                  
                  # shiny::actionButton("updateheatm", "Update the clusters", style =
                  #                       "color: #fff; background-color: #337ab7; border-color: #2e6da4"),
                  br(),
                  downloadButton("savecut", "Save your plot" , style =
                                   "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                
                ))
                       )
                )
      
    )),
  
  tabItem(tabName = "About",
          tags$style(type='text/css', ".well { max-width: 20em; }"),
          tabPanel(p(icon("info-circle"),
                     "About"),
                   mainPanel(includeMarkdown(
                     "markdown/about.md"
                   )), 
                   br(), br(), br(),
                   br(), br(), br(),
                   br(), br(), br(),
                   br(), br(), 
                   actionLink("session",
                              "Print version information about R, the OS and attached or loaded packages."),
                   br(), br(), br(),
                   DT::dataTableOutput("sessinfo")
          )
  )
  
  
 )
)
)

###############################
######## END dashboardbody    # 
############################### 

  ## GOOGLE ANALYTIC
 #tags$head(includeScript("google-analytics.js"))

  dbHeader <- dashboardHeader(title = "MATRiX")
  dbHeader$children[[2]]$children <-  tags$a(tags$img(src='matrix.png',height='40',width='40',style="margin:5px 0 5px 0;",align='left'), tags$h3("MATRiX",style="font-family:Purisa; margin:15px 25px 5px 0;color:white; "))
  
 shinyUI( 
    dashboardPage(skin="blue",title = "MATRiX app",
    dbHeader,
    sidebar,
    body
  )
 )




