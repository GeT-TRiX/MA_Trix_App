### Author: Franck Soubès
### Bioinformatics Master Degree - University of Bordeaux, France
### Link: https://github.com/fsoubes/MA_Trix_App
### Where: GET-TRiX's facility
### Application: MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics
### Licence: GPL-3.0


###############################
######## dashboardsidebar     #
###############################  


inactivity <- "function idleTimer() {
  var t = setTimeout(logout, 5000);
  window.onmousemove = resetTimer; // catches mouse movements
  window.onmousedown = resetTimer; // catches mouse movements
  window.onclick = resetTimer;     // catches mouse clicks
  window.onscroll = resetTimer;    // catches scrolling
  window.onkeypress = resetTimer;  //catches keyboard actions

  function logout() {
    window.close();  //close the window
  }

  function resetTimer() {
    clearTimeout(t);
    t = setTimeout(logout, 5000);  // time is in milliseconds (1000 is 1 second)
  }
}
idleTimer();"



shinyjscode <- "
shinyjs.init = function() {
  $(window).resize(shinyjs.calcHeight);
}

shinyjs.calcHeight = function() { 
  Shiny.onInputChange('plotHeight', $(window).height());
}
"


dbHeader <- dashboardHeader(title = "MATRiX")
dbHeader$children[[2]]$children <-  tags$a(tags$img(src='matrix.png',height='40',width='40',style="margin:5px 0 5px 0;",align='left'), 
                                           tags$h3("MATRiX",style="font-family:Purisa; margin:15px 25px 5px 0;color:white; "))


sidebar <- dashboardSidebar( # analyse par microréseau de l'impact transcriptomique des xénobiotiques
  useShinyjs(),
  inlineCSS(appCSS),
  tags$style(type="text/css", Errorcss),
  tags$style(type="text/css", inactivity),
  shinyjs::extendShinyjs(text = shinyjscode),

#tags$head(includeHTML("www/google-analytics.html")),
tags$head(
     #tags$script(src = inactivity),   
     tags$script(src = "custom.js")),
  div(id = "loading-content-bar",p()),

  div(
    id = "matrixapp",
    sidebarMenu(id = "side",
      menuItem("Home", tabName = "Home", icon = icon("home")),      
      menuItem("Upload Data", tabName = "Upload", icon = icon("upload")),
      menuItem("PCA", tabName = "PCA", icon = icon("line-chart")),
      menuItem("Venn diagram", tabName = "Venn", icon = icon("line-chart")),
      menuItem("Heatmap", tabName = "Heatmap", icon = icon("line-chart")),
      #menuItem("Support", tabName = "Support", icon = icon("question-circle")),
      #menuItem("About", tabName = "About", icon = icon("info-circle")),
      menuItemOutput("dymMenu"),
      collapsed = TRUE,

      tags$a(img(
        src = "GeT_logo-RVB.png",
        height = 50,
        width = 180,
        style = "position:absolute;bottom:50px;margin:0 0 15px 10px;"
      ) , href="https://get.genotoul.fr/en/", target="_blank"),
      tags$a(img(
        src = "Logotype-INRA-transparent.png",
        height = 43,
        width = 168,
        style = "position:absolute;bottom:0;margin:0 0 15px 10px;"
      ) , href="https://www6.toulouse.inra.fr/toxalim", target="_blank")
      
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
  #tags$head(includeScript("google-analytics.js")),
  
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
  
  useToastr(),
  inlineCSS(appCSS),
  includeCSS("./css/style.css"),
  div(
    id = "loading-content",
    br(),br(),
    br(),
    h2("Please wait while MATRiX is loading...")),
  div(
    id = "matrixapp",
  tabItems(
    tabItem(tabName = "Home",
            fluidRow(
              column(width=9,
                     div(style="width:100% ;max-width: 1500px; height: 1500px max-height: 2200px;",id = "homepage",
                         
                         tabBox(title="Welcome to MATRiX", width=NULL,id = "homepage",

                                tabPanel("About", style = "background-color: #ffffff;",
                                         tags$h3("MATRiX is a shiny application for Microarray Analysis on Transcriptomic impact of Xenobiotics."),
                                         p("This project initiated by Yannick Lippi aims to facilitate access to biologist in order to publish graphs such as heatmap, PCA or Venn diagram related to specifics data produced by TRiX's facility.", tags$br(),"  

MATRiX is an application dedicated to DNA chip analysis, this application incorporates quality control with Principal components analysis to summarizes microarray and differential analysis with various methods such as Venn diagram, Heatmap clustering and GO Enrichment analysis by querying the DWS (DAVID WEB SERVICES).",tags$br(),"

MATRiX app is working with specific data produced by the limma package, resulting p-values are adjusted according to the Benjamini and Hochberg procedure [Benjamini and Hochberg 1995]. PCA is computed with the FactoMineR package and the plot is produced with the factoextra package, for the Heatmap and Venn diagram the graphs are obtained respectively with the gplots and VennDiagram package, those packages are available on CRAN This application works only with specific data produced by the plateau TRiX, you can check the example file (MA_Trix_App/sampleData.zip)."),
                                      
                                         p("Hereafter is the global workflow passing by the statistical analysis to the visualization:"),tags$br(),
                                         div(id="workflow",
                                         tags$p(
                                           tags$img(src = "whatmaen.png",style="width: 100%; height: 100%"))
                                         )
                                         ),
                                tabPanel("Authors", h3("The main contributors to MATRiX:"),
                                         p(a("Yannick Lippi",href="mailto:yannick.lippi@inra.fr"), "(Initiator, beta-testing, feature suggestions)"),
                                         p(a("Franck Soubès", href="mailto:franck.soubes@inra.fr"), "(Coding, Unit testing, documentation, packaging, feature suggestions)",tags$a(href = "https://github.com/fsoubes",target="_blank",
                                                                                                                                                                                   "See github")),
                                         h3("Acknowledgements"),
                                         p("Thanks to the Toxalim's team BioToMyc & TIM and especially to the following people for their helps reporting errors, proposing new features and beta testing of MATRiX:"),
                                         p("Laura Costes,", "Anne Fougerat,","Claire Naylies,", "Philippe Pinton,","Arnaud Polizzi," ,"Marion Regnier," , "Sandrine Ellero-Simatos,","Sarra Smati."),
                                         p("Special Thanks to Didier Laborie for installing the virtual machine with Ubuntu and for answering my questions")
                                         
                                ),
                                tabPanel("Packages",
                                         tags$h3("If you are using MATRiX in your work, you can cite some of the packages by clicking on the link down below."),
                                         
                                         actionLink("session",
                                                    "Print version information about R, the OS and attached or loaded packages."),
                                         br(), br(), br(),
                                         DT::dataTableOutput("sessinfo")
                                ),
                                tabPanel("Support",
                                         tags$head(
                                           tags$style(
                                             "#entry {width: 100%;position: relative;left: 4%;}
              #users ul li {font-family: 'Inconsolata', cursive;font-weight: 500;line-height: 1.5;color: white;position: static;font-size: 18px;} 
              #users a{color: red;} #users p{color:white;}")), 
                                         
                                         includeCSS("www/shinychat.css"),
                                         
                                         # And custom JavaScript -- just to send a message when a user hits "enter"
                                         # and automatically scroll the chat window for us. Totally optional.
                                         includeScript("www/sendOnEnter.js"),
                                         fluidRow(
                                           column(width=9,
                                                  div( style = "width:100% ; max-width: 1200px; height: 500px",
                                                       #tags$h2("Support client"),
                                                       div(
                                                         class = "row-fluid", 
                                                         # Create a spot for a dynamic UI containing the chat contents.
                                                         uiOutput("chat"),
                                                         
                                                         # Create the bottom bar to allow users to chat.
                                                         fluidRow(
                                                           div(class="span8",
                                                               textInput("entry", "")
                                                           ),
                                                           div(class="span2 center",
                                                               actionButton("send", "Send")
                                                           )
                                                         )
                                                       )))
                                         
                                )), 
                                tabPanel("Video",
                                         div( id="video",
                                         fluidRow(
                                           column(8, align="center", offset = 2,
                                         tags$iframe(src = "https://www.youtube.com/embed/lfI0zRYzeJs?vq=hd1080", width="960", height="540", align= "middle", frameborder="0",allowfullscreen ="1" )                                         
                                         ))))

                                ))),
              column(width=3,
                     div(id="pass",style = "word-wrap: break-word;",
                         box(id="boxpass",title = strong("Session information", style="font-size:25px;"), width = NULL, background = "light-blue",
                             
                             # The right sidebar
                             # Let the user define his/her own ID
                             textInput("user", "Your User ID:", value=""),
                             tags$hr(),
                             h5("Connected Users"),
                             # Create a spot for a dynamic UI containing the list of users.
                             div(id ="users",
                                 uiOutput("userList"),
                                 tags$hr(),
                                 #helpText(HTML("<p>Built using R & <a href = \"http://rstudio.com/shiny/\">Shiny</a>.<p>Source code available <a href =\"https://github.com/trestletech/ShinyChat\">on GitHub</a>.")),
                                 p("Built using R and" ,tags$a(href = "http://rstudio.com/shiny/",target="_blank",
                                                               "Shiny")),
                                 p("Chat source code is available ", 
                                   tags$a(href = "https://github.com/trestletech/ShinyChat",target="_blank",
                                          "here"))
                             ))
                     ),
                     
                     box(
                       title = "What's new in MATRiX", width = NULL, status = "primary",
                       div(style = 'overflow-y: scroll; height: 500px',
                           addNews("Aug 15th 2018", "Presentation/Video", "Added a video to present MATRiX and add modules to import files"),
                           addNews("Aug 10th 2018", "Upload/Volcano", "You can explore your different comparisons with a volcano plot"),
                           addNews("Aug 6th 2018", "Venn Diagram/Enrichment", "Add acyclic graph if download"),
                           addNews("Aug 4th 2018", "Heatmap/Bubble graph", "You can display or not the labels within the bubbles"),
                           addNews("Aug 3th 2018", "Heatmap/Go(Analysis)", "You can now plot a bubble graph (Highcharts) that summarizes the top categories from the resulting datatable"),
                           addNews("Jul 28th 2018", "Home/Support", "You can asks your questions directly in the Support"),
                           addNews("Jul 25th 2018", "Tutorial/Video", "Soon will be added a video to summarise the application"),
                           addNews("Jul 20th 2018", "New page", "Add a Home page regrouping informatios about the app"),
                           addNews("Jul 16th 2018", "Venn" ,"You can now choose your color for the venn diagram"),
                           addNews("Jul 16th 2018","Bug fixes","Venn diagram display erros when filtering"),
                           addNews("Jul 5th 2018","Venn/DAVID","Add Gene functionnal classification for selected intersection(s)"),
                           addNews("Jun 26th 2018","Add features","It's now possible to interact with the rendering table to filter the table in the aim of plotting the top n genes.
                          For the GO enrichment it is now possible to select the rows in order to display the gene symbol according to the entrez ids"),
                           addNews("Jun 22th 2018","Bug fixes","For two contrasts the venn.draw function was not ordering the contrast names in the right order."),
                           addNews("Jun 20th 2018","MATRiX","First public release of MATRiX. 
                                                              Enhancement of the gui with the use of dashboard package"),
                           addNews("Jun 18th 2018","GO enrichment","It is now possible to query the DWS for the Heatmap and save the result in xlsx format for the different clusters"),
                           addNews("Jun 15th 2018","DNS ","Adding DNS for the MATRiX application (matrix.toulouse.inra.fr)"),
                           addNews("Jun 10th 2018","Venn diagram","The venn diagram FC and display of the top n genes
                                                                have been added to compare the results of 2 or more contrasts."), 
                           addNews("Jun 5th 2018","PCA/Heatmap","Display color groups side by side in the gui"), 
                           addNews("May 29th 2018","beta-test","The service will be made available once the beta test phase is officially completed.")
                       )
                     )
                    
              )
            )
            ),
    
    tabItem(tabName = "Upload",
            bsAlert("alert"),
            tags$style(type='text/css', ".well { max-width: 2em; }"),
            fluidRow(
              tags$head(
                tags$style(type="text/css", ".myslidermain .irs-grid-text {bottom: 5px;color: #333;} 
      .myslidermain .irs-min{color: #333;font-size: 10px;line-height: 1.333;text-shadow: none;top: 0;padding: 1px 3px;
      background: rgba(0,0,0,0.1);border-radius: 3px;-moz-border-radius: 3px} 
      .myslidermain .irs-max{color: #333;font-size: 10px;line-height: 1.333;text-shadow: none;top: 0;padding: 1px 3px;
      background: rgba(0,0,0,0.1);border-radius: 3px;-moz-border-radius: 3px}")
              ),
              #div(style = "width:100% ; max-width: 1500px; height: 1500px max-height: 2200px;",
              column(width=9,
                div( style = "width:100% ; max-width: 1500px; height: 1500px max-height: 2200px;" , id = "upload",
                     tabBox(title="Upload your data", width=NULL,id = "upload",
                            
                            tabPanel("Import your data", style = "background-color: #ffffff;",
                            conditionalPanel(condition = 'output.boolmark', #Hide or Show event depending on the loading data success or failure
                                             #includeMarkdown("markdown/help.md")
                                             tags$h1("How to import ?"),
                                             #<h1 id="howtoimport">How to import ?</h1>
                                             tags$ul(
                                               tags$li("First click on the browse button to load the data"),
                                               tags$li("After the pop up has appeared, you will have to select the files within the access path that is given in the report produced by Yannick. "),
                                               tags$li("You will then find three distinct csv files, these files are respectively named xxx_pData, xxx_WorkingSet and xxx_ResTable."),
                                               tags$li("The final step consist to select all the data at once and then confirm the selection by clicking on the open button."),
                                               tags$li("A green message will then appear to confirm the data loading with a summary table.")
                                             ),

                                               tags$p(
                                                 
                                                tags$img(src = "pdata.png"),
                                                tags$img(src = "workingset.png"),
                                                 tags$img(src = "restable.png")
                                                
                                                 ),

                                             tags$h1("Tips"),
                                             tags$ul(
                                               tags$li("You can select a region by handling the left click button if the files are stacked together, if it's not the case you can select the different files by maintening the Ctrl button and clicked on the files.")
                                             ),  
                                             tags$h1("Warning"),
                                             tags$ul(
                                               tags$li("It is highly recommanded to not modify these files (removed columns, change column names ...) in the aim of not disturbing the well functionning of the application.")
                                             )
                                             ,ns = NS("datafile")
                                             ),
                            conditionalPanel(condition = '!output.boolmark',
                            
                            textOutput("myFileName"),
                            
                            column(12, h3(
                                "This table summarizes the number of significant genes depending on the p-value treshold choosen with the slider bar"
                              ),
                              helpText("Choose your p-value treshold to modify the following data table"),
                              div( class= "myslidermain",
                              sliderInput("pval1","",min = 0.01,max = 0.05,value = 0.05,step = 0.01,
                                width = "500"
                              ),
                              dataTableOutput("data_summary") # render a renderTable or renderDataTable within an application page
                            )),
                            
                            column(12,
                              h3("This table shows the samples with the corresponding groups"),
                              dataTableOutput("new_test")
                            )
                            ,ns = NS("datafile")
                            
                   )),
                     tabPanel("Volcano plot",value="volcano", style = "background-color: #ffffff;", 
                              conditionalPanel(condition = '!output.boolmark',
                             div(style="display:inline-block;",           
                              fluidRow(column(3, style="width:33.5%;",
                              downloadButton("savevolcano", "Save your Volcano plot" , style =
                                               "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                              column( 3, style="width:20%;",
                                      selectInput(
                                        "formvolc",label = NULL,
                                        choices = c("png", "eps", "pdf")))))
                              ),
                              plotOutput(outputId = "volcanoplot", height = 900)
                   )
            ))),  
            div(id="pass",style = "word-wrap: break-word;",
            column(width = 3,
            box(id="boxpass",title = strong("Upload data", style="font-size:25px;"), width = NULL, background = "light-blue",
                inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),

                
                downloadLink("downloadData", label = "download sample data", style="color:red; float:right;"),
                br(),br(),
                csvFileInput("datafile", "User data (.csv format)"),
                p("Import local example",style="color:white; font-weight: 700; font-size: 14px;"),
                dirModuleUI("datafile"),

                #csvFileInput("file", "Choose your csv files"),
                # fileInput(
                #   "file",
                #   "Choose your csv files",
                #   accept = c("text/csv","text/comma-separated-values,text/plain",".csv"
                #   ),
                #   multiple = T # Attribute to load multiple data at once
                # ),
                br(),
                
              conditionalPanel(condition = '!output.boolmark',
                selectInput(
                  "method","Choose your statistical method",choices = c("adj.p.val (FDR)" = "FDR", "p.value (raw)" = "None")),
                strong("VOLCANO plot",style="font-size:18px;"),
                br(),br(),
              uiOutput("compvolc"),
              div(id = "mytextvolc", 
                  p(" Highlight your selected gene(s) in the volcano plot with a comma-separated list of input ")
                  ),
              
              textInput(inputId = "fillvolc",label = NULL,value = "",
                        placeholder = "FOXP2,OT,AVPR1a",width = "100%"
              ),
              div(id = "mytextvolcgrep", 
                  p(" Highlight a family of gene in the volcano plot")
              ),
              
              textInput(inputId = "findfamily",label = NULL,
                        placeholder = "Cyp",width = "100%"),
              numericInput(
                'topvolc',
                'Maximal number of genes by comparison(s)',
                NULL,min = 0,max = 5000),
              fluidRow(column(6, sliderInput('volclab', "Choose your lab size",min = 1, max=6, step = 0.5,value=3.0)),
                       column(6,sliderInput('volcpt', "Choose your point size", min=0.5, max=3, step=0.1,value=1))),
              fluidRow(column(6, sliderInput('volcfc', "Choose your cutoff FC",min = 1, max=10, step = 1,value=1)),
                       column(6,sliderInput('volcpval', "Choose your pval cutoff", min=0.01, max=0.05, step=0.01,value=0.05)))
              ,ns = NS("datafile")
                ))
              )
            ),
            conditionalPanel(condition = '!output.boolmark',
            column(12,
                    h3("Show the actual data frame with the columns selected"),
                    dataTableOutput("new_group")
            ),ns = NS("datafile"))
          
            
            )),
    
  ###############################
  ######## PCA page             #
  ############################### 
    
    tabItem(tabName = "PCA",
            tags$style(type='text/css', ".well { max-width: 20em; }"),
            tags$style(type='text/css', ".well { max-height: 50em; }"),
            fluidRow(column(
              width = 9,
              div(style = "width:100% ; max-width: 1500px; height: 1500px max-height: 2200px;",
                #style = "width:100% ; max-width: 1200px; height: 1050px",
                #tabBox(title="PCA, width=NULL,id = "homepage",
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
                    tags$style(type = "text/css",".shiny-output-error:before { visibility: hidden; }"),
                    
                    
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
                    
                    plotOutput(outputId = "PCA", height = 700)
                  )
                  
                )
              )
            ),
            div(id="pass",style = "word-wrap: break-word;",
                column(width=3,
                       box(id="boxpasspca",title = strong("PCA settings",style="font-size:25px;"), width = NULL, background = "light-blue",
                           inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
                           strong("Choose your group to visualize"),
                          uiOutput("individuselpca"),
                  actionButton(
                    inputId = "allIndividuspca",label = "Select all",
                    icon = icon("check-square-o"),
                    style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                  ),
                  actionButton(
                    inputId = "noIndividuspca",label = "Clear selection",icon = icon("square-o"),
                    style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                  ),
                
                
                  fluidRow(column(6,
                    selectInput("dim1",
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
                  column(6,
                    selectInput("dim2",
                      label = h6(gettext("y axis")),
                      choices = list(
                        "Dim 1" = 1,
                        "Dim 2" = 2,
                        "Dim 3" = 3,
                        "Dim 4" = 4,
                        "Dim 5" = 5
                      ),
                      selected = secdim
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
                    "labelsiize","Label size",min = 2,max = 6,value = 4,step = 1
                  ),
                  
                  sliderInput("pointsiize","Point size",min = 2,max = 6,value = 2,step = 1
                  ),
                  
                  uiOutput('myPanelpca'),
                  br()
            ))))), 
  ###############################
  ######## Venn Page            #
  ############################### 
  
  
    tabItem(tabName = "Venn",
            
            tags$style(type='text/css', ".well { max-width: 25em; }"),
            tags$style(type='text/css', ".well { max-height: 70em; }"),
            fluidRow(column(
              width = 9,
              div(
                style = "width:100% ; max-width: 1500px; height: 1780px; max-height: 2800px;",
                tabsetPanel(
                id = "Vennd",    

                  tabPanel(
                    value= "vennset",
                    strong("Visualize the Venn diagram"),
                  
                    div(style="display:inline-block",
                        fluidRow(column(3, style= "width:14.2%;",

                                        downloadButton('downloadvenn', "Download the data",
                                                       style =
                                                         "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                 column(3, style="width:14.2%;",
                                        downloadButton("downloadsetven", "Download venn set" , style =
                                                         "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                 column(3, style="width:11.8%;",
                                        downloadButton("savevenn", "Save your plot" , style =
                                                         "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                 
                                     column(3,style="width:10%;", selectInput(
                                       "formven",label = NULL,
                                       choices = c("png", "eps", "pdf"))),
                                 column(3 ,style="width:5%;"),
                                 column(3,
                                        downloadButton('downloadvennset', "Download the filtered data",
                                                style ="color: #fff; background-color: #337ab7; border-color: #2e6da4;"))
                                 
                        )),

                    conditionalPanel(condition = '!output.bool',
                                     uiOutput(outputId = "image")
                                     , uiOutput("sorry")),
                    tags$script(src="libraries/bootstrap.min.js") , 
                    tags$script(src="libraries/prettify.js") , 
                    tags$script(src="libraries/bootstrap-colorpicker.min.js") , 
                    tags$script(src="libraries/jvenn.min.js")  ,
                    tags$script(src="libraries/canvas2svg.js")  ,
                    fluidRow(column(6,br(),br(),
                    tags$script(src="jvenn.js"),
                    tags$div(id="jvenn-container", style = "background-color: white;  width: 600px;")

                    ),
                    column(6,
                           div(class= "dfvenn" , style="font-size:24px; margin-top: 17px;",
                               htmlOutput("dfvenn")),
                  conditionalPanel(condition = "input.dispvenn == 'genes'", 
                           helpText(
                             "You can directly filtered the table by fold change and save the output table"
                           )),
                           
                           DT::dataTableOutput("vennresinter"),br(),br(),br(),
                           conditionalPanel(condition = "input.selcontjv", 
                                            div(class= "dfvennbef" , style="font-size:24px; margin-top: -28px; "))

                    )),
                           div(style="display:inline-block", id ="dontwanttoshow",
                               fluidRow(
                                 tags$head(
                                   tags$style(type="text/css", ".topgeness label{ display: table-cell; text-align: left; vertical-align: middle; } 
                 .inline .form-group{display: table-row;} ")
                                 ),
                                 column(3,br(),style= "width:21%;",
                                        actionButton(
                                          inputId = "topdegenes",
                                          label = "Plot top DE genes",
                                          style =
                                            "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                        )),
                                 column(3, style= "width:26.0%;",br(),
                                        
                                        downloadButton("savebarplot", "Save your barplot" , style =
                                                         "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                 column(3 ,br(),style= "width:11%;  padding: 0%;",
                                        selectInput( "formvenbar",label = NULL,
                                                     choices = c("png", "eps", "pdf"))),
                                 
                                 column(3,style= "width:9%; padding: 0%;", 
                                        
                                        uiOutput("topgenesvenn", style= "padding: 0px;font-weight: 400;top: 0px;
                                                 right: -22px;left: 0px;color: #3c8dbc;position: absolute;"))
                               )),
                   plotOutput(outputId ="barplotvenn", height = "500px", width ="100%"),
                    #plotOutput(outputId ="barplotvenn", height = "auto"),
                   br(),
                   ##includeHTML("HTML/colorandname.html"),
                   
                   h1("Here's a tracker for your different selections:"),
                   tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # add style.css in order to add better police
                   ),
                   tags$head(
                     tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap-responsive.css") # add style.css in order to add better police
                   ),
                   tags$head(tags$style("
                                        #container * {
                                        display: inline;
                                        }")),
                      
                   tags$head(tags$style("#mytext p{font-weight: 500;font-size: 17px;line-height: 1.5;color: white;
                                        position: static;}
                                        #mytext a{color: red;}"
                                        )),
                   
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
                     p("for this intersection"),
                     htmlOutput("continter"),
                     p("if you click on the top DE genes button you will plot the top"),
                     htmlOutput("topgenesdf"),
                     p("rows the of the previous table")
                   )
                   
                  ),
   
  
                tabPanel(strong("Venn GO enrichment"),
                            value = "venngopanel",
                            useShinyjs(),
                            
                                fluidRow( column(6 ,
                                                 downloadButton(
                                                   "saveclusterchoose",
                                                   "Download the graph" ,
                                                   style =  "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                                                 ),
                                          column( 3, 
                                                  selectInput(
                                                    "formvennclus",label = NULL,
                                                    choices = c("png", "eps", "pdf")))
                                          
                                )),
                            
                            
                          plotOutput("clusterPlot", width = "100%", height = "700px"),
                          br(),br(),br(),
                          #plotOutput("acyclicgo", width = "100%", height = "1200px"),
                          verbatimTextOutput("debug")
                 )
                          
                )
              )
            ),
            div(id="pass",style = "word-wrap: break-word;",
                column(width=3,
                       box(id="boxpassvenn",title = strong("Venn settings", style ="font-size:25px;"), width = NULL, background = "light-blue",height = "100%",
                           inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
                           uiOutput("contout"),
                           actionButton(
                             inputId = "allCont",
                             label = "Select all",
                             icon = icon("check-square-o"),
                             style =
                               "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           ),
                           
                          actionButton(inputId = "noCont",label = "Clear selection",
                          icon = icon("square-o"),
                          style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                           ),
                           fluidRow(column(6,
                           selectInput("methodforvenn","Statistical method",
                             choices = c("adj.p.val (FDR)"= "FDR", "p.value (raw)" = "None")
                           )),
                          column(6,
                          selectInput("regulation", #  Create a select list that can be used to choose a single or multiple items from a list of values.
                                                "Choose your regulation",
                                                choices = c("both","up", "down")))),
                          div(id = "mytext", 
                           p("A comma-separated list of ",
                                tags$a(href = "https://stat.columbia.edu/~tzheng/files/Rcolor.pdf",target="_blank",
                                  "x11"),
                                "or",
                                tags$a(href = "https://en.wikipedia.org/wiki/Web_colors#Hex_triplet",target="_blank",
                                  "hex colors."))),
                          
                           textInput(inputId = "fill",label = NULL,value = c( "green,blue,red,purple,orange,brown"),
                             placeholder = "grey70, white, steelblue4",width = "100%"
                           ),
                           
                           fluidRow( column(6,
                                            sliderInput("pvalvenn","P-value treshold",
                                              min = 0.01,max = 0.05,
                                              value = 0.05,step = 0.01
                                            )),
                                     #br(),
                                     column(6,
                                            sliderInput("fcvenn","FC treshold",min = 1, max = 10,
                                              value = 1,step = 1
                                            ))),
                           br(),
                           
                           fluidRow(
                             column(12,
                                  selectInput("dispvenn", #  Create a select list that can be used to choose a single or multiple items from a list of values.
                                              "Choose if you want to display probes or genes",
                                              choices = c("probes", "genes"))),
                           column(6, 
                                  checkboxInput("Notanno","Remove the genes that are not annotated",FALSE)),
                           column(6, 
                                  checkboxInput("Allcont","Parse all the resulting logFC values depending on the comparison(s) selected",FALSE))),
                           
                           br(),
                          
                          
                          
                          shiny::actionButton(
                            "toggleAdvancedJvenn",
                            "Advanced Jvenn Options",
                            href = "#",
                            style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                          ),
                          br(),
                  
                          shinyjs::hidden(div(
                            id = "advancedjvenn",
                            br(),

                            fluidRow(
                              column(6,
                                     p("Select your Type of Venn",style="color:white; font-weight: 700; font-size: 14px;"),
                                     includeHTML("HTML/jvenntype.html")),
                              column(6,
                                     p("Display the stat",style="color:white; font-weight: 700; font-size: 14px;"),
                                     includeHTML("HTML/displaystat.html"))),
                            br(),
                             p("Police's size", style="color:white; font-weight: 700; font-size: 14px;"),
                              includeHTML("HTML/fontsize.html"),
                            br(),
                               p("Find an element in list(s)",style="color:white; font-weight: 700; font-size: 14px;"),
                             includeHTML("HTML/seekgene.html")
                           
                              
                            )),
                          br(),

                      
                      strong("Functional Annotation Clustering",style = "font-family: 'times'; font-size:20px; font-style: strong; "),
                      
                      br(),br(),
                      fluidRow(column(6, br(),sliderInput("clusterNumber",label = "Cluster",
                                                          value = 1, min = 1,max = 5
                      )),
                      column(6,br(),
                      selectInput("Speciesvenn", "Choose your Species:", selected = "Mus musculus", 
                                  c("Mouse" = "Mus musculus", "Human" = "Homo sapiens", "Rat" = "Rattus norvegicus", "C. elegans" = "Caenorhabditis elegans",
                                    "Zebrafish" = "Danio rerio",  "Pig" = "Sus scrofa", 
                                    "Chicken" = "Gallus gallus", "Chimpanzee" = " Pan troglodytes" )))),
                      fluidRow(
                        column(5),
                        
                      column(4,br(),
                      actionButton("GOvenn", "Run GO",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")))
                      
                       ))))),

  ###############################
  ######## Heatmap Page         #
  ###############################
  
  
    tabItem(tabName = "Heatmap",
            tags$style(type='text/css', ".well { max-width: 25em; }"),
          
            
            fluidRow(column(
              width = 8,
              div(
                tabsetPanel(
                  id = "tabset1",
                  tabPanel(
                     strong("Visualize the Heatmap"),value = "hmmainpan",
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
                    
                    conditionalPanel(
                      condition = "input.col1 =='blue' && input.col3 =='red' && input.submit == 0 ", 
                      wellPanel(style = "position: absolute; width: 30%; left: 35%; top: 40%;
                         box-shadow: 10px 10px 15px grey;",
                                selectInput("text", "Choose your intermediate color:", choices = c("yellow", "white")),
                                actionButton("submit", "Submit"))
                    ),
                        
                        #plotOutput("warningsheat")
                        conditionalPanel(condition = 'output.heatmbool',
                                         plotOutput("distPlot", width = "100%" , height = 1300)
                                         
                                         ),
                        
                        h1("Here's a tracker for your different selections:"),
                          tags$head(
                            tags$link(rel = "stylesheet", type = "text/css", href = "style.css") # add style.css in order to add better police
                          ),
                          
                          tags$head(tags$style("
                             #container * {display: inline;}")),
                          
                          div(
                            id = "container",p("There are"),htmlOutput("myNUM"),
                            p("significant genes"),
                            p("for the following comparison(s)"),
                            htmlOutput("testtt")
                          ),
                          div(
                            id = "container",
                            p('The selected rows for your heatmap are based on the '),
                            textOutput("myMET"),
                            p("method, with a P-value and FC treshold respectively set to "),
                            textOutput("myPVAL"),
                            p('and'),
                            textOutput("myFC")
                          ),
                    
                    conditionalPanel(condition = "input.maxgen != null",
                     div(
                       id = "container",
                      p("You have chosen to regulate your comparison to "),
                      textOutput("maxGen"),
                      p(" genes maximum"))),
                     
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
                      
                      
                      h3("This table summarizes the number of significant probes and genes by cluster"),
                      helpText(
                        "For the number of genes by cluster the duplicated genes are removed"
                      ),
                      dataTableOutput("totalgenbyc")
                    )),
                  tabPanel(
                    strong("(GO) enrichment-based cluster analysis"),value="maingo",
                    downloadButton("savegohmdavxlsx", "Save your enrichment as xlsx" , style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"),

                    conditionalPanel(condition = "input.GO",
                     div(class= "highvenn" , style="font-size:24px; text-align: center;",
                               htmlOutput("titlegomain")),
                    DT::dataTableOutput("davidgo"), 
              
                    verbatimTextOutput("printmessage"),
                    verbatimTextOutput("printselected"),
                    div(class= "highvenn" , style="font-size:24px; text-align: center;",
                        htmlOutput("titlegotop")),

                    tags$script(src="libraries/jquery-1.9.1.min.js"),
                    tags$script(src="libraries/jquery-ui.min.js"),
                    tags$script(src="libraries/highcharts.js"),
                    tags$script(src="libraries/highcharts-more.js"),
                    #tags$script(src="libraries/modules/exporting.js"), 
                    #tags$script(src="libraries/modules/export-data.js"),
                    tags$script(src="https://code.highcharts.com/modules/exporting.js"), 
                    tags$script(src="https://code.highcharts.com/modules/export-data.js"),
                    
                    
                    tags$div(id="highChart")  ,
                    #tags$div(id="highChart",style="width: 100%;, height: 600px;")  ,
                    checkboxInput("addlabelhigh", "add label", FALSE),
                    tags$script(src="bubble.js")

                  )),
                  tabPanel(
                    strong("Cut heatmap"),#icon("table"), 
                    bsAlert("alert"),value = "cuthmmainpan",
                    plotlyOutput(outputId = "cutheatmap", height = 600),
                    
                    
                    br(),br(),br(),br(),br(),br(),br(),br(),br(),
                    br(),br(),br(),br(),br(),br(),br(),br(),br(),
                    br(),br()
                    
                    )
                  )
            )
            ),
                column(width=4,

                       div(id="mypanheat", style="color: white;",
                      tabBox(
                         
                         title = "",
                         id = "tabset25",
                         width = NULL, 
                        
                           tabPanel("Heatmap",id= "heatmpan", ##ADD8E6
                             style="background-color: #3c8dbc;",
                             value="widgetheat",
                          strong("Heatmap settings", style="font-size:25px;") ,
                          br(),
                             
                           actionLink("resetAll",  label = ("reset all"), style="color:orange;float:right;font-size: 18px;"),
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
                               style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             ),
                           div(
                             id = "form",
                               # Creates a panel with a slightly inset border and grey background
                               uiOutput("testout"),
                               actionButton(
                                 # Action button that automatically react when triggered
                                 inputId = "allTests",label = "Select all",icon = icon("check-square-o"),
                                 style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                               ),
                               actionButton(
                                 inputId = "noTests",label = "Clear selection",icon = icon("square-o"),
                                 style ="color: #fff; background-color: #337ab7; border-color: #2e6da4"
                               ),
                             br(),br(),
                             fluidRow( column(6,
                                              numericInput(
                                                # Create an input control for entry of numeric values
                                                'maxgen',
                                                'Maximal number of genes by comparison(s)',
                                                NULL,min = 100,max = 2500
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
                             br(),
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
                               id = "advanced",
                                 fluidRow(
                                   column(6,
                                          numericInput('clusters', 'Cluster count', 3,
                                                       min = 1, max = 15)),
                                   column(6,
                                          selectInput(
                                            "dist","Choose your matrix distance",choices = c("correlation", "euclidian","manhattan", "cosine"))
                                   )),
                                 fluidRow(
                                   column(6,
                                          checkboxInput("meangrp",
                                                        "Compute the mean for the different groups",
                                                        FALSE)),
                                   column(6,
                                          selectInput(
                                            "algomet","Choose your hierarchical clustering method",choices = c("ward.D2", "single","complete","average")))
                                   
                                 )))
                             ,br(),
                             shiny::actionButton("toggleAdvancedcolors","Advanced graphical Settings",
                               href = "#",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
                             ),
                             
                             br(),
                             
                             shinyjs::hidden(div(
                               id = "advancedcol",
                                 fluidRow(
                                   column(6,
                                          colourpicker::colourInput(
                                            "col1","Select colour for downregulated genes",firstcol,palette = "limited"
                                          )
                                   ),
                                   column(
                                     6,
                                     colourpicker::colourInput("col3", "Select colour for upregulated genes", lastcol,
                                                               palette = "limited")
                                   )
                                 ),
                                 fluidRow(column(4,
                                   numericInput(
                                     'rowsize','Row size',0.9,
                                     min = 0.2,max = 1.5,step = 0.1
                                   )
                                 ),
                                 column(4,
                                   numericInput(
                                     'colsize','Col size',0.9,
                                     min = 0.2,max = 1.5,step = 0.1
                                   )
                                 ),
                                 column(4,
                                   numericInput(
                                     'legsize','Legend size',0.8,
                                     min = 0.2,max = 1.5,step = 0.1
                                   )
                                 )
                                 
                                 ),
                                 
                                 fluidRow(column(6,
                                   radioButtons("rowname",
                                                "show/hide rowname",
                                                c("hide", "show"))
                                   
                                 ),
                                 column(6,
                                   radioButtons("colname",
                                                "show/hide colnames",
                                                c("show", "hide"))
                                 )),
 
                              uiOutput('myPanel'),
                                 br()
                             ))), #end of the div "form"
                           br(),
                           shinyjs::hidden(div(
                             # Hide some widgets between the tags
                             id = "advancedgo",
                             wellPanel(
                               
                             )
                           )),
                           
                           br(),
                           div(id = 'center', strong("Print Heatmap",style = "font-family: 'times'; font-size:20px; font-style: strong; ")),
                           br(),
                           
                           fluidRow(column(6, uiOutput("button")
                           ),
                           column(6,
                             checkboxInput("reactheat",
                                           "Add reactivity",
                                           FALSE))
                           ),
                          #shinyjs::disabled(actionButton("stop", "Stop")),
                          
                           helpText("Note: It is highly advised to check this box if you're working with a set of genes close to 1000.",style="color:White; font-size:15px;"),
                           
                           
                           #conditionalPanel(condition = 'output.heatmbool',
                          conditionalPanel(condition = 'output.heatmbool',
                          #conditionalPanel(condition = 'input.button >0', 
                                            
                                            div(id = 'center', strong("Functional enrichment analysis",style = "font-family: 'times'; font-size:20px; font-style: strong; ")),
                                            br(),
                                            fluidRow(column( 4,
                                                             selectInput("Species", "Choose your Species:", selected = "Mus musculus", 
                                                                         c("Mouse" = "Mus musculus", "Human" = "Homo sapiens", "Rat" = "Rattus norvegicus", "C. elegans" = "Caenorhabditis elegans",
                                                                           "Zebrafish" = "Danio rerio",  "Pig" = "Sus scrofa", 
                                                                           "Chicken" = "Gallus gallus", "Chimpanzee" = " Pan troglodytes" ))),
                                                     column(4,
                                                       uiOutput("cutgo")),
                                                     column(3, 
                                                            selectInput('catinfo','Category: ',
                                                              choices =  c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY"),
                                                              selected=  c( `BP`= "GOTERM_BP_ALL", `MF` = "GOTERM_MF_ALL", `CC`=  "GOTERM_CC_ALL", `Kegg`= "KEGG_PATHWAY"),
                                                              multiple = TRUE
                                                            )
                                                     )),
                                            fluidRow(
                                              column(4,br(),
                                                     actionButton("GO", "Run Analysis",style = "color: #fff; background-color: #337ab7; border-color: #2e6da4")),
                                              column(4,br(),
                                                     uiOutput("DAVID"))
                                            ),br(),
                                            helpText("Run GO results are obtained by querying DWS (DAVID Web Services)", style="font-size:15px; color:white;")
                                            
                                            ),br(),br()

                   ),
                
                tabPanel(
                  "Heatmap clustering",
                  value="test3",
                  strong("Cut heatmap settings", style="font-size:25px;") ,
                  br(),
                  uiOutput("cutcluster"),
                  selectizeInput('cutinfo', 'Choose your types of plots',
                                 choices = cutheatmlist),
                  # cutheatmlist is a variable defined in the global environment
                  br(),
                  
                  selectInput("formcut","Choose your file format",choices = c("pdf", "png", "eps")
                  ), br(),
                  verbatimTextOutput("event"),
                  
                  br(),
                  downloadButton("savecut", "Save your plot" , style =
                                   "color: #fff; background-color: #337ab7; border-color: #2e6da4")
                
                ))
                       )
                )
      
    ))
 )
)
)

###############################
######## END dashboardbody    # 
############################### 

 shinyUI( 
    dashboardPage(skin="blue",title = "MATRiX app",
    dbHeader,
    sidebar,
    body
  )
 )




