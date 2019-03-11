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
          ),column(width = 9, div( style = "width:100% ; 
                                   max-width: 1500px; height: 1500px max-height: 2200px;" ,id = "upload",
                                   tags$h1("How to import ?"),
                                   tags$ul(
                                     tags$li("First click on the browse button to load the data"),
                                     tags$li("After the pop up has appeared, you will have to select the data files."),
                                     tags$li("You need three distinct csv files, these files are respectively named xxx_pData, xxx_WorkingSet and xxx_ResTable."),
                                     tags$li("pData : The experimental design in a 2 column table that associates samples to their respective biological conditions"),
                                     tags$li("WorkingSet : The table of the normalised expression values (log2, cpm, ...) with genes in rows and samples in columns. The first column must contain the unique gene identifier (or transcript, probe, ...)."),
                                     tags$li("ResTable :  The table containing the results of differential analysis (fold change, p-value and FDR) next to a first column with unique gene identifier and a second column with the gene symbol."),
                                     tags$li("The final step consist to select all the data at once and then confirm the selection by clicking on the open button."),
                                     tags$li("A green message will then appear to confirm the data loading with a summary table.")
                                   ),
                                   tags$p(
                                     tags$img(src = "pData.png"),
                                     tags$img(src = "restable.png"),
                                     tags$img(src = "workingset.png")
                                   ),
                                   
                                   tags$h1("Tips"),
                                   tags$ul(
                                     tags$li("Unique IDs and GeneName columns must appear in the statistical data and unique IDs columns for Workingset in order to import the data (RNA-seq, DNA-chips, ChIP-Seq)."),
                                     tags$li("You can select a region by handling the left click button if the files are stacked together, if it's not the case you can select the different files by maintening the Ctrl button and clicked on the files.")
                                     
                                   ),
                                   tags$h1("Warning"),
                                   tags$ul(
                                     tags$li("It is highly recommanded to not modify these files (removed columns, change column names ...) in the aim of not disturbing the well functionning of the application.")
                                   )



))),  div(id="pass",style = "word-wrap: break-word;",
          column(width = 3,
                 box(id="boxpass",title = strong("Upload data", style="font-size:25px;"), width = NULL, background = "light-blue",
                     inlineCSS(list(.pwdGREEN = "background-color: #DDF0B3",.pwdRED = "background-color: #F0B2AD")),
                     
                     
                     downloadLink("downloadData", label = "download sample data", style="color:orange; float:right;"),
                     br(),br(),
                     
                     
                     csvFileInput("datafile", "User data (.csv format)"),
                     fluidRow(column(6,
                                     p("Import local example",style="color:white; font-weight: 700; font-size: 14px;"),
                                     
                                     dirModuleUI("datafile")),
                              column(6,
                                     csvDecimal("datafile")
                                     
                              )),
                     csvIdentifier("datafile", "Unique identifier"),
                     br()
                     

)))),

tabItem(tabName = "Data summary",
        fluidRow(tabItem(tabName = "Data summary",
                         fluidRow(
                           column(
                             width = 9,
                             div(
                               style = "width:100% ; max-width: 1500px; height: 1500px max-height: 2200px;" ,
                               id = "sumardata",
                               tabBox(
                                 title = "Data summary",
                                 width = NULL,
                                 id = "sumardata",
                                 tabPanel(
                                   "Summary tables",
                                   style = "background-color: #ffffff;",
                                   textOutput("myFileName"),
                                   
                                   column(
                                     12,
                                     h3(
                                       "This table summarizes the number of significant genes depending on the p-value treshold choosen with the slider bar"
                                     ),
                                     helpText("Choose your p-value treshold to modify the following data table"),
                                     div(
                                       class = "myslidermain",
                                       sliderInput(
                                         "pval1",
                                         "",
                                         min = 0.01,
                                         max = 0.05,
                                         value = 0.05,
                                         step = 0.01,
                                         width = "500"
                                       ),
                                       renderoutputTable("rendersummary")# render a renderTable or renderDataTable within an application page
                                     )
                                   ),
                                   
                                   column(
                                     12,
                                     h3("This table shows the samples with the corresponding groups"),
                                     renderoutputTable("renderpdata")
                                   ),
                                   column(
                                     12,
                                     h3("Show the actual data frame with the columns selected"),
                                     renderoutputTable("renderestab")
                                   )
                                   
                                 ),
                                 
                                 tabPanel(
                                   "Volcano plot",
                                   style = "background-color: #ffffff;",
                                   
                                   
                                   div(style = "display:inline-block;",
                                       fluidRow(
                                         column(5, style = "width:30%", downloadFiles("savevolc", "Save your barplot")),
                                         column(3, style = "width:20%;", selFormat("savevolc"))
                                       )),
                                   plotOutput(outputId = "volcanoplot", height = 900) ,
                                   
                                   div(style = "display:inline-block;",
                                       fluidRow(
                                         column(
                                           3,
                                           style = "width:34.0%;",
                                           downloadFiles("savebarvolc", "Save your Volcano plot")
                                         ),
                                         column(3, style = "width:20%;", selFormat("savebarvolc"))
                                       )),
                                   
                                   plotOutput(outputId = "barplotvolc", height = 500)
                                 ),
                                 tabPanel(
                                   "Stripchart genes",
                                   value = "stripgenes",
                                   
                                   column(width = 12,
                                          div(
                                            id = "stripbox",
                                            box(
                                              title = "Filter the table",
                                              width = 10,
                                              status = "primary",
                                              solidHeader = TRUE,
                                              collapsible = TRUE,
                                              collapsed = TRUE,
                                              icon = icon("arrow-down"),
                                              column(
                                                width = 4,
                                                radioButtons(
                                                  "decidemethodstrip",
                                                  label = "Choose your statistical method",
                                                  choices = c("adj.p.val (FDR)" = "FDR", "p.value (raw)" = "None"),
                                                  inline = TRUE
                                                )
                                              ),
                                              div(class = "myslidermain", cutoffElements("degstrip", 3, 3)),
                                              
                                              column(width = 12,  textOutput("selected_stripgene"))
                                            )
                                          )),
                                   
                                   column(
                                     12,
                                     h3("This table shows the normalized values"),
                                     renderoutputTable("orderedwk")
                                   ),
                                   
                                   div(style = "display:inline-block;",
                                       fluidRow(
                                         column(3, style = "width:27.0%;", downloadFiles("savestrip", "Save your plot")),
                                         column(3, selFormat("savestrip"))
                                       )),
                                   
                                   
                                   plotOutput(
                                     outputId = "renderstripgenes",
                                     height = "500px",
                                     width = "100%"
                                   )
                                   
                                   
                                 )
                               )
                             )
                           ),
                           div(id = "pass", style = "word-wrap: break-word;",
                               column(
                                 width = 3,
                                 box(
                                   id = "boxpass",
                                   title = strong("Upload data", style = "font-size:25px;"),
                                   width = NULL,
                                   background = "light-blue",
                                   inlineCSS(
                                     list(.pwdGREEN = "background-color: #DDF0B3", .pwdRED = "background-color: #F0B2AD")
                                   ),
                                   
                                   
                                   downloadLink("downloadData", label = "download sample data", style =
                                                  "color:orange; float:right;"),
                                   br(),
                                   br(),
                                   
                                   
                                   csvFileInput("datafile", "User data (.csv format)"),
                                   fluidRow(column(
                                     6,
                                     p("Import local example", style = "color:white; font-weight: 700; font-size: 14px;"),
                                     
                                     dirModuleUI("datafile")
                                   ),
                                   column(6,
                                          csvDecimal("datafile"))),
                                   csvIdentifier("datafile", "Unique identifier"),
                                   
                                   
                                   br(),
                                     selectInput(
                                       "method",
                                       "Choose your statistical method",
                                       choices = c("adj.p.val (FDR)" = "FDR", "p.value (raw)" = "None")
                                     )
                                 ),
                                 box(
                                   id = "boxpass2",
                                   title = strong("VOLCANO plot", style = "font-size:25px;"),
                                   width = NULL,
                                   background = "light-blue",
                                   
                                   fluidRow(column(6,
                                                   uiOutput("compvolc")),
                                            column(
                                              6,
                                              selectInput(
                                                "regulationvolc",
                                                #  Create a select list that can be used to choose a single or multiple items from a list of values.
                                                "Choose your regulation",
                                                choices = c("both", "up", "down")
                                              )
                                              
                                            )),
                                   
                                   fluidRow(column(
                                     6,
                                     sliderInput(
                                       'volcfc',
                                       "Choose your FC cutoff",
                                       min = 1,
                                       max = 10,
                                       step = 1,
                                       value = 1
                                     )
                                   ),
                                   column(
                                     6,
                                     sliderInput(
                                       'volcpval',
                                       "Choose your pval cutoff",
                                       min = 0.01,
                                       max = 0.05,
                                       step = 0.01,
                                       value = 0.05
                                     )
                                   )),
                                   fluidRow(column(
                                     6,
                                     sliderInput(
                                       'volclab',
                                       "Choose your lab size",
                                       min = 1,
                                       max = 6,
                                       step = 0.5,
                                       value = 3.0
                                     )
                                   ),
                                   column(
                                     6,
                                     sliderInput(
                                       'volcpt',
                                       "Choose your point size",
                                       min = 0.5,
                                       max = 3,
                                       step = 0.1,
                                       value = 1
                                     )
                                   )),
                                   
                                   
                                   
                                   
                                   div(
                                     id = "mytextvolc",
                                     p(
                                       " Highlight your selected gene(s) in the volcano plot with a comma-separated list of input "
                                     )
                                   ),
                                   
                                   textInput(
                                     inputId = "fillvolc",
                                     label = NULL,
                                     value = NULL,
                                     placeholder = "FOXP2,OT,AVPR1a",
                                     width = "100%"
                                   ),
                                   fluidRow(column(
                                     6,
                                     
                                     textInput(
                                       inputId = "findfamily",
                                       label = "Highlight a gene family",
                                       value = NULL,
                                       placeholder = "Cyp",
                                       width = "100%"
                                     )
                                   ),
                                   column(
                                     6,
                                     numericInput(
                                       'topvolc',
                                       'Max number of genes',
                                       NULL,
                                       min = 0,
                                       max = 5000
                                     )
                                   )),
                                   
                                   uiOutput("addcompvolc")
                                 )
                               ))
))))