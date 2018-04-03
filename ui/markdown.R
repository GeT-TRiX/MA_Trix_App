tabPanel(p(icon("question-circle"),
           "How to use?"),
         mainPanel(includeMarkdown("markdown/help.md"))),
tabPanel(p(icon("info-circle"),
           "About"),
         mainPanel(includeMarkdown("markdown/about.md")))