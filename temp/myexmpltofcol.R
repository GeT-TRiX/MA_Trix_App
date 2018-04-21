my_checkboxGroupInput <- function(variable, label, choices, selected, colors){
  choices_names <- choices
  if(length(names(choices))>0) choices_names <- names(choices)
  my_colors <- rep("black", length(choices))
  is_selected <- choices %in% selected
  my_colors[is_selected] <- colors[1:sum(is_selected)]
  div(id=variable,class="form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
      HTML(paste0('<label class="control-label" for="',variable,'">',label,'</label>')),
      div( class="shiny-options-group",
           HTML(paste0('<div class="checkbox" style="color:', my_colors, '">',
                       '<label>',
                       '<input type="checkbox" name="', variable, 
                       '" value="', choices, 
                       '"', ifelse(is_selected, 'checked="checked"', ''), 
                       '/>',
                       '<span>', choices_names,'</span>',
                       '</label>',
                       '</div>', collapse = " "))
      )
  )
}


my_names <- c('one','two','three','four','five','six')
my_selected <- c('one','two','three','four','five','six')
my_colors <-c('blue','red','green','purple','lemon','brown')

shinyApp(ui=fluidPage(uiOutput("my_cbgi")),
         
         server = function(input, output, session) {
           my <- reactiveValues(selected=my_selected, firt_call_ignore=TRUE)
           output$my_cbgi <- renderUI(my_checkboxGroupInput("variable", "Variable:",
                                                            choices = my_names, 
                                                            selected=my$selected,
                                                            colors=my_colors ))
           observeEvent(input$variable,{
             if(my$firt_call_ignore)
               my$firt_call_ignore=FALSE
             else
               my$selected <- input$variable
           }, ignoreNULL = FALSE)
         })