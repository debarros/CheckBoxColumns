library(shiny)

shinyServer(function(input, output) {
  
  #it may be better to set the column width, or to set it to auto
  # http://www.w3schools.com/css/css3_multiple_columns.asp
  # http://www.w3schools.com/cssref/css3_pr_column-width.asp
  # http://codepen.io/cimmanon/pen/CcGlE
  
  tweaks = reactive ({
    list(tags$head(tags$style(HTML(paste0(
      ".multicol {-webkit-column-count: ",input$numberOfColumns,"; /* Chrome, Safari, Opera */ 
                  -moz-column-count: ",input$numberOfColumns,";    /* Firefox */ 
                  column-count: ",input$numberOfColumns,"; 
                  -moz-column-fill: auto;
                  -column-fill: auto;}"
    )))))
  })
  
  
  output$tweak2 = renderUI(tagList(tweaks()))
  
  all_rows = reactive({
    if(is.null(input$numberOfCheckboxes)){return(1:25)
      } else {
    x = 1:input$numberOfCheckboxes
    names(x) = paste("Row",x)
    return(x)}
  })
  
  plot_data <- reactive(input$numSelector)
  
  output$plot <- renderPlot({plot(
    x = plot_data(), 
    y = plot_data(), 
    pch = 6, 
    cex = 2, 
    xlim = c(1, 50), 
    ylim = c(1, 50) )
  })
  
  boxes = reactive({checkboxGroupInput(
    inputId  = 'numSelector', 
    label    = "Select the numbers:", 
    choices  = all_rows(),
    selected = all_rows(),
    inline   = FALSE)
  })
  
  output$controls = renderUI({list(
    h3("Multicolumn checkboxGroupInput"), 
    tags$div(
      align = 'left', 
      class = 'multicol', 
      tagList(boxes())
    )) })
  

  output$boxcount = renderUI({
    print("boxcount")
    numericInput(inputId = "numberOfCheckboxes",label = "How many boxes?",value = 25)})
  output$colcount = renderUI({
    print(paste0("colcount"))
    return(numericInput(inputId = "numberOfColumns",label = "How many columns?",value = 2))
    })
  
  output$topbar = renderUI({
    print("topbar")
    tagList(fluidRow(
      column(width = 6, isolate(uiOutput("boxcount"))),
      column(width = 6, isolate(uiOutput("colcount"))) )) 
  }) 
})