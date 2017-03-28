library(shiny)

shinyServer(function(input, output) {
  
  #it may be better to set the column width, or to set it to auto
  # http://www.w3schools.com/css/css3_multiple_columns.asp
  # http://www.w3schools.com/cssref/css3_pr_column-width.asp
  # http://codepen.io/cimmanon/pen/CcGlE
  
  tweaks = reactive ({
    x = paste0(".multicol { -moz-column-fill: auto; -column-fill: auto;",
      "-webkit-column-count: ",input$numberOfColumns,
      "; -moz-column-count: ",input$numberOfColumns,
      "; column-count: ",input$numberOfColumns,";",
      "position: relative;",
      "max-height: ", input$HeightOfColumns ,"vh;","}")
    return(list(tags$head(tags$style(HTML(x)))))
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
  
  boxes = reactive({tagList(
    checkboxGroupInput(
    inputId  = 'numSelector', 
    label    = "Select the numbers:", 
    choices  = all_rows(),
    selected = all_rows(),
    inline   = F))
  })
  
  output$controls = renderUI({list(
    h3("Multicolumn checkboxGroupInput"), 
    tags$div(
      style = "position: relative;",
      align = 'left', 
      class = 'multicol', 
      tagList(boxes())
    )) })
  
  
  output$boxcount = renderUI({
    print("boxcount")
    numericInput(inputId = "numberOfCheckboxes",label = "How many boxes?",value = 25)})
  output$colcount = renderUI({
    print(paste0("colcount"))
    return(numericInput(inputId = "numberOfColumns",label = "How many columns?",value = 2))})
  output$HeightOrColumns = renderUI({
    return(checkboxInput(inputId = "useHeight",label = "Define by Height or #Cols?"))})
  output$ColumnHeight = renderUI({
    return(numericInput(inputId = "HeightOfColumns",label = "How tall the columns?",value = 70))})
  
  
  output$controlsPanel = renderUI({
    return(tagList(column(width = 7, wellPanel(uiOutput("controls")))))
    })
    
  
  
  output$topbar = renderUI({
    print("topbar")
    tagList(fluidRow(
      column(width = 6, isolate(uiOutput("HeightOrColumns"))),
      column(width = 6, isolate(uiOutput("ColumnHeight"))),
      column(width = 6, isolate(uiOutput("boxcount"))),
      column(width = 6, isolate(uiOutput("colcount"))) )) 
  }) 
})