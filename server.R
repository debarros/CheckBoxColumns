library(shiny)

shinyServer(function(input, output) {
  
  tweaks = reactive ({
    list(tags$head(tags$style(HTML(paste0(
      ".multicol {-webkit-column-count: ",input$numberOfColumns,"; /* Chrome, Safari, Opera */ 
                  -moz-column-count: ",input$numberOfColumns,";    /* Firefox */ 
                  column-count: ",input$numberOfColumns,"; 
                  -moz-column-fill: auto;
                  -column-fill: auto;}"
    )))))
  })
  
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
  
  ColCountPersist = reactive({
    if(is.null(input$numberOfColumns)){return(2)
    }else return(input$numberOfColumns)
  })
  
  BoxCountPersist = reactive({
    if(is.null(input$numberOfCheckboxes)){return(25)
    }else return(input$numberOfCheckboxes)
  })
  
  output$boxcount = renderUI({numericInput(inputId = "numberOfCheckboxes",label = "How many boxes?",value = BoxCountPersist())})
  output$colcount = renderUI({numericInput(inputId = "numberOfColumns",label = "How many columns?",value = ColCountPersist())})
  
  output$topbar = renderUI({tagList( 
    fluidRow(
      column(width = 6, isolate(uiOutput("boxcount"))),
      column(width = 6, isolate(uiOutput("colcount"))) )) 
  }) 
  
  output$wholepage = renderUI({
    fluidPage(
      tweaks(),
      uiOutput("topbar"),
      fluidRow(
        column(width = 4, uiOutput("controls")),
        column(width = 8, plotOutput("plot"))))
  })
})