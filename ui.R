library(shiny)
shinyUI(fluidPage(
  uiOutput("topbar"),
  fluidRow(uiOutput("tweak2"),
           uiOutput("controlsPanel"),
           column(width = 5, plotOutput("plot")))
  
  ))