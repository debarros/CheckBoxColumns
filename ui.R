library(shiny)
shinyUI(fluidPage(
  uiOutput("topbar"),
  fluidRow(uiOutput("tweak2"),
           column(width = 4, uiOutput("controls")),
           column(width = 8, plotOutput("plot")))
  
  ))