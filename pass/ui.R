# info https://daattali.com/shiny/shinyalert-demo/

library(shiny)
library(shinyalert)

shinyUI(fluidPage(
  # Add Javascript
  useShinyalert(),
  
  textOutput("aa"),
  dataTableOutput(outputId = "dt2")
))
