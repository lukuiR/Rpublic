library(shiny)
library(shinydashboard)



dashboardPage(
  dashboardHeader(title = "Drilldown", titleWidth = 400),
  
  dashboardSidebar(
  ),
  
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$style(HTML("
                      .nav-tabs {
                      visibility:hidden;
                      }
                      
                      "))
      ),
    tabsetPanel(id = "tabs",
                tabPanel("Main",
                         dataTableOutput("tabela"))
    )
      )
    )