#dt ext https://rstudio.github.io/DT/extensions.html
#DT ino: https://yihui.shinyapps.io/DT-info/
###
library(shiny)
library(shinydashboard)

title <- tags$a(href='https://www.google.com',
                tags$img(src="sabic.png", height = '55'), target="_blank")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = title #,titleWidth = '200px'
                  ,tags$li(class= 'dropdown',
                           tags$style(".main-header {max-height: 55px;}"),
                           tags$style(".main-header .logo {height: 55px;}"),
                           tags$style(".sidebar-toggle {height: 55px; padding-top: 18px !important;}"),
                           tags$style(".navbar {min-height:55px !important}"),
                           tags$style(".navbar-custom-menu, .main-header .navbar-right {float: left !important;}"),HTML( '<b><font color=#ffffff> <font size="6"> Strategic Workforce Planning Tool</font>'))
  ),
  dashboardSidebar(
    #####
    tags$style(".left-side, .main-sidebar {padding-top: 55px}"),
    #tags$style(".left-side, .main-sidebar {width: 200px}"),
    sidebarMenu(
      id = "tabs",
      menuItem("Instruction", icon = icon("info"), tabName = "tab1"),
      menuItem("Model Setup", icon = icon("glyphicon glyphicon-cog", lib ='glyphicon'), tabName = "tab2"),
      menuItem("Executive View", icon = icon("line-chart"), tabName = "tab3"),
      menuItem("Deep Dive View", icon = icon("glyphicon glyphicon-stats", lib ='glyphicon'), tabName = "tab4")
    )
    #####
  ),
  ######
  # end
  #dashbody                                
  dashboardBody(
    tabItems(
      #tab1
      #####
      tabItem(tabName = "tab1",includeCSS("styles.css"),
              h3('instruction')
      ),
      #####
      #end tab1
      #tab2
      ######
      tabItem(tabName = "tab2",
              fluidRow(
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Global Parameters</font></font>')
                  , status = "primary", solidHeader = TRUE,width = '100%',
                  tabBox(
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", height = "250px", width = '100%',
                    tabPanel("Training", "First tab content"),
                    tabPanel("Supply", "Tab content 2")
                    )
                  )
                ),
              fluidRow(
                box( width = '100%',status = "primary",column(2,selectInput("div",label=HTML('<font color=#454545>Division</font>'),choices=unique(iris$Species), multiple = TRUE)),
                     column(2,selectInput("rg",label=HTML('<font color=#454545>Region</font>'),choices=unique(iris$Species), multiple = TRUE)),
                     column(2,selectInput("Function Type",label=HTML('<font color=#454545>Function Type</font>'),choices=unique(iris$Species), multiple = TRUE)),
                     column(2,selectInput("Function Name",label=HTML('<font color=#454545>Function Name</font>'),choices=unique(iris$Species), multiple = TRUE)),
                     column(2,selectInput("Family",label=HTML('<font color=#454545>Family Name</font>'),choices=unique(iris$Species), multiple = TRUE)),
                     column(2,selectInput("Sub",label=HTML('<font color=#454545>Sub-Family</font>'),choices=unique(iris$Species), multiple = TRUE))
                )
              ),
              fluidRow(
                box(
                  title =HTML( '<font color=#ffffff> <font size="5">Specyfic Parameters</font></font>')
                  , status = "primary", solidHeader = TRUE,width = '100%',
                  tabBox(
                    # The id lets us use input$tabset1 on the server to find the current tab
                    id = "tabset1", height = "250px", width = '100%',
                    tabPanel("Demand", "First tab content"),
                    tabPanel("Supply", "Tab content 2")
                    )
                  )
                )
      ),
      ######
      #end tab2
      #tab3
      ######
      tabItem(tabName = "tab3",
               fluidRow(
                 box(
                 title =HTML( '<font color=#ffffff> <font size="5">Scenario Selection</font></font>')
                   , status = "primary", solidHeader = TRUE,
                tags$style(HTML(".radio-inline {margin-right: 20%;color:#262626;}")),
                radioButtons("radio1", label = NULL,
                 choiceNames = list("As-is", "Optimistic","Realistic"
                ),
                choiceValues = list(
                  "As-is", "Optimistic","Realistic"
                ),selected = "Realistic", inline = TRUE),width = '60%'
                 )
               ),
              box( status = "primary",column(2,selectInput("div",label=HTML('<font color=#454545>Division</font>'),choices=unique(iris$Species), multiple = TRUE)),
              column(2,selectInput("rg",label=HTML('<font color=#454545>Region</font>'),choices=unique(iris$Species), multiple = TRUE)),
              column(3,selectInput("Function Type",label=HTML('<font color=#454545>Function Type</font>'),choices=unique(iris$Species), multiple = TRUE)),
              column(3,selectInput("Function Name",label=HTML('<font color=#454545>Function Name</font>'),choices=unique(iris$Species), multiple = TRUE)),
              column(2,selectInput("Family",label=HTML('<font color=#454545>Family Name</font>'),choices=unique(iris$Species), multiple = TRUE))
              ,width = '100%'),
              fluidRow(
                column(3,selectInput("emp",label=HTML('<font color=#454545>Employment</font>'),choices=unique(iris$Species), multiple = TRUE)),        
                column(3,selectInput("seg",label=HTML('<font color=#454545>Segment</font>'),choices=unique(iris$Species), multiple = TRUE))
              ),
              fluidRow(
                box(status = "primary", radioButtons("radio2", label = HTML('<font color=#454545>Workforce Journey</font>'),
                             choiceNames = list("plot 1","plot 2"
                             ),
                             choiceValues = list(
                               "Optimistic","Realistic"
                             ), inline = TRUE),
                h1('plot')
              )
              ),
              fluidRow(
                box(status = "primary",
                    label='asd', h4('Scenario Comparison'),
                    h1('plot')
                )
              )
              ),
      ######
      #end
      #tab4
      ######
      tabItem(tabName = "tab4",
              tabsetPanel(id = "tabs2",   
                          tabPanel(
                            title = "Section Selection",icon = icon("glyphicon glyphicon-saved",lib ="glyphicon"),
                            value = "page1",
                            fluidRow(
                                     box(
                                       title =HTML( '<font color=#ffffff> <font size="5">Scenario Selection</font></font>')
                                       , status = "primary", solidHeader = TRUE,width = '100%',
                                       tags$style(HTML(".radio-inline {margin-right: 20%;color:#262626;}")),
                                       radioButtons("radio1", label = NULL,
                                                    choiceNames = list("As-is", "Optimistic","Realistic"
                                                    ),
                                                    choiceValues = list(
                                                      "As-is", "Optimistic","Realistic"
                                                    ),selected = "Realistic", inline = TRUE)
                                     )
                            ),
                            fluidRow(
                              box( width = '100%',status = "primary",column(2,selectInput("div",label=HTML('<font color=#454545>Division</font>'),choices=unique(iris$Species), multiple = TRUE)),
                                 column(2,selectInput("rg",label=HTML('<font color=#454545>Region</font>'),choices=unique(iris$Species), multiple = TRUE)),
                                 column(3,selectInput("Function Type",label=HTML('<font color=#454545>Function Type</font>'),choices=unique(iris$Species), multiple = TRUE)),
                                 column(3,selectInput("Function Name",label=HTML('<font color=#454545>Function Name</font>'),choices=unique(iris$Species), multiple = TRUE)),
                                 column(2,selectInput("Family",label=HTML('<font color=#454545>Family Name</font>'),choices=unique(iris$Species), multiple = TRUE))
                                 )
                              ),
                            fluidRow(
                              column(3,selectInput("emp",label=HTML('<font color=#454545>Employment</font>'),choices=unique(iris$Species), multiple = TRUE)),        
                              column(3,selectInput("seg",label=HTML('<font color=#454545>Segment</font>'),choices=unique(iris$Species), multiple = TRUE))
                            )
                            
                          ),
                          tabPanel("Section Summary", icon = icon("line-chart") 
                                   
                          ),
                          tabPanel("Workflow Jurney", icon = icon("line-chart"), 
                                   fluidRow(
                                     box(status = "primary", radioButtons("radio2", label = HTML('<font color=#454545>Workforce Journey</font>'),
                                                                          choiceNames = list("plot 1","plot 2"
                                                                          ),
                                                                          choiceValues = list(
                                                                            "Optimistic","Realistic"
                                                                          ), inline = TRUE),
                                         h1('plot')
                                     )
                                   )
                          ),
                          tabPanel("Workflow Forcast", icon = icon("line-chart") 
                                     
                          )
                          )
      )
      
      ######
      #end

      )
  )
  )
##########

# Define server logic required to draw a histogram
server <- function(input, output) {
   

}

# Run the application 
shinyApp(ui = ui, server = server)

