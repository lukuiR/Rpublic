#dt ext https://rstudio.github.io/DT/extensions.html
#DT ino: https://yihui.shinyapps.io/DT-info/
###
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(plotly)
library(readxl)
library(data.table)
library(DT)

source('prep_dat.R')

title <- tags$a(href='https://www.sabic.com/en',
                tags$img(src="sabic.png", height = '55'), target="_blank")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  skin = "black",
  dashboardHeader(title = title ,titleWidth = 200
                  ,tags$li(class= 'dropdown',
                           tags$style(".main-header {max-height: 55px;}"),
                           tags$style(".main-header .logo {height: 55px;}"),
                           tags$style(".sidebar-toggle {height: 55px; padding-top: 18px !important;}"),
                           tags$style(".navbar {min-height:55px !important}"),
                           tags$style(".navbar-custom-menu, .main-header .navbar-right {float: left !important;}"),HTML( '<b><font color=#ffffff> <font size="6"> Strategic Workforce Planning Tool</font></font></b>'))
  ),
  dashboardSidebar(
    width = 200,
    #####
    tags$style(".left-side, .main-sidebar {padding-top: 55px}"),
    #tags$style(".left-side, .main-sidebar {width: 200px}"),
    sidebarMenu(
      id = "tabs",
      menuItem("Instruction", icon = icon("info"), tabName = "tab1"),
      menuItem("Model Setup", icon = icon("glyphicon glyphicon-cog", lib ='glyphicon'), tabName = "tab2"),
      menuItem("Executive View", icon = icon("line-chart"), tabName = "tab3"),
      menuItem("Deep Dive View", icon = icon("glyphicon glyphicon-stats", lib ='glyphicon'), tabName = "tab4"),
      menuItem("Data View", icon = icon("glyphicon glyphicon-folder-open", lib ='glyphicon'), tabName = "tab5")
    )
    #####
  ),
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
                    id = "tabset1", width = '100%',# height = "250px",
                    tabPanel("Training",
                             {
                             box(status = "primary", width = '100%',
                                 actionButton("reset_ld","Reset L&D"),
                                 fluidRow(
                                   dataTableOutput(outputId = "tld")
                                 ),
                                 fluidRow(
                                   column(2,br(),br(),h5('Total L&D Spend / FTE')),
                                   column(2,
                                          textInput("MEA", h5("Year 1 Value"),
                                                    value = "1250")) ,
                                   column(2,
                                          textInput("APAC", h5("Year 2 Value"), 
                                                    value = "1300")) ,
                                   column(2,
                                          textInput("AMR", h5("Year 3 Value"),
                                                    value = "2000")) ,
                                   column(2,
                                          textInput("EUR", h5("Year 4 Value"),
                                                    value = "2000")) ,
                                   column(2,
                                          textInput("EUR", h5("Year 5 Value"),
                                                    value = "2000"))
                                   )
                                 )
                             }
                             ),
                    tabPanel("Supply", 
                             box(status = "primary",width = 12,
                                 actionButton("reset_nhr","Reset Demand NHR"),
                                 fluidRow(
                                   dataTableOutput(outputId = "tnhr")
                                 ),
                               {fluidRow(
                               column(1,br(),br(),h5(2018)),
                               
                               column(2, 
                                      textInput("MEA", h5("MEA"), 
                                                value = "1.1")) ,
                               
                               column(2, 
                                      textInput("APAC", h5("APAC"), 
                                                value = "1.2")) ,
                               
                               column(2, 
                                      textInput("AMR", h5("AMR"), 
                                                value = "20")) ,
                               
                               column(2, 
                                      textInput("EUR", h5("EUR"), 
                                                value = "4"))   
                             )},
                               {fluidRow(
                                 column(1,h5(2019)),
                                 
                                 column(2, 
                                        textInput("MEA",NULL,# h5("MEA"), 
                                                  value = "1.1")) ,
                                 
                                 column(2, 
                                        textInput("APAC",NULL,# h5("APAC"), 
                                                  value = "1.2")) ,
                                 
                                 column(2, 
                                        textInput("AMR",NULL,# h5("AMR"), 
                                                  value = "20")) ,
                                 
                                 column(2, 
                                        textInput("EUR",NULL,# h5("EUR"), 
                                                  value = "4"))   
                               )
                             },
                               {fluidRow(
                                 column(1,h5(2020)),
                                 
                                 column(2, 
                                        textInput("MEA",NULL,# h5("MEA"), 
                                                  value = "1.1")) ,
                                 
                                 column(2, 
                                        textInput("APAC",NULL,# h5("APAC"), 
                                                  value = "1.2")) ,
                                 
                                 column(2, 
                                        textInput("AMR",NULL,# h5("AMR"), 
                                                  value = "20")) ,
                                 
                                 column(2, 
                                        textInput("EUR",NULL,# h5("EUR"), 
                                                  value = "4"))   
                               )
                             }),
                             #end
                             #knobInput nhr
                             {
                             fluidRow(
                               box(title="Supply rate adjustments:",solidHeader = TRUE, status="primary",colour="light blue",
                                   fluidRow(column(6,
                                                   knobInput(
                                                     inputId = "hire_rate_y1",label = "Year 1 hire percentage:",
                                                     value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                                                     fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                            column(6,
                                                   knobInput(
                                                     inputId = "exit_rate_y1",label = "Year 1 exit percentage:",
                                                     value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                                                     fgColor = "#c9413f",angleOffset = -135,angleArc=270,inputColor = "#c9413f"))
                                   ),
                                   fluidRow(column(6,
                                                   knobInput(
                                                     inputId = "hire_rate_y2",label = "Year 2 hire percentage:",
                                                     value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                                                     fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                            column(6,
                                                   knobInput(
                                                     inputId = "exit_rate_y2",label = "Year 2 exit percentage:",
                                                     value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                                                     fgColor = "#c9413f",angleOffset = -135,angleArc=270,inputColor = "#c9413f"))
                                   ),
                                   fluidRow(column(6,
                                                   knobInput(
                                                     inputId = "hire_rate_y3",label = "Year 3 hire percentage:",
                                                     value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                                                     fgColor = "#42cb69",angleOffset = -135,angleArc=270,inputColor = "#42cb69")),
                                            column(6,
                                                   knobInput(
                                                     inputId = "exit_rate_y3",label = "Year 3 exit percentage:",
                                                     value = 0,min = 0,max=50,displayPrevious = TRUE,lineCap = "round",
                                                     fgColor = "#c9413f",angleOffset = -135,angleArc=270,inputColor = "#c9413f"))
                                   ),
                                   width=12
                               ))
                             }
                             #knf
                             
                             )
                    )
                  )
                ),
              fluidRow(
                box( width = 14,status = "primary",column(2,selectInput("div",label='Division',choices=unique(db$Division), multiple = TRUE)),
                     column(2,selectInput("rg",label=HTML('<font color=#454545>Region</font>'),choices=unique(db$Region), multiple = TRUE)),
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
              column(2,selectInput("rg1",label=HTML('<font color=#454545>Region</font>'),choices=unique(iris$Species), multiple = TRUE)),
              column(3,selectInput("Function Type",label=HTML('<font color=#454545>Function Type</font>'),choices=unique(iris$Species), multiple = TRUE)),
              column(3,selectInput("Function Name",label=HTML('<font color=#454545>Function Name</font>'),choices=unique(iris$Species), multiple = TRUE)),
              column(2,selectInput("Family",label=HTML('<font color=#454545>Family Name</font>'),choices=unique(iris$Species), multiple = TRUE))
              ,width = '100%'),
              fluidRow(
                column(3,selectInput("emp",label=HTML('<font color=#454545>Employment</font>'),choices=unique(iris$Species), multiple = TRUE)),        
                column(3,selectInput("seg",label=HTML('<font color=#454545>Segment</font>'),choices=unique(iris$Species), multiple = TRUE))
              ),
              fluidRow(
                box(status = "primary", width = 12, 
                    radioButtons("radio2", label = HTML('<font color=#454545>Workforce Journey</font>'),
                             choiceNames = list("plot 1","plot 2"
                             ),
                             choiceValues = list(
                               "Optimistic","Realistic"
                             ), inline = TRUE),
                h1('plot'),plotlyOutput("plot1")
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
                                 column(2,selectInput("rg2",label=HTML('<font color=#454545>Region</font>'),choices=unique(iris$Species), multiple = TRUE)),
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
      ),
      ######
      #end
      #tab5
      ######
      tabItem(tabName = "tab5",
              conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                               tags$div("Loading...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                                  <span class="sr-only">Loading...</span>'),id="loadmessage")),
              align = "center",
              uiOutput("KKosz")
              )
      ######
      #end
      )
  )
  )
##########

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  observeEvent(input$reset_dd,{
    #f_pull_dd(rvalues$dd_raw)
  })

  #plot
  #####
  output$plot1<- renderPlotly({
  x <- c('1Current<br>headcount', '1Current<br>headcount', '2Addition',  '3Substraction', '4Automation', '5Final')
  y <- c(300, 410, 660,  590, 400,  340)
  base <- c(0, 0, 430,  570, 370,  0)
  revenue <- c(320, 110, 260,  0, 0,  0)
  costs <- c(0, 0, 0,  120, 200, 0)
  profit <- c(0, 0, 0, 0, 0, 370)
  text <- c('$320K','$430K', '$260K', '$-120K', '$-200K', '$370K')
  data <- data.frame(x, base, revenue, costs, profit, text)
  
  #The default order will be alphabetized unless specified as below:
  
  plot_ly(data, x = ~x, y = ~base, type = 'bar', marker = list(color = 'rgba(1,1,1, 0.0)'))%>%
    add_trace(y = ~revenue, marker = list(color = 'rgba(55, 128, 191, 0.7)',
                                          line = list(color = 'rgba(55, 128, 191, 0.7)',
                                                      width = 2))) %>%
    add_trace(y = ~costs, marker = list(color = 'rgba(219, 64, 82, 0.7)',
                                        line = list(color = 'rgba(219, 64, 82, 1.0)',
                                                    width = 2))) %>%
    add_trace(y = ~profit, marker = list(color = 'rgba(50, 171, 96, 0.7)',
                                         line = list(color = 'rgba(50, 171, 96, 1.0)',
                                                     width = 2))) %>%
    layout(title = 'Headcount',
           xaxis = list(title = ""),
           yaxis = list(title = ""),
           barmode = 'stack',
           #  paper_bgcolor = 'rgba(245, 246, 249, 1)',
           # plot_bgcolor = 'rgba(245, 246, 249, 1)',
           showlegend = FALSE) %>%
    add_annotations(text = text,
                    x = x,
                    y = y,
                    xref = "x",
                    yref = "y",
                    font = list(family = 'Arial',
                                size = 14,
                                color = 'rgba(245, 246, 249, 1)'),
                    showarrow = FALSE)
  })
  #####
  #end
  
  #table
  #####
  
  #nhr
  #####
  output$tnhr<- renderDataTable({
    datatable(nhr,rownames = FALSE,
              options = list(
                #dom = 'Bfrti',
                autoWidth = FALSE,
                deferRender = TRUE,
                scrollX = TRUE,
                scrollY = "51vh",
                scroller = TRUE,
                scollCollapse = TRUE,
                fixedHeader = TRUE,
                columnDefs = list(
                  list(orderable = FALSE, className = 'details-control', targets = 0)
                )
              ) ,escape=F
    )%>%
      formatPercentage(c('MEA','AMR','EUR','APAC'), 2)
  })
  #####
  #end
  #ld
  #####
  output$tld<- renderDataTable({
    datatable(ld,rownames = FALSE,
              options = list(
                #dom = 'Bfrti',
                autoWidth = FALSE,
                deferRender = TRUE,
                scrollX = TRUE,
                scrollY = "51vh",
                scroller = TRUE,
                scollCollapse = TRUE,
                fixedHeader = TRUE,
                columnDefs = list(
                  list(orderable = FALSE, className = 'details-control', targets = 0)
                )
              ) ,escape=F
    )#%>%
      #formatCurrency(c('MEA','AMR','EUR','APAC'), 2)
  })
  #####
  #end
  
  #vals preparation
  ######
  vals=reactiveValues()
  vals$Data=data.table(db)
  
  newEntry1 <- observe(vals$Data <- mutate(vals$Data, 
                                           M_NHR= case_when(Region == "MEA" ~ input$MEA,  Region == "APAC" ~ input$APAC,
                                                            Region == "AMR" ~ input$AMR, Region == "EUR" ~ input$EUR)
  )
  )
  newEntry <- observe({vals$Data <- data.table(db[db$Region %in%  if(is.null(input$rg)){unique(db$Region)} else (input$rg),])
  vals$Data <- data.table(db[db$Division %in%  if(is.null(input$div)){unique(db$Division)} else (input$div) ,])
  })
  ######
  #end vals
  #####
  output$KKosz=renderUI({
    box(width=12,
        column(12,dataTableOutput("Main_kosz")),
        tags$script(HTML('$(document).on("click", "input", function () {
                         var checkboxes = document.getElementsByName("row_selected");
                         var checkboxesChecked = [];
                         for (var i=0; i<checkboxes.length; i++) {
                         if (checkboxes[i].checked) {
                         checkboxesChecked.push(checkboxes[i].value);
                         }
                         }
                         Shiny.onInputChange("checked_rows",checkboxesChecked);})')),
        tags$script("$(document).on('click', '#Main_kosz button', function () {
                    Shiny.onInputChange('lastClickkId',this.id);
                    Shiny.onInputChange('lastClickk', Math.random()) });")
        
        )
    })
  
  output$Main_kosz=renderDataTable({ DT=vals$Data[] 
  
  datatable(DT[],
            options = list(
              #dom = 'Bfrti',
              autoWidth = FALSE,
              deferRender = TRUE,
              scrollX = TRUE,
              scrollY = "51vh",
              scroller = TRUE,
              scollCollapse = TRUE,
              fixedHeader = TRUE,
              columnDefs = list(
                list(orderable = FALSE, className = 'details-control', targets = 0)
              )
            ) ,escape=F)
  
  })
  
  #####action showModal
  observeEvent(input$lastClickk,
               {
                 showModal(modal_modify)
               }
  )
  #####
  ##modalDialog
  #####
  modal_modify=modalDialog(
    fluidPage(
      h3(strong("Row modification"),align="center"),
      hr(),
      dataTableOutput('row_modif'),
      actionButton("save_changes","Save"),
      tags$script(HTML("$(document).on('click', '#save_changes', function () {
                       var list_value=[]
                       for (i = 0; i < $( '.new_input' ).length; i++)
                       {
                       list_value.push($( '.new_input' )[i].value)
                       }
                       Shiny.onInputChange('newValue', list_value)
                       });"
                           ))
      
      
      ),
    size="m"
    )
  #####
  
  ##co modyfikuje
  output$row_modif<-renderDataTable({
    selected_row=as.numeric(gsub("modify_","",input$lastClickkId))
    old_row=vals$Data[selected_row,c('n','Manual')]
    row_change=list()
    row_change[[1]]=vals$Data[selected_row,c('n')]
    row_change[[2]]=paste0('<input class="new_input" type="number" id=new_',2,'><br>')
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT=rbind(old_row,row_change)
    rownames(DT)<-c("Current values","New values")
    DT
    
  },escape=F,options=list(dom='t',ordering=F),selection="none")
  
  ##data table update by modification
  ######
  observeEvent(input$newValue,
               {
                 newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 #dataset()[as.numeric(gsub("modify_","",input$lastClickkId)),'Manual']<-DF
                 
                 vals$Data[as.numeric(gsub("modify_","",input$lastClickkId)),'Manual']<-DF
               }
  )
  ######
  
  #####
  #end tb
}

# Run the application 
shinyApp(ui = ui, server = server)

