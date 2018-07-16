#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(survival)
library(plotly)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Surv",
                                    tags$li(class = "dropdown", 
                                            tags$a(href="http://www.sbim-paris7.com/", target="_blank", 
                                            tags$img(height = "20px", alt="SNAP Logo", src='sbim.png')
                                    )      )          ),
                    dashboardSidebar(
                      # specify number of tabs               
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Start", icon = icon("info"), tabName = "dash1"
                        ),
                        #tab 1 R Conference  
                        menuItem("R Conference", icon = icon("calendar"), tabName = "dashboard2"),
                        # Year filter               
                        selectInput("cont",label="Choose the continent",choices=unique(veteran$age),multiple = TRUE)
                      )
                    ),
                    # dashboard body  
                    dashboardBody(includeCSS("styles.css"),
                                  tabItems(
                                    #1st tab with informations mainly text with additional css styling
                                    tabItem(tabName = "dash1",includeCSS("styles1.css"),
                                            img(src='sbim.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;"),
                                            h4(HTML('<b>"Free ticket to eRum"</b>'), align = "center"),
                                            h4(HTML("So... big news."),align = "left"),
                                            h4(HTML("Jumping Rivers is sponsoring eRum 2018 and in light of this news we are giving away a free place at the conference!    </br>    (Not to mention our very own lead consultant, Colin Gillespie, is one of the invited speakers.)  </br> More information can be found on page:"),align = "left"),
                                            h4(HTML('<a href="http://www.sbim-paris7.com/"><b>sbim-paris7.com</b></a>'), align = "center")
                                            ,align = "center",
                                            h4(HTML('<b>"The Main Competition"</b>'), align = "center"),
                                            h4(HTML('Here at Jumping Rivers, we maintain the site <a href="https://jumpingrivers.github.io/meetingsR/"><b>meetingsR</b></a>. This comprises of three comprehensive lists:
                                                    </br>1. All upcoming (and foregone) R conferences.
                                                    </br>2. All R useR groups from around the globe.
                                                    </br>3. All R-Ladies groups from around the globe.
                                                    </br>See the <a href="https://github.com/jumpingrivers/meetingsR/"><b>GitHub repo</b></a> for the contents.'),align = "left"),
                                            h6(HTML('Copyright <a href="https://pl.linkedin.com/in/łukasz-janiszewski-390a1660/"><b>Łukasz Janiszewski</b></a>'), align = "center")
                                            
                                            ),
                                    
                                    #2nd tab, Information about R Conference
                                    tabItem(tabName = "dashboard2",includeCSS("styles1.css"),
                                            
                                            img(src='sbim.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;")
                                            ,
                                            h4("R Conference")
                                            ,
                                            fluidRow(
                                              h2("Location", align = "center")
                                            ),br(),
                                            #Year checkbox filter
                                            checkboxGroupInput("yearr", 
                                                               h3("Year"), 
                                                               choices = list("2016" = 2016, "2017" = 2017, "2018" = 2018, "2019" = 2019), inline =TRUE,
                                                               selected = c(2018) #at start automatically selected 2018, can be remove or replace
                                            ),
                                            
                                            br(),
                                            fluidRow( 
                                              #loading animation when system busy
                                              conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                                                               tags$div("Loading...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                                                              <span class="sr-only">Loading...</span>'),id="loadmessage")), #end
                                               
                                              align = "center",                      
                                              fluidRow(
                                                h2("Select pin to view details"), 
                                                plotlyOutput("plot")),
                                              fluidRow(
                                                h2("Select pin to view details"), 
                                                plotlyOutput("plot2")),
                                              fluidRow(box(dataTableOutput(outputId = "tb1")) ,align = "center"
                                              )
                                              ,h6(HTML('Copyright <a href="https://pl.linkedin.com/in/łukasz-janiszewski-390a1660/"><b>Łukasz Janiszewski</b></a>'), align = "center")                                     
                                                               )
                                            # 2nd tab with useR information same structure like before, one change no year filter
                                            
                                            )                            
                                    )
                                    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$tb1 <- renderDataTable(veteran, 
                                   options = list(scrollX = TRUE, autoWidth = TRUE))
   
  output$plot <- renderPlotly({
    km_trt2_fit <- survfit(Surv(time, status)~1 , data=veteran[veteran$trt==2,])
    km_trt1_fit <- survfit(Surv(time, status)~1 , data=veteran[veteran$trt==1,])
    
    plot_ly(x=~km_trt2_fit$time,y=~km_trt2_fit$upper, type ="scatter",mode = "lines",  name = 'Upper 95% trt 2',showlegend = FALSE,
            line = list(color = 'transparent'))%>% 
      add_trace(x=~km_trt2_fit$time,y=km_trt2_fit$lower,
                fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
                showlegend = TRUE, name = 'Lower 95% trt 2') %>%
      add_trace(x=~km_trt2_fit$time,y=km_trt2_fit$surv,line = list(color='rgb(0,100,80)'),
                showlegend = TRUE, name = 'Survive trt 2')%>% 
      add_trace(x=~km_trt1_fit$time,y=~km_trt1_fit$upper, type ="scatter",mode = "lines",  name = 'Upper 95% trt 1',showlegend = FALSE,
                line = list(color = 'transparent'))%>% 
      add_trace(x=~km_trt1_fit$time,y=km_trt1_fit$lower,
                fill = 'tonexty', fillcolor='rgba(100,0,80,0.2)', line = list(color = 'transparent'),
                showlegend = TRUE, name = 'Lower 95% trt1') %>%
      add_trace(x=~km_trt1_fit$time,y=km_trt1_fit$surv,line = list(color='rgb(100,0,80)'),
                showlegend = TRUE, name = 'Survive trt 1')%>% 
      layout(margin = list(b = 80, l = 80),xaxis = list(title= "Survival time"),yaxis = list(title= "Surv %",tickformat = "%"))
    
  })


output$plot2 <- renderPlotly({
  km_trt2_fit <- survfit(Surv(time, status)~1 , data=veteran[veteran$age>60,])
  km_trt1_fit <- survfit(Surv(time, status)~1 , data=veteran[veteran$age<=60,])
  
  plot_ly(x=~km_trt2_fit$time,y=~km_trt2_fit$upper, type ="scatter",mode = "lines",  name = 'Upper 95% ov60',showlegend = FALSE,
          line = list(color = 'transparent'))%>% 
    add_trace(x=~km_trt2_fit$time,y=km_trt2_fit$lower,
              fill = 'tonexty', fillcolor='rgba(0,100,80,0.2)', line = list(color = 'transparent'),
              showlegend = TRUE, name = 'Lower 95% ov60') %>%
    add_trace(x=~km_trt2_fit$time,y=km_trt2_fit$surv,line = list(color='rgb(0,100,80)'),
              showlegend = TRUE, name = 'Survive ov60')%>% 
    add_trace(x=~km_trt1_fit$time,y=~km_trt1_fit$upper, type ="scatter",mode = "lines",  name = 'Upper 95% lt60',showlegend = FALSE,
              line = list(color = 'transparent'))%>% 
    add_trace(x=~km_trt1_fit$time,y=km_trt1_fit$lower,
              fill = 'tonexty', fillcolor='rgba(100,0,80,0.2)', line = list(color = 'transparent'),
              showlegend = TRUE, name = 'Lower 95% lt60') %>%
    add_trace(x=~km_trt1_fit$time,y=km_trt1_fit$surv,line = list(color='rgb(100,0,80)'),
              showlegend = TRUE, name = 'Survive lt60')%>% 
    layout(margin = list(b = 80, l = 80),xaxis = list(title= "Survival time"),yaxis = list(title= "Surv %",tickformat = "%"))
  
})
}

# Run the application 
shinyApp(ui = ui, server = server)

