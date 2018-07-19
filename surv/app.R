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
library(DT)

# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Surv",
                                    tags$li(class = "dropdown", 
                                            tags$a(href="http://www.sbim-paris7.com/", target="_blank", 
                                            tags$img(height = "15px", alt="SNAP Logo", src='sbim.png')
                                    )      )          ),
                    dashboardSidebar(
                      # specify number of tabs               
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Introduction", icon = icon("info"), tabName = "dash1"
                        ),
                        #Analysis 
                        menuItem("Analysis", icon = icon("line-chart"), tabName = "dashboard2")
                      )
                    ),
                    # dashboard body  
                    dashboardBody(includeCSS("styles.css"),
                                  tabItems(
                                    #1st tab with informations mainly text with additional css styling
                                    {tabItem(tabName = "dash1",
                                            img(src='sbim.png', width='300px' ,style="display: block; margin-left: auto; margin-right: auto;"),
                                            h4(HTML('<b>"Survival Package Demo"</b>'), align = "center"),
                                            h4(HTML("This app is small presentation for data visualisation prepare for institute in Paris. </br> More information can be found on page:"),align = "left"),
                                            h4(HTML('<a href="http://www.sbim-paris7.com/"><b>sbim-paris7.com</b></a>'), align = "center")
                                            ,align = "center",
                                            h4(HTML("This demo was prepare, with big influence of Joseph Rickert's post, which can be found",'<a href="https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/"><b>here.</b></a>'),align = "left"),
                                            h4(HTML("I have tried to implement those ideas of veterans dataset analysis in form of presented app, which can be hosted thanks to RStudio and its free shiny server."), align = "left"),
                                            h4(HTML("Our object of interes is veteran database included in Survival package (data sample):"), align = "left"),
                                            fluidRow( 
                                              #loading animation when system busy
                                              conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                                                               tags$div("Loading...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                                                          <span class="sr-only">Loading...</span>'),id="loadmessage")), #end
                                              
                                              align = "center"),
                                            dataTableOutput('view') ,align = "center",
                                            br(),
                                            verbatimTextOutput("summary"),
                                            h4(HTML("The variables in <code>veteran</code> are: * <code>trt</code>: 1=standard 2=test * <code>celltype</code>: 1=squamous, 2=small cell, 3=adeno, 4=large * <code>time</code>: survival time in days * <code>status</code>: censoring status * <code>karno</code>: Karnofsky performance score (100=good) * <code>diagtime</code>: months from diagnosis to randomization * <code>age</code>: in years * <code>prior</code>: prior therapy 0=no, 10=yes"), align = "left"),
                                            
                                            h4(HTML("Please go to next tab, which can be found on the left in navigation bar."), align = "left"),
                                            br(),
                                            h6(HTML('Copyright <a href="https://pl.linkedin.com/in/łukasz-janiszewski-390a1660/"><b>Łukasz Janiszewski</b></a>'), align = "center")
                                            
                                            )
                                    }
                                    ,
                                    
                                    #2nd tab, Analysis
                                    {tabItem(tabName = "dashboard2",
                                            
                                            img(src='sbim.png', width='300px' ,style="display: block; margin-left: auto; margin-right: auto;")
                                            ,
                                            h4("R Analysis of veteran dataset from survival package")
                                            ,
                                            fluidRow( 
                                              #loading animation when system busy
                                              conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                                                               tags$div("Loading...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                                                          <span class="sr-only">Loading...</span>'),id="loadmessage")) #end
                                            ),
                                              align = "center",                      
                                              fluidRow(
                                                h2("plot1"), 
                                                plotlyOutput("plot1")),br(),
                                            #Year checkbox filter
                                            fluidRow(
                                            column(3,
                                                   selectInput("trt",label="Choose the trt",choices=sort(veteran$trt), multiple = TRUE)
                                            ),
                                            column(3,
                                            selectInput("cellt",label="Choose the celltype",choices=sort(veteran$celltype), multiple = TRUE)
                                            ),
                                            column(3,
                                                   sliderInput("slage", label="Choose the age",
                                                               min =34, max = 81, value = c(34, 81)
                                                               )
                                            ),
                                            column(3,
                                                   checkboxInput("addd", "Add data series", value = F)
                                            )),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.addd == true",
                                                column(3,
                                                       selectInput("trt2",label="Choose the trt",choices=sort(veteran$trt), multiple = TRUE)
                                                ),
                                                column(3,
                                                       selectInput("cellt2",label="Choose the celltype",choices=sort(veteran$celltype), multiple = TRUE)
                                                ),
                                                column(3,
                                                       sliderInput("slage2", label="Choose the age",
                                                                 min =34, max = 81, value = c(34, 81)
                                                       )
                                                )
                                              )
                                            ),
                                            column(8,
                                            checkboxGroupInput("chm", 
                                                               label="Choose the method", 
                                                               choices = list("Kaplan Meier" = 1, 
                                                                              "Cox Proportional Hazards" = 2
                                                                              ), 
                                                               inline = TRUE
                                                               )),
                                            column(4,
                                                   checkboxInput("i95", "Add 95% interval", value = F)
                                                   ),
                                            br(),
                                            br(),
                                            br(),
                                            fluidRow(
                                              conditionalPanel(
                                                condition = "input.chm!=0",
                                                plotlyOutput("plot12")
                                              )
                                              
                                              
                                            ),
                                              fluidRow(
                                                #if(length(textOutput("test"))>0){
                                                  dataTableOutput('view2')
                                                  
                                                #}
                                                 ,align = "center",
                                                br(),
                                                verbatimTextOutput("summary2"),
                                                br(),
                                                h2("plot1"), 
                                                plotlyOutput("plot"))
                                              
                                              ,
                                            br(),
                                              h6(HTML('Copyright <a href="https://pl.linkedin.com/in/łukasz-janiszewski-390a1660/"><b>Łukasz Janiszewski</b></a>'), align = "center")                                     
                                                               
                                            # 2nd tab with useR information same structure like before, one change no year filter
                                            
                                            )}                            
                                    )
                                    )
)

# #######################
     #SERVER#
# #######################
server <- function(input, output) {
  ####
  #data prep
  {
    km_fit <- survfit(Surv(time, status) ~ 1, data=veteran)
    
    vet <- mutate(veteran, AG = ifelse((age < 60), "LT60", "OV60"),
                  AG = factor(AG),
                  trt = factor(trt,labels=c("standard","test")),
                  prior = factor(prior,labels=c("N0","Yes")))
    
    cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior , data = vet)
    
    cox_fit <- survfit(cox)
    
    r_fit <- ranger(Surv(time, status) ~ trt + celltype + 
                      karno + diagtime + age + prior,
                    data = vet,
                    mtry = 4,
                    importance = "permutation",
                    splitrule = "extratrees",
                    verbose = TRUE)
    
    # Average the survival models
    death_times <- r_fit$unique.death.times 
    surv_prob <- data.frame(r_fit$survival)
    avg_prob <- sapply(surv_prob,mean)
    
    kmi <- rep("KM",length(km_fit$time))
    km_df <- data.frame(km_fit$time,km_fit$surv,kmi)
    names(km_df) <- c("Time","Surv","Model")
    
    coxi <- rep("Cox",length(cox_fit$time))
    cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
    names(cox_df) <- c("Time","Surv","Model")
    
    rfi <- rep("RF",length(r_fit$unique.death.times))
    rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
    names(rf_df) <- c("Time","Surv","Model")
  
  
  
  plot_df <- rbind(km_df,cox_df,rf_df)
  
  
  
  
  }

  #tab1
  ######################
  output$view <- renderDataTable(datatable( veteran, 
                                 options = list(scrollX = TRUE, pageLength = 5)))
  
  output$summary <- renderPrint({
    summary(veteran)
  })
  #------------------------------
  #data prep tab2
  {
  veteran2<-reactive({veteran[(#veteran$celltype %in% if(is.null(input$cellt)){veteran$celltype} else{input$cellt}
    veteran$trt %in% if(is.null(input$trt)){veteran$trt} else{input$trt})
    ,]    
  })
  veteran1<-reactive({veteran2()[veteran2()$celltype %in% if(is.null(input$cellt)){veteran2()$celltype} else{input$cellt} ,]    
  })
  veteran3<-reactive({veteran1()[veteran1()$age>= input$slage[1] & veteran1()$age<= input$slage[2],]    
  })
  
  veteran22<-reactive({veteran[(#veteran$celltype %in% if(is.null(input$cellt)){veteran$celltype} else{input$cellt}
    veteran$trt %in% if(is.null(input$trt2)){veteran$trt} else{input$trt2})
    ,]    
  })
  veteran12<-reactive({veteran22()[veteran22()$celltype %in% if(is.null(input$cellt2)){veteran22()$celltype} else{input$cellt2} ,]    
  })
  veteran32<-reactive({veteran12()[veteran12()$age>= input$slage2[1] & veteran12()$age<= input$slage2[2],]    
  })
}
  #models
  {
  model1<-reactive({
    if(input$chm==1){survfit(Surv(time, status) ~ 1, data=veteran3())}
  
    else if(input$chm==2){cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior , data = veteran3())
    cox_fit <- survfit(cox)}
  })
  
  model2<-reactive({
    if(input$chm[2]==2){cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior , data = veteran3())
    cox_fit <- survfit(cox)}
  })
  model3<-reactive({
    if(is.na(input$chm[2])==F){cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior , data = veteran32())
    cox_fit <- survfit(cox)}
    
    else{
      if(input$chm==1){survfit(Surv(time, status) ~ 1, data=veteran32())}
      
      else if(input$chm==2){cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior , data = veteran32())
      cox_fit <- survfit(cox)}
    }
  })
  }
  
  output$test<- renderText({
    input$chm[2]
  })
  #tab2
  #####################
  
  output$plot1 <- renderPlotly({
    plot_ly(plot_df,x=~Time, y=~Surv, color=~Model, type ="scatter",mode = "lines")%>% 
      layout(margin = list(b = 80, l = 80, r = 20),xaxis = list(title= "Time"), yaxis = list(title= "Surv %",tickformat = "%"), 
             legend = list(orientation = 'h',x=0.5, y = 0.9))
    
  })
  
  output$plot12 <- renderPlotly({
    if(input$i95==T){
      if(input$addd==F){
        if(is.na(input$chm[2])==F){
          plot_ly(x=~model1()$time, y=~model1()$surv, type ="scatter",mode = "lines",name = "KM Model")%>%
            add_trace(x=~model1()$time,y=~model1()$upper, type ="scatter",mode = "lines",  name = 'Upper 95%',showlegend = FALSE,
                      line = list(color = 'transparent'))%>%
            add_trace(x=~model1()$time,y=model1()$lower,
                      fill = 'tonexty', fillcolor='rgba(0,50,100,0.2)', line = list(color = 'transparent'),
                      showlegend = TRUE, name = '95% Conf interval') %>%
            add_trace(x=~model2()$time, y=~model2()$surv, type ="scatter",mode = "lines",name = "Cox Model")%>%
            add_trace(x=~model2()$time,y=~model2()$upper, type ="scatter",mode = "lines",  name = 'Upper 95%',showlegend = FALSE,
                      line = list(color = 'transparent'))%>%
            add_trace(x=~model2()$time,y=model2()$lower,
                      fill = 'tonexty', fillcolor='rgba(100,0,80,0.2)', line = list(color = 'transparent'),
                      showlegend = TRUE, name = '95% Conf interval') %>%
            layout(margin = list(b = 80, l = 80, r = 20),xaxis = list(title= "Time"), yaxis = list(title= "Surv %",tickformat = "%"), 
                   legend = list(x=0.8, y = 1))
        }
        else{
          plot_ly(x=~model1()$time, y=~model1()$surv, type ="scatter",mode = "lines",name = "Model")%>%
            add_trace(x=~model1()$time,y=~model1()$upper, type ="scatter",mode = "lines",  name = 'Upper 95%',showlegend = FALSE,
                      line = list(color = 'transparent'))%>%
            add_trace(x=~model1()$time,y=model1()$lower,
                      fill = 'tonexty', fillcolor='rgba(0,50,100,0.2)', line = list(color = 'transparent'),
                      showlegend = TRUE, name = '95% Confident interval') %>%
            layout(margin = list(b = 80, l = 80, r = 20),xaxis = list(title= "Time"), yaxis = list(title= "Surv %",tickformat = "%"), 
                   legend = list(orientation = 'h',x=0.5, y = 1))
        }
      }
      #zle
      else{
        plot_ly(x=~model1()$time, y=~model1()$surv, type ="scatter",mode = "lines",name = "KM Model")%>%
          add_trace(x=~model1()$time,y=~model1()$upper, type ="scatter",mode = "lines",  name = 'Upper 95%',showlegend = FALSE,
                    line = list(color = 'transparent'))%>%
          add_trace(x=~model1()$time,y=model1()$lower,
                    fill = 'tonexty', fillcolor='rgba(0,50,100,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = '95% Conf interval') %>%
          add_trace(x=~model3()$time, y=~model3()$surv, type ="scatter",mode = "lines",name = "Cox Model")%>%
          add_trace(x=~model3()$time,y=~model3()$upper, type ="scatter",mode = "lines",  name = 'Upper 95%',showlegend = FALSE,
                    line = list(color = 'transparent'))%>%
          add_trace(x=~model3()$time,y=model3()$lower,
                    fill = 'tonexty', fillcolor='rgba(100,0,80,0.2)', line = list(color = 'transparent'),
                    showlegend = TRUE, name = '95% Conf interval') %>%
          layout(margin = list(b = 80, l = 80, r = 20),xaxis = list(title= "Time"), yaxis = list(title= "Surv %",tickformat = "%"), 
                 legend = list(x=0.8, y = 1))
      }
    }
    else{
      if(input$addd==F){
        if(is.na(input$chm[2])==F){
          plot_ly(x=~model1()$time, y=~model1()$surv, type ="scatter",mode = "lines",name = "KM Model")%>% 
              add_trace(x=~model2()$time, y=~model2()$surv, type ="scatter",mode = "lines",name = "Cox Model")%>% 
            layout(margin = list(b = 80, l = 80, r = 20),xaxis = list(title= "Time"), yaxis = list(title= "Surv %",tickformat = "%"), 
                   legend = list(orientation = 'h',x=0.5, y = 1))
        }
        else{
            plot_ly(x=~model1()$time, y=~model1()$surv, type ="scatter",mode = "lines",name = "Model")%>% 
            layout(margin = list(b = 80, l = 80, r = 20),xaxis = list(title= "Time"), yaxis = list(title= "Surv %",tickformat = "%"), 
                   legend = list(orientation = 'h',x=0.5, y = 1))
        }
      }
      else{
        plot_ly(x=~model1()$time, y=~model1()$surv, type ="scatter",mode = "lines",name = "Model 1")%>% 
          add_trace(x=~model3()$time, y=~model3()$surv, type ="scatter",mode = "lines",name = "Model 2")%>% 
          layout(margin = list(b = 80, l = 80, r = 20),xaxis = list(title= "Time"), yaxis = list(title= "Surv %",tickformat = "%"), 
                 legend = list(orientation = 'h',x=0.5, y = 0.9))
      }
    }
  })
  
  
  
  
  output$view2 <- renderDataTable(datatable( veteran3(), 
                                            options = list(scrollX = TRUE, pageLength = 5)))
  
  output$summary2 <- renderPrint({
    summary(veteran3())
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

