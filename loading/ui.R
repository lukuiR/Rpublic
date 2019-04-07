
library(shiny)
library(shinydashboard)
library(data.table)
library(rgdal)
library(leaflet)
library(dygraphs)
library(plotly)
#data

a1 <-read.csv(file="www/2017-03-27.csv", header=TRUE, sep=";") #fread(paste('http:// adres www /csv/','2017-03-27','.csv',sep = ""))
a1 <- a1[,10]
a1<-as.numeric(sub(",", ".", a1, fixed = TRUE))

mydat <-read.csv(file="www/2017-03-27.csv", header=TRUE, sep=";") #fread(paste('http:// adres www /csv/','2017-03-27','.csv',sep = ""))
mydat <- mydat[,c(1,10,8)]
names(mydat) <- c("Time","a1","a2")

dashboardPage(skin = "black",
              
              dashboardHeader(title = "aeris qualitas",
                              tags$li(class = "dropdown",
                                      tags$a(href="http://aerisqualitas.org/", target="_blank", 
                                      tags$img(height = "20px", alt="SNAP Logo", src="http://aerisqualitas.org/wp-content/uploads/2015/11/zmak-kolor-01.png")
                                      )
              )
              ),
              
              dashboardSidebar(
                
                
                sidebarMenu(
                  id = "tabs",
                  menuItem("Start", icon = icon("info"), tabName = "tab1"
                  ),
                  
                  menuItem("Sala nr 103", icon = icon("line-chart"), tabName = "dashboard")
                  )
              ),
              
              dashboardBody(includeCSS("styles.css"),
              
                            tabItems(
                              
                              tabItem(tabName = "tab1",includeCSS("styles1.css"),
                                   
                                      img(src='http://aerisqualitas.org/wp-content/uploads/2015/10/kolor-01-1024x640.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;")
                                      ,
                                      h4(HTML("Aplikacja powstała w&nbspramach projektu pn."), align = "center"),
                                      h4(HTML('<b>"Poprawa jakości powietrza wewnętrznego w&nbsppolskich placówkach edukacyjnych – działania w Szkole Podstawowej nr&nbsp10 w&nbspLubinie"</b>'), align = "center"),
                                      h4(HTML("organizowanego przez Stowarzyszenie Zwykłe <big>aeris qualitas</big>"), align = "center"),br(),
                                      h4(HTML("Wybierając zakładkę po lewej stronie dowiesz się jaka jest jakość powietrza wewnętrznego w wybranej sali dydaktycznej w Szkole Podstawowej nr&nbsp10 w&nbspLubinie."),align = "left"),br(),
                                      h4(HTML("Jeśli chcesz uzyskać informacje o&nbspProjekcie, Stowarzyszeniu Zwykłym aeris qualitas lub o&nbspjakości powietrza wewnętrznego i&nbspjego wpływie na zdrowie, znajdziesz je na stronie internetowej:"),align = "left"),
                                      
                                      h4(HTML('<a href="http://www.aerisqualitas.org"><b>www.aerisqualitas.org</b></a>'), align = "center"),br()
                                      
                                      ,align = "center",
                                      h6("Copyright aeris qualitas & Łukasz Janiszewski 2017", align = "center")
                                      
                                      ),
                              tabItem(
                                tabName = "dashboard",
                                fluidRow(
                                  h2("Jakość powietrza w sali dydaktycznej nr 103 jest:", align = "center")
                                    ),
                                h2(HTML( '<b><font color=#35528E>' ,  if (a1[dim(mydat)[1]]<701){
                                  paste("BARDZO DOBRA")}
                                  
                                  else if (a1[dim(mydat)[1]]<1201) {
                                    paste("DOBRA ")} 
                                  else if (a1[dim(mydat)[1]]<1601) {
                                    paste("DOPUSZCZALNA")}
                                  else if (a1[dim(mydat)[1]]<2001) {
                                    paste("ZŁA")}
                                  else{
                                    paste("BARDZO ZŁA")},'</b></font>' ), align = "center"),
                                br(),
                                fluidRow( conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                                                           #poniżej https://fontawesome.com/how-to-use/svg-with-js
                                                           tags$div("Wczytywanie...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                                                          <span class="sr-only">Loading...</span>'),id="loadmessage")),
                                          imageOutput("image2"),align = "center"),
                                fluidRow(
                                      plotlyOutput("plot1")
                                  
                                ),br(),
                                
                                fluidRow(plotlyOutput("plot4")
                                )
                                
                                    )
                              )
                            
      )
)







# Put them together into a dashboardPage

