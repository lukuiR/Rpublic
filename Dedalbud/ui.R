#DT ino: https://yihui.shinyapps.io/DT-info/


library(shiny)
library(shinydashboard)
library(data.table)
library(rgdal)
library(leaflet)
library(dygraphs)
library(plotly)
#data


mycss <- "
#plot-container {
position: relative;
}
#loading-spinner {
position: absolute;
left: 50%;
top: 50%;
z-index: -1;
margin-top: -33px;  /* half of the spinner's height */
margin-left: -33px; /* half of the spinner's width */
}
#plot.recalculating {
z-index: -2;
}
"





dashboardPage(skin = "black",
              
              
              dashboardHeader(title = "Dedal-Bud",
                              tags$li(class = "dropdown",
                                      tags$a(href="http://dedalbud.pl/", target="_blank", 
                                             tags$img(height = "20px", alt="SNAP Logo", src="http://www.dedalbud.pl/img/logo.png")
                                      )
                              )
              ),
              
              dashboardSidebar(
                
                
                sidebarMenu(
                  id = "tabs",
                  menuItem("Start", icon = icon("info"), tabName = "tab1"
                  ),
                  menuItem("Promocje", icon = icon("money"), tabName = "promo"
                  ),
                  menuItem("Koszyk", icon = icon("shopping-basket"), tabName = "koszyk"
                  ),
                  menuItem("Dostawa", icon = icon("truck"), tabName = "dashboard")
                )
              ),
              
              dashboardBody(includeCSS("styles.css"),
                            tags$script('
                                        // Bind function to the toggle sidebar button
                                        $(".sidebar-toggle").on("click",function(){
                                        $(window).trigger("resize"); // Trigger resize event
                                        })'
    ),
    
    tags$head(
      tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Roboto');
                      
                      h3 {
                      font-family: 'Roboto';padding: 0 30px;
                      font-weight: bold;
                      }
                      
                      "))
      ),
    tags$head(
      tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Roboto');
                      
                      h2 {
                      font-family: 'Roboto';font-weight: bold;padding: 0 30px;
                      
                      }
                      
                      "))
      ),
    
    tabItems(
      
      tabItem(tabName = "tab1",includeCSS("styles1.css"),
              
              
              
              img(src='http://www.dedalbud.pl/img/logo.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;")
              ,
              h4(HTML("Aplikacja powstała w&nbspramach projektu pn."), align = "center"),
              h4(HTML('<b>"Poprawa sprzedaży o kontaktu z klientem w hurtowni materiałów budowlanych w&nbspZłotoryi"</b>'), align = "center"),
              h4(HTML("Jeśli chcesz uzyskać informacje o&nbspProjekcie, znajdziesz je na stronie internetowej:"),align = "left"),
              h4(HTML('<a href="http://www.dedalbud.pl"><b>www.dedalbud.pl</b></a>'), align = "center"),br(),
              h4(HTML('Kliknij pineskę, żeby zobaczyć zasieg dostawy'), align = "center"),br()
              ,leafletOutput("map")
              
              ,align = "center",
              h6("Łukasz Janiszewski 2017", align = "center")
              
              
      ),
      tabItem(tabName = "promo",includeCSS("styles1.css"),
              
              
              
              img(src='http://www.dedalbud.pl/img/logo.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;")
              ,
              h2(HTML('<b>"Aktualne Promocje!!!"</b>'), align = "center"),
              h4(HTML("Przejżyj nasze promocje nastepnie przejdz do zakładki koszyk aby z nich skożystać. 
                      W zakładce koszyk możesz zapoznać się z naszymi produktami i ich cenami oraz zrobić symulację zakupów."),align = "left"),
              h4(HTML('<a href="http://www.dedalbud.pl"><b>www.dedalbud.pl</b></a>'), align = "center"),br(),
              
              align = "center",
              h6("Łukasz Janiszewski 2017", align = "center")
              
              
      ),
      tabItem(tabName = "koszyk",includeCSS("styles1.css"),
              
              
              h4(HTML("Jestes w zakładce koszyk tu możesz wybrać produkty"), align = "center"),
              h4(HTML('<b>"Uwaga obecnie mamy promocje na Mazdy"</b>'), align = "center"),
              fluidRow(
                p(class = 'text-center')
              ),
              hr(),
              h1('Produkty'),
              fluidRow(
                column(6, DT::dataTableOutput('x4')),
                column(6, verbatimTextOutput('x5')),
                column(6, DT::dataTableOutput('x6'))
                
              ),
              fluidRow( h1('Twój koszyk:')),
              fluidRow(
                uiOutput("MainBody")
                
              )
              
              
      ),
      tabItem(
        tabName = "dashboard",includeCSS("styles1.css"),
        
        fluidRow( conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                                   tags$div("Wczytywanie...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
<span class="sr-only">Loading...</span>'),id="loadmessage")),
                 align = "center"),
        fluidRow(h4(HTML("Zasięg darmowej dostawy"),align = "left"),
                 h4(HTML("zależy od wartości towaru i ilość kursów w danym kierunku:"),align = "left"),
                 leafletOutput("map1"),
                 h4(HTML("Zadzwoń napewno się dogadamy"),align = "left")
          
          ),br(),
        
        h6("Łukasz Janiszewski 2017", align = "center")
        
          )
        )
    
    )
    )







# Put them together into a dashboardPage

