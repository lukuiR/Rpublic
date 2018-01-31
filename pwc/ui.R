
library(shiny)
library(shinydashboard)
library(data.table)
library(rgdal)
library(leaflet)
library(dygraphs)
library(plotly)
#data

mydat <- fread(paste('http://aerisqualitas.org/coyote/csv/',Sys.Date(),'.csv',sep = ""))

a1<-as.numeric(sub(",", ".", mydat$`deviceID:115 type:30 variable:4`, fixed = TRUE))
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
              
              
              dashboardHeader(title = "PwC",
                              tags$li(class = "dropdown",
                                      tags$a(href="http://pwc.pl/", target="_blank", 
                                             tags$img(height = "20px", alt="SNAP Logo", src="https://upload.wikimedia.org/wikipedia/commons/thumb/0/05/PricewaterhouseCoopers_Logo.svg/1280px-PricewaterhouseCoopers_Logo.svg.png")
                                      )
                              )
              ),
              
              dashboardSidebar(
                
                
                sidebarMenu(
                  id = "tabs",
                  menuItem("Why R?", icon = icon("info"), tabName = "tab1"
                  )
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
              
              
              img(src='https://scontent.fwaw3-1.fna.fbcdn.net/v/t1.0-9/16997941_425547181129530_3464045437544313590_n.jpg?oh=acfed565d1d788a91efa43935201a00b&oe=5A5D5010',height = "300px" ,style="display: block; margin-left: auto; margin-right: auto;")
              ,
              h4(HTML('Krótka prezentacja danych związanych ze zbliżającą się konferencja <b>"Why R? 2017"</b>'), align = "center"),br(),
              h4(HTML('Dystrybucja potencjalnych uczestników konferencji z proporcją ilości mężczyzn do kobiet'), align = "center"),br(),
              
              leafletOutput("map"),
              
              h4(HTML('Najpopularniejsze Technologie z jakimi współpracują potencjalni uczestnicy konferencji'), align = "center"),br(),
              
              fluidRow(plotlyOutput("plot3")),br(),
              h4(HTML('Najpopularniejsze Pakiety R z jakimi współpracują potencjalni uczestnicy konferencji'), align = "center"),br(),
              
            
              fluidRow(plotlyOutput("plot5")),
              
              h6("Copyright & Łukasz Janiszewski 2017", align = "center")
              
              
      )
        )
    
    )
    )







# Put them together into a dashboardPage

