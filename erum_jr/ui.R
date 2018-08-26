


#ui.r
# read needed libraries
library(shiny)
library(shinydashboard)
library(data.table)
library(leaflet)
#data
# We need load data with continent country information to feed out filter options 
cc1=read.csv("cc.csv", header = TRUE, sep=",")
#Remove space
cc1$country<- gsub("\\s", "", cc1$country)
#Standard Dashboard structure 
dashboardPage(skin = "black",
                      dashboardHeader(title = "eRum",
                              tags$li(class = "dropdown",                                  tags$a(href="https://blog.jumpingrivers.com/posts/2018/jumping_rivers_erum/#the-main-competition", target="_blank", 
                                      tags$img(height = "20px", alt="SNAP Logo", src="https://blog.jumpingrivers.com/img/2018/eRum_jr.png")
                                      )      )          ),
                            dashboardSidebar(
                 # specify number of tabs               
                sidebarMenu(
                  id = "tabs",
                  menuItem("Start", icon = icon("info"), tabName = "tab1"
                  ),
                #tab 1 R Conference  
                  menuItem("R Conference", icon = icon("calendar"), tabName = "dashboard2"),       
               #tab2 R useR 
                menuItem("R useR", icon = icon("far fa-users"), tabName = "dashboard1"),
              # tab3 R Girls                  
                  menuItem("R Girls", icon = icon("far fa-female"), tabName = "dashboard"),
# Year filter               
selectInput("cont",label="Choose the continent",choices=unique(cc1$continent),multiple = TRUE),
              selectInput("kraj",label="Choose the country",choices=sort(cc1$country), multiple = TRUE)
                  )
              ),
            # dashboard body  
              dashboardBody(includeCSS("styles.css"),
                                          tabItems(
           #1st tab with informations mainly text with additional css styling
                              tabItem(tabName = "tab1",includeCSS("styles1.css"),                                                                         img(src='https://blog.jumpingrivers.com/img/2018/eRum_jr.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;"),
                                      h4(HTML('<b>"Free ticket to eRum"</b>'), align = "center"),
                                      h4(HTML("So... big news."),align = "left"),
                                      h4(HTML("Jumping Rivers is sponsoring eRum 2018 and in light of this news we are giving away a free place at the conference!    </br>    (Not to mention our very own lead consultant, Colin Gillespie, is one of the invited speakers.)  </br> More information can be found on page:"),align = "left"),
                                      h4(HTML('<a href="https://blog.jumpingrivers.com/posts/2018/jumping_rivers_erum/#the-main-competition"><b>blog.jumpingrivers.com</b></a>'), align = "center")
                                      ,align = "center",
                                      h4(HTML('<b>"The Main Competition"</b>'), align = "center"),
                                      h4(HTML('Here at Jumping Rivers, we maintain the site <a href="https://jumpingrivers.github.io/meetingsR/"><b>meetingsR</b></a>. This comprises of three comprehensive lists:
                                              </br>1. All upcoming (and foregone) R conferences.
                                              </br>2. All R useR groups from around the globe.
                                              </br>3. All R-Ladies groups from around the globe.
                                              </br>See the <a href="https://github.com/jumpingrivers/meetingsR/"><b>GitHub repo</b></a> for the contents.'),align = "left"),
                                      h6("Copyright Ĺukasz Janiszewski", align = "center")
                                      
                                      ),
#2nd tab, Information about R Conference
                              tabItem(tabName = "dashboard2",includeCSS("styles1.css"),
                                      
                                      img(src='https://blog.jumpingrivers.com/img/2018/eRum_jr.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;")
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
       tags$div("Wczytywanie...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                                                          <span class="sr-only">Loading...</span>'),id="loadmessage")), #end
                                                leafletOutput("map1"), #map output 
align = "center",                      
fluidRow(
h2("Select pin to view details"), 
#Below standard information, like location, that will be rendered after selecting pin on map
                                                 column(4,uiOutput("image3", align = "center")),
                                                 column(4,
                                                        h2("Location:"),
                                                        h4("Country:"), textOutput("zz13"),
                                                        h4("City:"),textOutput("zz23")  
                                                 ),
                                                column(4,
                                                        h2("Conference name:"),textOutput("zz53"),
                                                        h4("Year:"),textOutput("zz33"),
                                                        h4("Date:"),textOutput("zz43")
                                                 ),
       #I start with displaying all information in table this can be remove or replace, but sometimes when data do not transform correctly we can see it in table. 
                                                 dataTableOutput(outputId = "zzz223") ,align = "center"
                                      )
                                      ,h6("Copyright Łukasz Janiszewski, image source: wikipedia & meetup", align = "center")                                     
                              ),
# 2nd tab with useR information same structure like before, one change no year filter
                              tabItem(tabName = "dashboard1",includeCSS("styles1.css"),
                                      
                                      img(src='https://blog.jumpingrivers.com/img/2018/eRum_jr.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;"),                                      
                                      fluidRow(
                                        h2("R user location", align = "center")
                                      ),                                      
                                      br(),
                                      fluidRow( conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
         tags$div("Wczytywanie...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
         <span class="sr-only">Loading...</span>'),id="loadmessage")),
                                                leafletOutput("map2"),align = "center") #map                                     
                                      ,fluidRow(h2("Select pin to view details"),
#text information
                                                column(3,uiOutput("image21", align = "center")),
                                                column(3,
                                                       h2("Location:"),
                                                       h4("Country:"), textOutput("zz11"),
                                                       h4("City:"),textOutput("zz21")                                                       
                                                ),                                                
                                                column(3,
                                                       h2("Group name:"),textOutput("zz51"),
                                                       h4("Members:"),textOutput("zz31"),
                                                       h4("Organizer:"),textOutput("zz41")
                                                ),                                                
                                                column(3, h2("Logo:"),
                                                       uiOutput("image12", align = "center")
                                                ),
#table
                                                dataTableOutput(outputId = "zzz22"),align = "center"                                      
                                      ),
                                      h6("Copyright Łukasz Janiszewski, image source: wikipedia & meetup", align = "center")
                                      
                              ),
# 3rd tab R Girls the same structure
                              tabItem(
                                tabName = "dashboard",includeCSS("styles1.css"),
                                img(src='https://blog.jumpingrivers.com/img/2018/eRum_jr.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;")
                                ,
                                fluidRow(
                                  h2("R Girls location", align = "center")
                                    ),  br(),
#Loading animation
                                fluidRow( conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
tags$div("Wczytywanie...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
             <span class="sr-only">Loading...</span>'),id="loadmessage")),
                                          leafletOutput("map3"), # map output
align = "center" ),
fluidRow(h2("Select pin to view details"),
#text information
                                  column(3,uiOutput("image", align = "center")),
                                  column(3,
                                         h2("Location:"),
                                         h4("Country:"), textOutput("zz1"),
                                         h4("City:"),textOutput("zz2")                                         
                                         ),                                  
                                  column(3,
                                         h2("Group name:"),textOutput("zz5"),
                                         h4("Members:"),textOutput("zz3"),
                                         h4("Organizer:"),textOutput("zz4")
                                         ),                                  
                                  column(3, h2("Logo:"),
                                         uiOutput("image1", align = "center") ),
                                  # table                                
                                  dataTableOutput(outputId = "zzz2") ,align = "center"
                                )
                     ,h6("Copyright Łukasz Janiszewski, image source: wikipedia & meetup", align = "center")                             
                                    )
                              )                            
      )
)
)
# end of ui.r
