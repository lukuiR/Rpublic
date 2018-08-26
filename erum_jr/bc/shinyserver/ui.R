
library(shiny)
library(shinydashboard)
library(data.table)
library(rgdal)
library(leaflet)
library(dygraphs)
library(plotly)
#data

aa1=read.csv("cc.csv", header = TRUE, sep=",")
aa1$country<- gsub("\\s", "", aa1$country)

dashboardPage(skin = "black",
              
              dashboardHeader(title = "eRum",
                              tags$li(class = "dropdown",
                                      tags$a(href="https://blog.jumpingrivers.com/posts/2018/jumping_rivers_erum/#the-main-competition", target="_blank", 
                                      tags$img(height = "20px", alt="SNAP Logo", src="https://blog.jumpingrivers.com/img/2018/eRum_jr.png")
                                      )
              )
              ),
              
              dashboardSidebar(
                
                
                sidebarMenu(l
                  id = "tabs",
                  menuItem("Start", icon = icon("info"), tabName = "tab1"
                  ),
                  
                  menuItem("R Conference", icon = icon("calendar"), tabName = "dashboard2")
                ,
                
                menuItem("R useR", icon = icon("far fa-users"), tabName = "dashboard1")
              ,
                  
                  menuItem("R Girls", icon = icon("far fa-female"), tabName = "dashboard"),
              #sidebarSearchForm(textId = "sliderInput", buttonId = "searchButton"
               #                 ),
              #withTags(div(class='row-fluid',
               #                                div(class='span3', checkboxGroupInput("yearr2", 
                #                                                                     h3("Year"), 
                 #                                                                    choices = list("2016" = 2016, "2017" = 2017, 
                  #                                                                                  "2018" = 2018,
                   #                                                                                 "2019" = 2019)))
                    #            )),
              #
              selectInput("cont",label="Choose the continent",choices=unique(aa1$continent),multiple = TRUE),
              selectInput("kraj",label="Choose the country",choices=sort(aa1$country), multiple = TRUE)
              
              #
                  )
              ),
              
              dashboardBody(includeCSS("styles.css"),
              
                            tabItems(
                              
                              tabItem(tabName = "tab1",includeCSS("styles1.css"),
                                   
                                      img(src='https://blog.jumpingrivers.com/img/2018/eRum_jr.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;")
                                      ,
                                      h4(HTML('<b>"Free ticket to eRum"</b>'), align = "center"),
                                      h4(HTML("So... big news."),align = "left"),
                                      h4(HTML("Jumping Rivers is sponsoring eRum 2018 and in light of this news we are giving away a free place at the conference!
                                              </br>
                                              (Not to mention our very own lead consultant, Colin Gillespie, is one of the invited speakers.)
                                              </br> More information can be found on page:"),align = "left"),
                                      h4(HTML('<a href="https://blog.jumpingrivers.com/posts/2018/jumping_rivers_erum/#the-main-competition"><b>blog.jumpingrivers.com</b></a>'), align = "center")
                                      ,align = "center",
                                      h4(HTML('<b>"The Main Competition"</b>'), align = "center"),
                                      h4(HTML('Here at Jumping Rivers, we maintain the site <a href="https://jumpingrivers.github.io/meetingsR/"><b>meetingsR</b></a>. This comprises of three comprehensive lists:
                                              </br>1. All upcoming (and foregone) R conferences.
                                              </br>2. All R useR groups from around the globe.
                                              </br>3. All R-Ladies groups from around the globe.
                                              </br>See the <a href="https://github.com/jumpingrivers/meetingsR/"><b>GitHub repo</b></a> for the contents.'),align = "left"),
                                      h6("Copyright Łukasz Janiszewski", align = "center")
                                      
                                      ),
                              tabItem(tabName = "dashboard2",includeCSS("styles1.css"),
                                      
                                      img(src='https://blog.jumpingrivers.com/img/2018/eRum_jr.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;")
                                      ,
                                      h4("R Conference")
                                      ,
                                      fluidRow(
                                        h2("Location", align = "center")
                                      ),br(),
                                      checkboxGroupInput("yearr", 
                                                         h3("Year"), 
                                                         choices = list("2016" = 2016, "2017" = 2017, "2018" = 2018,
                                                                        "2019" = 2019), inline =TRUE,
                                                         selected = c(2018) ),
                                      
                                      br(),
                                      fluidRow( conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                                                                 #poniĹĽej https://fontawesome.com/how-to-use/svg-with-js
                                                                 tags$div("Wczytywanie...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                                                          <span class="sr-only">Loading...</span>'),id="loadmessage")),
                                                leafletOutput("map23"),align = "center",
                                               
                                                #hide error
                                                tags$style(type="text/css",
                                                           ".shiny-output-error { visibility: hidden; }",
                                                           ".shiny-output-error:before { visibility: hidden; }" )
                                                
                                      ),fluidRow(h2("Select pin to view details"),
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
                                                 
                                                 
                                                 
                                                 dataTableOutput(outputId = "zzz223") ,align = "center"
                                      )
                                      ,h6("Copyright Łukasz Janiszewski, image source: wikipedia & meetup", align = "center")
                                      
                              ),
                              tabItem(tabName = "dashboard1",includeCSS("styles1.css"),
                                      
                                      img(src='https://blog.jumpingrivers.com/img/2018/eRum_jr.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;")
                                      ,
                                      
                                      fluidRow(
                                        h2("R user location", align = "center")
                                      ),
                                      
                                      br(),
                                      fluidRow( conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                                                                 #poniĹĽej https://fontawesome.com/how-to-use/svg-with-js
                                                                 tags$div("Wczytywanie...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                                                          <span class="sr-only">Loading...</span>'),id="loadmessage")),
                                                leafletOutput("map2"),align = "center")
                                      
                                      ,fluidRow(h2("Select pin to view details"),
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
                                                dataTableOutput(outputId = "zzz22"),align = "center"
                                      
                                      ),
                                      h6("Copyright Łukasz Janiszewski, image source: wikipedia & meetup", align = "center")
                                      
                              ),
                              tabItem(
                                tabName = "dashboard",includeCSS("styles1.css"),
                                img(src='https://blog.jumpingrivers.com/img/2018/eRum_jr.png', width='40%' ,style="display: block; margin-left: auto; margin-right: auto;")
                                ,
                                fluidRow(
                                  h2("R Girls location", align = "center")
                                    ),
                                
                                br(),
                                fluidRow( conditionalPanel(condition="$('html').hasClass('shiny-busy')", 
                                                           #poniĹĽej https://fontawesome.com/how-to-use/svg-with-js
                                                           tags$div("Wczytywanie...",HTML('<i class="fa fa-spinner fa-pulse fa-3x fa-fw"></i>
                                                                                          <span class="sr-only">Loading...</span>'),id="loadmessage")),
                                          leafletOutput("map"),align = "center"
                               
                                       
                                   
                                ),fluidRow(h2("Select pin to view details"),
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
                                         uiOutput("image1", align = "center")
                                         ),
                                  
                                
                                  dataTableOutput(outputId = "zzz2") ,align = "center"
                                )
                                
                                ,h6("Copyright Łukasz Janiszewski, image source: wikipedia & meetup", align = "center")
                             
                                    )
                              )
                            
      )
)







# Put them together into a dashboardPage

