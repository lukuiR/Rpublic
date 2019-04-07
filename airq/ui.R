library(shiny)
library(shinydashboard)
library(rgdal)
library(leaflet)
library(dygraphs)
library(plotly)
library(DT)
library(mapview)
library(maptools)
library(rsconnect)
#data

db1<-read.csv("./data/5_Links.csv",header = TRUE)
db<-data.frame(db1)
time1<-read.csv("./data/time.csv",header = TRUE)

states <- readOGR("./data/sample.shp",layer = "sample")


dat1<-read.csv("./data/dat1.csv",header = TRUE)

info1<-read.csv("./data/info1.csv",header = TRUE)
info<-data.frame(info1)
#1.Dashboard structure - start

dashboardPage(skin = "purple",
              dashboardHeader(title = "Dynamic sidebar" 
              ),
              ####################################################################################
              #1.1 set separate pages/tabs-start (menuItem)
              dashboardSidebar(
                sidebarMenu(
                  menuItem("Read Me", icon = icon("dashboard"), tabName = "raw")
                  ,
                  menuItem("Traffic Flow/Speed", icon = icon("dashboard"), tabName = "speed")
                  , 
                  menuItem("Emision Profile", icon = icon("dashboard"), tabName = "dashboard")
                  ,                
                  menuItem("Spare Emission", icon = icon("dashboard"), tabName = "import")
                  ,
                  menuItem("Graphs", icon = icon("dashboard"), tabName = "graph")
                )
              ),
              #1.1 set separate pages/tabs-end
              ############################################################################ 
              #1.2 main place of dashboard content-start
              dashboardBody(
                #1.2.1 tabitems seperate tab
                tabItems(
                  #1.2.2 tabitem- tab content
                  ###########################################################33
                  #first Tab  Read me
                  tabItem(
                    tabName = "raw",
                    fluidRow(h2("Read Me")),
                    fluidRow(box(h6("CH2M HILL, also known as CH2M, is a global engineering company that provides consulting, 
                                    design, construction, and operations services for corporations, and federal, state, 
                                    and local governments. The firm's headquarters is in Meridian, an unincorporated area of Douglas County, 
                                    Colorado in the Denver-Aurora Metropolitan Area. The postal designation of nearby 
                                    Englewood is commonly listed as the company's location in corporate filings and local news accounts. 
                                    As of December 2013, CH2M had approximately 26,000 employees and 2013 revenues totaled $5.88 billion.
                                    [3] The firm is employee-owned,[1] with an internal stock market that operates buy/sell events quarterly. 
                                    CH2M HILL announced a global rebrand on 13 April 2015, adopting the nickname CH2M while retaining 
                                    CH2M HILL Companies Ltd. as the firm's legal name.[4]"))
                    ),
                    fluidRow(box(h2("History"),
                                 h6("CH2M was founded in 1946 in Corvallis, Oregon by Oregon State University civil engineering professor 
                                    Fred Merryfield and three of his students: Holly Cornell, James Howland and Thomas Burke Hayes 
                                    (the firm was named after the founders, with Howland and Hayes making up the H2 portion).[5] 
                                    Cornell, Howland, and Hayes were all graduates of Oregon State University.[6] The company became 
                                    CH2M Hill name after a 1971 merger with Clair A. Hill and Associates of Redding, California.[7] 
                                    The firm remained headquartered in Oregon until 1980, when a decision was made to relocate to Colorado,
                                    a more central location in the United States.")))
                                 )
                  ,
                  #end tab 1
                  #################################################################3
                  #start tab2 Traffic Flow/Speed
                  tabItem(
                    tabName = "speed",
                    fluidRow(h2("Traffic Flow/Speed"))
                    ,
                    fluidRow(box(h5("add coment 1"),
                                 radioButtons("dataset", "Choose a data type:", 
                                              choices = list("Total Flow" = 5, "Speed" = 9)))
                    ),
                    fluidRow(box(h5("add coment 2")),
                             leafletOutput("map", height = "800px")
                             
                             
                    )
                    
                  ),
                  #end2
                  ###########################################################################3
                  #start tab3 Emision Profile
                  tabItem(
                    tabName = "dashboard",
                    fluidRow(h2("Flow Distribution")),
                    fluidRow(box(h5("add coment 3"),
                                 sliderInput("range", label = "Time Interval:",
                                             min = 1, max = 24, value = c(7, 18))
                    )
                    ),
                    
                    fluidRow(box(h5("add coment 4")),
                             leafletOutput("mapplot", height = "800px"),
                             mapview:::plainViewOutput("test")
                             
                    )
                    
                  ),
                  #end tab3
                  ##############################################################################333
                  #start tab 4  Spare Emission
                  
                  tabItem(
                    tabName = "import",
                    fluidRow(h2("Import data that would be displayed")),
                    fluidRow(
                      box(h5("Data can include only this information with the same column name: id, X, Y"),
                          fileInput('file1', 'Choose file to upload',
                                    accept = c(
                                      'text/csv',
                                      'text/comma-separated-values',
                                      'text/tab-separated-values',
                                      'text/plain',
                                      '.csv',
                                      '.tsv'
                                    )
                          ),
                          tags$hr(),
                          checkboxInput('header', 'Header', TRUE),
                          radioButtons('sep', 'Separator',
                                       c(Comma=',',
                                         Semicolon=';',
                                         Tab='\t'),
                                       ','),
                          radioButtons('quote', 'Quote',
                                       c(None='',
                                         'Double Quote'='"',
                                         'Single Quote'="'"),
                                       '"'),
                          tags$hr(),
                          p('If you want a sample .csv or .tsv file to upload,',
                            'you can first download the sample',
                            a(href = 'mtcars.csv', 'mtcars.csv'), 'or',
                            a(href = 'pressure.tsv', 'pressure.tsv'),
                            'files, and then try uploading them.'
                          )
                      ), box(radioButtons("radio", label = h3("Vehicle Fleet Split"),
                                          choices = list("All LGV" = 5, "All HGV" = 6,
                                                         "Petrol Car" = 7,"Diesel Car" = 8, "Diesel LGV" = 11,
                                                         "Rigid HGV" = 12,"Artic HGV" = 13, "Buses/Coaches" = 14),selected = 5,inline  = TRUE))),
                    
                    fluidRow(box(h5("add coment 8")),
                             leafletOutput("mapimport", height = "800px")),
                    
                    fluidRow(box(h5("add coment 9"),
                                 dataTableOutput('cont2'),width=NULL)
                    )),
                  #end tab4
                  ##########################################################################
                  #start tab5 graph
                  tabItem(
                    tabName = "graph",
                    fluidRow(h2("Graphs")),
                    
                    fluidRow(
                      box(h5("add coment 5"),
                          plotlyOutput("trendPlot"),width = NULL)
                    ),
                    fluidRow(
                      box(h5("add coment 6"),
                          plotlyOutput("trendPlot2"),width = NULL)
                    ),
                    fluidRow(
                      box(h5("add coment 7"),
                          plotlyOutput("trendPlot3"),width = NULL)
                    ),
                    fluidRow(
                      box(h5("add coment 8"),checkboxGroupInput("checkGroup", 
                                                                label = h3("Checkbox group"), 
                                                                choices =sort(dat1$Source_nam),
                                                                selected = 1,inline   = TRUE),
                          plotlyOutput("trendPlot4"),width = NULL)
                    ),
                    fluidRow(
                      h5("add coment 8"),
                      plotlyOutput("trendPlot5")
                    )
                  )
                  #end tab5
                                 )
                    )
                    )
#1.2 main place of dashboard content-end
#1.Dashboard structure - end
