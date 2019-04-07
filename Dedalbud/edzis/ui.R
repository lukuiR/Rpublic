library(shiny)
library(shinydashboard)

library(DT)


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
                                 
                  menuItem("Spare Emission", icon = icon("dashboard"), tabName = "import")
                  
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
                    fluidRow(box(h6("Program dobierajacy diete w zaleznosci od zapotrzebowania, nalezy wczytac produkty,
                                    nastepnie zapotrzebowanie na produkty dla danej osoby, a aplikacja sama dostosuje produkty."))
                    )
                                 )
                  ,
                  #end tab 1
                  #################################################################3
                  #start tab 4  Spare Emission
                  
                  tabItem(
                    tabName = "import",
                    fluidRow(h2("Import data that would be displayed")),
                    fluidRow(
                      box(h5("Import product data"),
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
                      ), 
                      
                      box(h5("Upload Personal Dieta"),
                          fileInput('file2', 'Choose file to upload',
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
                          checkboxInput('header1', 'Header', TRUE),
                          radioButtons('sep1', 'Separator',
                                       c(Comma=',',
                                         Semicolon=';',
                                         Tab='\t'),
                                       ','),
                          radioButtons('quote1', 'Quote',
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
                      )),
                    
                    
                    fluidRow(box(h5("add coment 9"),
                                 dataTableOutput('cont3'),width=NULL)
                    ),
                    
                    
                    fluidRow(box(h5("add coment 9"),
                                 dataTableOutput('cont4'),width=NULL)
                    ))
                  #end tab4
                  ##########################################################################
              
                                 )
                    )
                    )
#1.2 main place of dashboard content-end
#1.Dashboard structure - end
