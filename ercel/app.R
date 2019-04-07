#dt ext https://rstudio.github.io/DT/extensions.html
#DT ino: https://yihui.shinyapps.io/DT-info/
###
library(shinydashboard)
library(shiny)
library(DT)
library(plotly)
library(shinyalert)
library(readxl)
library(tidyverse)

# Use purrr's split() and map() function to create the list
# needed to display the name of the airline but pass its
# Carrier code as the value


#select only required columns


ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Mercel" #,titleWidth = 200
                    ),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        id = "tabs",
                        menuItem("Start", icon = icon("info"), tabName = "tab1"),
                        menuItem("Supply", icon = icon("line-chart"), tabName = "dashboard")
                      ),
                      
                      selectInput(
                        inputId = "typ",
                        label = "Cherf/Not:", 
                        choices = c('Yes','No','NA'), 
                        multiple = TRUE),
                      selectInput(
                        inputId = "kod",
                        label = "Gender:", 
                        choices = c('Male','Female', 'NA'), 
                        multiple = TRUE),
                      sidebarMenu(
                        selectInput(
                          "month",
                          "Month:", 
                          list(
                            "All Year" = 99,
                            "January" = '01',
                            "February" = '02',
                            "March" = '03',
                            "April" = '04',
                            "May" = '05',
                            "June" = '06',
                            "July" = '07',
                            "August" = '08',
                            "September" = '09',
                            "October" = '10',
                            "November" = '11',
                            "December" = '12'
                          ) , 
                          selected =  "All Year", 
                          selectize = FALSE),
                        actionLink("remove", "Wyczy dodatkowe karty")
                      )
                    ),
                    dashboardBody( 
                      tabItems(
                        tabItem(tabName = "tab1",
                      tabsetPanel(id = "tabs",   
                                  tabPanel(
                                           title = "ZZJD Dashboard",icon = icon("glyphicon glyphicon-saved",lib ="glyphicon"),
                                           value = "page1",
                                           #useShinyalert(),
                                           fluidRow(
                                             
                                             column(3,
                                                    selectInput("Gender",label="Choose the Gender",choices=c('Male','Female', 'NA'), multiple = TRUE)),
                                             column(3,
                                                    selectInput("chef",label="Choose the chef/not",choices=c('Yes','No','NA'), multiple = TRUE)),
                                             column(3,
                                                    sliderInput("slage", label="Choose the age",
                                                                min =34, max = 81, value = c(34, 81))),
                                             column(3,
                                                    checkboxInput("addd", "Add data series", value = F))
                                           ),
                                           fluidRow(
                                             fileInput("uploadFile", "XLSX file")
                                           ),
                                           br(),
                                           fluidRow(
                                             conditionalPanel(
                                               condition = "output.fileUploaded",
                                               plotlyOutput("plot2")
                                             )
                                           ),
                                           fluidRow(
                                              dataTableOutput(outputId = "dt2")
                                           )
                                  ),
                                  tabPanel("Plot/Filter tab", icon = icon("line-chart"), 
                                           radioButtons("radio2", h3("filter "),
                                                        choices = list("Waga" = "WAGA", "Supergrupa" = "SUPERGRUPA",
                                                                       "Memonik" = "MNEMONIK_ODBIORCY"),inline = TRUE,selected = "WAGA"),
                                           plotlyOutput("plot1"),plotlyOutput("plot11")
                                  )
                      )
                        ),
                      tabItem(tabName = "dashboard",
                              h4('Filter tab'),
                              radioButtons("radio", h3("filte"),
                                           choices = list("Waga" = "WAGA", "Supergrupa" = "SUPERGRUPA",
                                                          "Memonik" = "MNEMONIK_ODBIORCY"),inline = TRUE,selected = "WAGA"),
                              fluidRow(
                                column(1,br(),h3(2018)),
                                
                                column(2, 
                                       textInput("text1", h5("Text input"), 
                                                 value = "Enter text...")) ,
                                
                                column(2, 
                                       textInput("text2", h5("Text input"), 
                                                 value = "Enter text...")) ,
                                
                                column(2, 
                                       textInput("text3", h5("Text input"), 
                                                 value = "Enter text...")) ,
                                
                                column(2, 
                                       textInput("text4", h5("Text input"), 
                                                 value = "Enter text..."))   
                              )
                              
                      
                              
                              )
                      )
                    )
)




server <- function(input, output, session) { 

  
  #  shinyalert(
  #   title = "Witaj",
  #   text = "Wprowadz: uzytkownik/haslo",
  #   closeOnEsc = TRUE,
  #   closeOnClickOutside = FALSE,
  #   html = FALSE,
  #   type = "input",
  #   inputType = "text",
  #   inputValue = "",
  #   inputPlaceholder = "",
  #   showConfirmButton = TRUE,
  #   showCancelButton = FALSE,
  #   confirmButtonText = "OK",
  #   confirmButtonCol = "#AEDEF4",
  #   timer = 0,
  #   imageUrl = "",
  #   animation = TRUE
  # )


  dataset<-reactive({
    inFile <- input$uploadFile 
    
    if (is.null(inFile))
      return(NULL)
    
    datraw<-read_excel(inFile$datapath, col_names = TRUE,skip=1)
    
    colnames(datraw)[9] <- "Job Family"
    colnames(datraw)[55] <- "Employment__1"
    dat <- select(datraw, Gender, Employment__1 , `Career Level` ,`Chief/Not` , `Company Name`, `SBU/MFG/CF`, Subfamily , Family , RegionCode, `Level 1`, `Year of retirement`, `Job Family`, `Encrypted Ids`)
    
    
    #calculate field
    
    dat <- mutate(dat, 
                  `SBU/MFG/CF Name`= case_when(`SBU/MFG/CF` == "SBU" ~ `Level 1`,  `SBU/MFG/CF` == "MFG Sites" ~ `Company Name`, TRUE ~ `SBU/MFG/CF`)
    )
    
    #Remove not needed job Family
    dat$`Job Family`[!(dat$`Job Family` %in% c('Professionals',	'Technicians', 'Executives',	'Advisor & Consultant',	'Managers',	'Supervisor',	
                                               'Administrators',	'Operators',	'Superintendent & Section Head',	
                                               'Security & Safety', 'Para Professional'))] <- 'Other'
    
    #Aggregate, count id
    ag_dat <- group_by(dat, Employment__1 , `Career Level` ,`Chief/Not` , `Company Name`, `SBU/MFG/CF`, Subfamily , Family , RegionCode, `Level 1`, `Year of retirement`, `Job Family`, `SBU/MFG/CF Name`) %>% 
      summarise(
        n = n()
      )
    
    #spread
    
    #BC
    ag_dat1 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, Employment__1 )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = Employment__1, value = n) #, fill =0)
    
    ag_dat2 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, `Chief/Not` )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = `Chief/Not`, value = n) #, fill =0)
    
    colnames(ag_dat2)[dim(ag_dat2)[2]] <- "`Chief/Not NA`"
    
    ag_dat3 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, `Career Level` )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = `Career Level`, value = n) #, fill =0)
    
    colnames(ag_dat3)[dim(ag_dat3)[2]] <- "`Career Level NA`"
    
    ag_dat4 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, Gender )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = Gender, value = n) #, fill =0)
    
    colnames(ag_dat4)[dim(ag_dat4)[2]] <- "`Gender NA`"
    
    ag_dat5 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, `Job Family` )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = `Job Family`, value = n) #, fill =0)
    
    ag_dat6 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, `Year of retirement` )%>% 
      summarise(
        n = n()
      ) %>%
      spread(key = `Year of retirement`, value = n) #, fill =0)
    
    #remove not needed years of retirement
    #ag_dat66=ag_dat6[, c('Subfamily' , 'Family' ,'SBU/MFG/CF', 'SBU/MFG/CF Name', 'RegionCode', '2018', '2019', '2020','2021','2022', '2023')] #1:5,17:22)]
    
    ag_dat66 <- select( ag_dat6, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode, '2018',`2019`,`2020`,`2021`,`2022`,`2023`)
    
    ag_dat6 <- group_by(dat, Subfamily , Family ,`SBU/MFG/CF`, `SBU/MFG/CF Name`, RegionCode )%>% 
      summarise(
        n = n()
      )
    
    z <- ag_dat1 %>% 
      inner_join(ag_dat2)%>% 
      inner_join(ag_dat3)%>% 
      inner_join(ag_dat4)%>% 
      inner_join(ag_dat5)%>% 
      inner_join(ag_dat66)%>% 
      inner_join(ag_dat6)
    
    head(z,10)
    
    
    return(head(z,10))
  })
  
  output$fileUploaded <- reactive({
    return(!is.null(dataset()))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$dt2<- renderDataTable({

    
    datatable(dataset(),
              extensions = c('FixedHeader', 'Buttons', 'ColReorder', 'Scroller'),
              options = list(
                dom = 'Bfrti',
                autoWidth = FALSE,
                colReorder = TRUE,
                deferRender = TRUE,
                scrollX = TRUE,
                scrollY = "51vh",
                scroller = TRUE,
                scollCollapse = TRUE,
                fixedHeader = TRUE,
                columnDefs = list(
                  list(orderable = FALSE, className = 'details-control', targets = 0)
                )
              ))
    
    })
  
  ####plot
  output$plot2 <- renderPlotly({
    
    plot_ly(x=dataset()$RegionCode, y=dataset()$n, mode = "bar"
    )
    
  })
  
  zz=data.frame(  
    cbind(c('actual', 'future'), c(220,140), c('grey', 'yellow'))
    )
  colnames(zz) <- c('Categorie', 'values','color')
 
  output$plot1 <- renderPlotly({
    plot_ly(zz, labels = ~Categorie, values = ~values, 
            textposition = 'inside',
            textinfo = 'label',
            insidetextfont = list(color = '#000000'),
            hoverinfo = 'text',
            marker = list(colors = ~color), type = 'pie', rotation=160)
  })
  
  zz1=data.frame(  
    cbind(c('actual', 'future'), c(140,220), c('grey', 'red'))
  )
  colnames(zz1) <- c('Categorie', 'values','color')
  
  output$plot11 <- renderPlotly({
    plot_ly(zz1, labels = ~Categorie, values = ~values, 
            textposition = 'inside',
            textinfo = 'label',
            insidetextfont = list(color = '#000000'),
            hoverinfo = 'text',
            marker = list(colors = ~color), type = 'pie', rotation=-20)
  })
  
  
  #####
  
  #####NHR
  {
  
    
}
  
    #####end event
  
}





shinyApp(ui, server)
