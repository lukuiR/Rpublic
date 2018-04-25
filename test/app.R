library(shinydashboard)
library(dplyr)
library(dbplyr)
library(purrr)
library(shiny)
library(highcharter)
library(DT)
library(htmltools)
library(plotly)
library(shinyalert)

# Use purrr's split() and map() function to create the list
# needed to display the name of the airline but pass its
# Carrier code as the value


ui <- dashboardPage(skin = "green",
  dashboardHeader(title = "BGZ BNP" #,titleWidth = 200
                  ),
  dashboardSidebar(
    
    selectInput(
      inputId = "typ",
      label = "Typ:", 
      choices = unique(alg$typ), 
      multiple = TRUE),
    selectInput(
      inputId = "kod",
      label = "Kod:", 
      choices = unique(met_kon$KOD_OBSZARU), 
      multiple = TRUE),
    selectInput(
      inputId = "waga",
      label = "Waga:", 
      choices = unique(met_kon$WAGA_KONTROLI), 
      multiple = TRUE),
    selectInput(
      inputId = "jed",
      label = "Jednostka:", 
      choices = unique(met_kon$JEDNOSTKA_ZGLASZAJACA), 
      multiple = TRUE),
    selectInput(
      inputId = "mail",
      label = "Mnemonik:", 
      choices = unique(o_grp$MNEMONIK_ODBIORCY), 
      multiple = TRUE),
    sidebarMenu(
      selectInput(
        "month",
        "Miesiąc:", 
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
      actionLink("remove", "Wyczyść dodatkowe karty")
    )
  ),
  dashboardBody(      
    tabsetPanel(id = "tabs",   
                tabPanel(includeCSS("styles1.css"),
                  title = "ZZJD Dashboard",icon = icon("glyphicon glyphicon-saved",lib ="glyphicon"),
                  value = "page1",
                  #useShinyalert(),
                  fluidRow(
                    valueBoxOutput("total_flights"),
                    valueBoxOutput("per_day"),
                    valueBoxOutput("percent_delayed")
                  ),
                  fluidRow(
                    valueBoxOutput("total_flights2"),
                    valueBoxOutput("per_day2"),
                    valueBoxOutput("percent_delayed2")
                  ),
                  fluidRow(
                    valueBoxOutput("total_flights3"),
                    valueBoxOutput("per_day3"),
                    valueBoxOutput("percent_delayed3")
                  ),
                  fluidRow(
                    valueBoxOutput("percent_delayed4")
                    
                  ),
                  fluidRow(
                    column(width = 7,
                           p(textOutput("monthly")),
                           highchartOutput("group_totals")),
                    column(width = 5,
                           p("Click on an airport in the plot to see the details"),
                           highchartOutput("top_airports"))
                  )
                ),
                tabPanel("Plot", icon = icon("line-chart"), plotlyOutput("plot"),plotlyOutput("plot2")
                         
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
   o_grp<-o_grp[complete.cases(o_grp),]
  o_grp1=reactive({na.omit(o_grp ) })
  
  
  tab_list <- NULL
  
 
  # Preparing the data by pre-joining flights to other
  # tables and doing some name clean-up
  # db_flights <- flights %>%
  #   left_join(airlines, by = "carrier") %>%
  #   rename(airline = name) %>%
  #   left_join(airports, by = c("origin" = "faa")) %>%
  #   rename(origin_name = name) %>%
  #   select(-lat, -lon, -alt, -tz, -dst) %>%
  #   left_join(airports, by = c("dest" = "faa")) %>%
  #   rename(dest_name = name) 
  
  output$monthly <- renderText({
    if(input$month == "99")"Click on a month in the plot to see the daily counts"
  })
  

  output$total_flights <- renderValueBox({
    # The following code runs inside the database

    result <- o_grp1()
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(input$month != 99) result <- filter(result, substr(DATA,6,7) == input$month)
    
    result <- result %>%
      tally() %>%
      pull() %>% 
      as.integer()
    
    valueBox(value = prettyNum(result, big.mark = ","),
             subtitle = "Number of data")
  })
  
  
  output$per_day <- renderValueBox({
    result <- o_grp
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    

    if(input$month != 99) result <- filter(result, substr(DATA,6,7) == input$month)
    result <- result %>%
      group_by(DATA) %>%
      tally() %>%
      summarise(avg = mean(n)) %>%
      pull()
    
    valueBox(prettyNum(result, big.mark = ","),
             subtitle = "Average Flights",
             color = "blue")
    valueBox(
      formatC(sum(o_grp[o_grp$DATA==max(as.character(o_grp$DATA)),"BLEDY_NOWE"]), format="d", big.mark=','),width=
        ,paste('Bledy Nowe')
      ,icon = icon("glyphicon glyphicon-remove-sign",lib='glyphicon')
      ,color = "red")
    
    
    
  })
  
  
  
  output$percent_delayed <- renderValueBox({
    result <- o_grp1()
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    # The following code runs inside the database

    if(input$month != 99) result <- filter(result, substr(DATA,6,7) == input$month)
    result <- result %>%
      filter(!is.na(GRUPA_ALGORYTMU)) %>%
      mutate(GRUPA_ALGORYTMU = ifelse(GRUPA_ALGORYTMU >= 15, 1, 0)) %>%
      summarise(GRUPA_ALGORYTMU = sum(GRUPA_ALGORYTMU),
                total = n()) %>%
      mutate(percent = GRUPA_ALGORYTMU / GRUPA_ALGORYTMU) %>%
      pull()
    
    valueBox(paste0(round(result * 100), "%"),
             subtitle = "Flights delayed",
             color = "teal")
    valueBox(
      formatC(sum(o_grp[as.character(o_grp$DATA)==max(as.character(o_grp$DATA)),"BLEDY"]), format="d", big.mark=','),width=
        ,paste('Bledy')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
  })
  
  #######new kpi
  
  output$total_flights2 <- renderValueBox({
    # The following code runs inside the database
    
    result <- o_grp1()%>%
      filter(MNEMONIK_ODBIORCY != 'OK')
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(input$month != 99) result <- filter(result, substr(DATA,6,7) == input$month)
    
    valueBox(
      formatC(sum(result[as.character(result$DATA)==max(as.character(o_grp$DATA)),"BLEDY"]), format="d", big.mark=','),width=
        ,paste('Błędy')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
  })
  
  
  output$per_day2 <- renderValueBox({
    result <- o_grp1()%>%
      filter(MNEMONIK_ODBIORCY != 'OK')
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    
    
    if(input$month != 99) result <- filter(result, substr(DATA,6,7) == input$month)

    valueBox(
      formatC(sum(result[as.character(result$DATA)==max(as.character(o_grp$DATA)),"BLEDY_NOWE"]), format="d", big.mark=','),width=
        ,paste('Błędy Nowe')
      ,icon = icon("glyphicon glyphicon-remove-sign",lib='glyphicon')
      ,color = "red")
    
  })
  
  
  
  output$percent_delayed2 <- renderValueBox({
    result <- o_grp1()%>%
      filter(MNEMONIK_ODBIORCY != 'OK')
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    # The following code runs inside the database
    
    valueBox(
      formatC(sum(result[as.character(result$DATA)==max(as.character(o_grp$DATA)),"BLEDY_POPRAWIONE"]), format="d", big.mark=','),width=
        ,paste('Bledy Poprawione')
      ,icon = icon("glyphicon glyphicon-wrench",lib='glyphicon')
      ,color = "green")
    
  })
  #######new kpi row2
  
  output$total_flights3 <- renderValueBox({
    # The following code runs inside the database
    
    result <- o_grp1()
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    if(input$month != 99) result <- filter(result, substr(DATA,6,7) == input$month)
    
    valueBox(
      paste(round(sum(result[result$MNEMONIK_ODBIORCY != 'OK' & as.character(result$DATA)==max(as.character(o_grp$DATA)),"BLEDY"])/sum(o_grp[o_grp$MNEMONIK_ODBIORCY == 'OK' & o_grp$DATA==max(o_grp$DATA),"BLEDY"]),6)*100,"%"),width=
        ,paste('Odstetek Błędów')
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
  })
  
  
  output$per_day3 <- renderValueBox({
    result <- o_grp1()%>%
      filter(MNEMONIK_ODBIORCY != 'OK')
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    
    
    if(input$month != 99) result <- filter(result, substr(DATA,6,7) == input$month)
    
    valueBox(
     paste( round(sum(result[as.character(result$DATA)==max(as.character(o_grp$DATA)),"BLEDY_NOWE"])/sum(result[result$DATA==max(result$DATA),"BLEDY"]), 6)*100,"%"),width=
        ,paste('Błędy Nowe/Błędy')
      ,icon = icon("glyphicon glyphicon-remove-sign",lib='glyphicon')
      ,color = "red")
    
  })
  
  
  
  output$percent_delayed3 <- renderValueBox({
    result <- o_grp1()%>%
      filter(MNEMONIK_ODBIORCY != 'OK')
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    # The following code runs inside the database
    
    valueBox(
      paste(round(sum(result[as.character(result$DATA)==max(as.character(o_grp$DATA)),"BLEDY_POPRAWIONE"])/sum(result[result$DATA==max(result$DATA),"BLEDY"]), 6)*100,"%"),width=
        ,paste('Błędy Poprawione/Błędy')
      ,icon = icon("glyphicon glyphicon-wrench",lib='glyphicon')
      ,color = "green")
    
  }) 
  output$percent_delayed4 <- renderValueBox({
    result <- o_grp1()%>%
      filter(MNEMONIK_ODBIORCY != 'OK')
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    # The following code runs inside the database
    
    valueBox(
      paste(round(sum(result[as.character(result$DATA)==max(as.character(o_grp$DATA)),"BLEDY_POPRAWIONE"])/sum(result[as.character(result$DATA)==max(as.character(o_grp$DATA)),"BLEDY_NOWE"]), 6)*100,"%"),width=
        ,paste('Błędy Poprawione/Błędy Nowe')
      ,icon = icon("glyphicon glyphicon-wrench",lib='glyphicon')
      ,color = "green")
    
  })
  # Events in Highcharts can be tracked using a JavaScript. For data points in a plot, the 
  # event.point.category returns the value that is used for an additional filter, in this case
  # the month that was clicked on.  A paired observeEvent() command is activated when
  # this java script is executed
  js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  
  output$group_totals <- renderHighchart({
    result <- o_grp1()
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    if(input$month != 99) {
      result1 <-filter(result, substr(DATA,6,7) == input$month) %>%
        group_by(DATA) %>%
        tally(BLEDY_NOWE) 
      group_name <- "Daily"
      result2 <-filter(result, substr(DATA,6,7) == input$month) %>%
        group_by(DATA) %>%
        tally(BLEDY_POPRAWIONE) 
      group_name <- "Daily"
    } else {
      result1 <- result %>%
        group_by(DATA) %>%
        tally(BLEDY_NOWE)     
      group_name <- "Monthly"
      result2 <- result %>%
        group_by(DATA) %>%
        tally(BLEDY_POPRAWIONE)     
      group_name <- "Monthly"
      result3 <- result %>%filter(MNEMONIK_ODBIORCY!="OK")%>%
        group_by(DATA) %>%
        tally(BLEDY)     
      group_name <- "Monthly"
      result4 <- result %>%filter(MNEMONIK_ODBIORCY!="OK")%>%
        group_by(DATA) %>%
        tally(BLEDY_NOWE_POPR)     
      group_name <- "Monthly"
    } 
    
    highchart() %>% 
           hc_chart(type = "line") %>% 
           hc_title(text = "Wykres błędów") %>% 
           hc_xAxis(categories = result1$DATA[complete.cases(result1)]) %>% 
          hc_add_series(data = result1$n[complete.cases(result1)],name = paste("Ilość błędów nowych"),
        events = list(click = js_click_line))%>%
    hc_add_series(data = result2$n[complete.cases(result2)],name = paste("Ilość błędów poprawione"),
                  events = list(click = js_click_line))%>%
      hc_add_series(data = result3$n[complete.cases(result3)],name = paste("Ilość błędów"),
                    events = list(click = js_click_line))%>%
      hc_add_series(data = result4$n[complete.cases(result4)],name = paste("Ilość błędów nowych pop."),
                    events = list(click = js_click_line))
    
    
  })
  
  # Tracks the JavaScript event created by `js_click_line`
  observeEvent(input$line_clicked != "",
               if(input$month == 99)
                 updateSelectInput(session, "month", selected = substr(input$line_clicked,6,7)),
               ignoreInit = TRUE)
  
  js_bar_clicked <- JS("function(event) {Shiny.onInputChange('bar_clicked', [event.point.category]);}")
  
  output$top_airports <- renderHighchart({
    # The following code runs inside the database
    result <- o_grp1()
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    
    if(input$month != 99) result <- filter(result, substr(DATA,6,7) == input$month) 
    
    result <- result %>%
      group_by(GRUPA_ALGORYTMU) %>%
      tally(BLEDY) %>%
      arrange(desc(n)) %>%
      collect() %>%
      head(10)
    
    highchart() %>%
      hc_add_series(
        data = result$n, 
        type = "bar",
        name = paste("GRUPA ALGORYTMU"),
        events = list(click = js_bar_clicked)) %>%
      hc_xAxis(
        categories = result$GRUPA_ALGORYTMU,
        tickmarkPlacement="on")
    
    
  })
  
  #####plot plane
  
  
  output$plot <- renderPlotly({
    result <- o_grp1()
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    
    plot_ly(result,labels = ~GRUPA_ALGORYTMU, values = ~BLEDY, type = "pie",textposition = "inside"
    )%>%
      layout(title = 'United States Personal Expenditures by Categories in 1960',
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  })
  
  
  output$plot2 <- renderPlotly({
    result <- o_grp1()
    if(is.null(input$waga) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) result <- filter(result, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) result <- filter(result, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    result<-result%>% group_by(DATA)%>% summarise(ratio=sum(BLEDY_POPRAWIONE)/sum(BLEDY_NOWE))
    
    plot_ly(result, x=~DATA, y=~ratio, type ="scatter",mode = "lines"
    )%>% add_trace(x=~DATA,y=1)%>%
      layout(yaxis = list(tickformat = "%"), margin = list(b = 80), xaxis = list(tickangle = 45))
    
  })
  
  

  #####
  mtSummarised1 <- reactive({
    
    details <- o_grp1() %>%
      filter(GRUPA_ALGORYTMU == input$bar_clicked[1])
    details <- details %>% 
      mutate(month = month.name[as.integer(substr(DATA,6,7))])
    
    if(input$month != 99) details <- filter(details, substr(DATA,6,7) == input$month) 
    
    if(is.null(input$waga) != TRUE) details <- filter(details, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) details <- filter(details, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) details <- filter(details, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) details <- filter(details, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) details <- filter(details, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    details
  
  })
  #####
  o_id1<-reactive({o_id[o_id$MNEMONIK_ODBIORCY== input$shinyalert, ]})
  
  mtSummarised2 <- reactive({
    details <-o_id1() %>%
      filter(GRUPA_ALGORYTMU == mtSummarised1()[input$tabela_rows_selected, ]$GRUPA_ALGORYTMU) %>% 
      filter(DATA == mtSummarised1()[input$tabela_rows_selected, ]$DATA) %>%
      group_by(GRUPA_ALGORYTMU) 
    if(input$month != 99) details <- filter(details, substr(DATA,6,7) == input$month) 
    
    if(is.null(input$waga) != TRUE) details <- filter(details, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$WAGA_KONTROLI %in% input$waga])
    if(is.null(input$typ) != TRUE) details <- filter(details, GRUPA_ALGORYTMU %in% alg$GRUPA_ALGORYTMU[alg$typ %in% input$typ])
    if(is.null(input$jed) != TRUE) details <- filter(details, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$JEDNOSTKA_ZGLASZAJACA %in% input$jed])
    if(is.null(input$mail) != TRUE) details <- filter(details, MNEMONIK_ODBIORCY %in% input$mail)
    if(is.null(input$kod) != TRUE) details <- filter(details, GRUPA_ALGORYTMU %in% met_kon$GRUPA_ALGORYTMU[met_kon$KOD_OBSZARU %in% input$kod])
    details
  })
  
  # Generate datatable
  output$tabela <- renderDataTable({
    brks <- quantile(mtSummarised1()$BLEDY, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    datatable(mtSummarised1(),
              escape = -2,
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
              ))%>%formatStyle('BLEDY', target = 'row',backgroundColor = styleInterval(brks, clrs))
    
  })
  
  output$tabela2 <- renderDataTable({
    brks <- quantile(mtSummarised2()$BLEDY, probs = seq(.05, .95, .05), na.rm = TRUE)
    clrs <- round(seq(255, 40, length.out = length(brks) + 1), 0) %>%
    {paste0("rgb(255,", ., ",", ., ")")}
    datatable(mtSummarised2(),
              escape = -2,
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
              ))%>%formatStyle('BLEDY', target = 'row',backgroundColor = styleInterval(brks, clrs))
    
  })
  
  #####
  
  
  
  observeEvent(input$bar_clicked,
               {
                 grupa <- input$bar_clicked[1]
                 tab_title <- paste(input$waga, 
                                    "-", grupa , 
                                    if(input$month != 99) paste("-" , month.name[as.integer(input$month)]))
                 
                 if(tab_title %in% tab_list == FALSE){
                   
                   appendTab(inputId = "tabs",
                             tabPanel(
                               tab_title,
                               dataTableOutput("tabela")
                             ),session = getDefaultReactiveDomain())
                   
                   tab_list <<- c(tab_list, tab_title)
                   
                 }
                 
                 updateTabsetPanel(session, "tabs", selected = tab_title)
                 
               })

  observeEvent(input$tabela_rows_selected,
               {
                 tab_title <- "ID Algorytmu"
                 
                 if (tab_title %in% tab_list == FALSE){
                   
                   appendTab(inputId = "tabs",
                             tabPanel(
                               tab_title,
                               dataTableOutput("tabela2")
                             ))
                   
                   tab_list <<- c(tab_list, tab_title)
                   
                 }
                 
                 updateTabsetPanel(session, "tabs", selected = tab_title)
                 
               })  

  
  observeEvent(input$remove,{
    # Use purrr's walk command to cycle through each
    # panel tabs and remove them
    tab_list %>%
      walk(~removeTab("tabs", .x))
    tab_list <<- NULL
  })
  
}





shinyApp(ui, server)