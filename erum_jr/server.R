#server.r start
# server.R
# read needed libraries
library(quantmod)
library(ggplot2)
library(plotly)
library(DT)
library(RColorBrewer)
#read csv with continent and country list, needed for filter update
cc2=read.csv("cc.csv", header = TRUE, sep=",")
cc2$country<- gsub("\\s", "", cc2$country)
shinyServer(function(input, output, session) {
  set.seed(122)
  
  ##
 # country filter update, when we select som continent, in county list will be contries related only to that continent 
  observeEvent(input$cont, {
    cont <- input$cont
    
    # Available model options based on cont
    choices <- cc2[cc2$continent %in% cont,3]
     # Update the kraj input object to only display wanted choices                  
    updateSelectInput(session, "kraj", choices = choices)
  })
  ##############
 # start data related to R Conference data
  
  conf_csv=read.csv("conf.csv", header = TRUE, sep=",",colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
 #create data that is updated base on country filter, I also add conditions that when, user do not select any country, or any continent, we show all available data. In conference it is also filtered by selected year.  
   conf <- reactive({conf_csv[conf_csv$year %in%  input$yearr & gsub("\\s", "", 
                                    conf_csv$id) %in% if(is.null(input$kraj)){if(is.null(input$cont)){cc2$country} else (cc2[cc2$continent %in% input$cont,3])} else (input$kraj),]     })
#render map
  
  output$map1 <- renderLeaflet(
    leaflet(conf()) %>% addTiles() %>%
      addMarkers(lng= conf()$lon, lat= conf()$lat  ,label = conf()$city ,popup=paste0('<BIG>', conf()$nazwa,'</BIG><br/>Web: ', conf()$web,'<br/>Date: ', conf()$date,'<br/>Year: ', conf()$year,"<br/><img src = ", conf()$wiki, " Width = 150><br/> <SMALL>source: wikipedia</SMALL> ")
      ))
 #render table
  output$zzz223 <- renderDataTable(conf()[conf()$lon==input$map1_marker_click$lng,], 
                                  options = list(scrollX = TRUE, autoWidth = TRUE))
  
  #render text information based on selected pin on map
  output$zz13<- renderText(paste(conf()$kraj[conf()$lon==input$map1_marker_click$lng]))
  output$zz23<- renderText(paste(conf()$miasto[conf()$lon==input$map1_marker_click$lng]))
  output$zz33<- renderText(paste(conf()$year[conf()$lon==input$map1_marker_click$lng]))
  output$zz43<- renderText(paste(conf()$date[conf()$lon==input$map1_marker_click$lng]))
  output$zz53<- renderText(paste(conf()$nazwa[conf()$lon==input$map1_marker_click$lng]))
  ######graph
 #render image of location based on selected pin on map 
  output$image1 <- renderUI({
    tags$img(height = "300px", src = as.character( paste(conf()$wiki[conf()$lon==input$map1_marker_click$lng])))
  })
   #render table based on selected pin on map
  output$zzz223 <- renderDataTable(conf()[conf()$lon==input$map1_marker_click$lng,], 
                                  options = list(scrollX = TRUE, autoWidth = TRUE))
#end of R Conference tab data
 #######start data related to useR tab
#read csv file with useR data  
user_csv=read.csv("user.csv", header = TRUE, sep=",",colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  user_csv$lon[is.na(user_csv$lon)] <- 0
  user_csv$lat[is.na(user_csv$lat)] <- 0
#create data that is updated base on country filter, I also add conditions that when, user do not select any country, or any continent, we show all available data    
  user <- reactive({user_csv[ gsub("\\s", "", user_csv$kraj) %in% if(is.null(input$kraj)){if(is.null(input$cont)){cc2$country} else (cc2[cc2$continent %in% input$cont,3])} else (input$kraj),]    
  }) 
  # render map
  output$map2 <- renderLeaflet(
    leaflet(user()) %>% addTiles() %>%
      addMarkers(lng= user()$lon, lat= user()$lat#, clusterOptions = markerClusterOptions(maxClusterRadius=20)  
                 ,label = user()$miasto ,popup=paste0('<BIG>', user()$nazwa,'</BIG><br/>Members: ', user()$members,'<br/>Organizes: ', user()$organizer,"<br/><img src = ", user()$logo, " Width = 150><br/> <SMALL>source: wikipedia</SMALL> ")
      ))

  #render text information based on selected pin on map
  output$zz11<- renderText(paste(user()$kraj[user()$lon==input$map2_marker_click$lng]))
  output$zz21<- renderText(paste(user()$miasto[user()$lon==input$map2_marker_click$lng]))
  output$zz31<- renderText(paste(user()$me[user()$lon==input$map2_marker_click$lng]))
  output$zz41<- renderText(paste(user()$org[user()$lon==input$map2_marker_click$lng]))
  output$zz51<- renderText(paste(user()$nazwa[user()$lon==input$map2_marker_click$lng]))
  #render image of location based on selected pin on map
  output$image2 <- renderUI({
    tags$img(height = "300px", src = as.character( paste(cff12()$wiki[cff12()$lon==input$map2_marker_click$lng])))
  })
   #render image of group logo based on selected pin on map
  output$image22 <- renderUI({
    tags$img(height = "200px", src = as.character( paste(cff12()$logo[cff12()$lon==input$map2_marker_click$lng])))
  })
  #render table
  output$zzz22 <- renderDataTable(cff12()[cff12()$lon==input$map2_marker_click$lng ,], 
                                 options = list(scrollX = TRUE, autoWidth = TRUE))  
#########end of data related to useR tab  

#start data related to tab R Girls
 #read R girls csv data 
  girl_csv=read.csv("girl.csv", header = TRUE, sep=",",colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
#create data that is updated base on country filter, I also add conditions that when, user do not select any country, or any continent, we show all available data  
girl <- reactive({girl_csv[ gsub("\\s", "", girl_csv$kraj) %in% if(is.null(input$kraj)){if(is.null(input$cont)){cc2$country} else (cc2[cc2$continent %in% input$cont,3])} else (input$kraj),] 
}) 
# render map in leaflet based on girl.csv file  
  output$map3 <- renderLeaflet(
    leaflet(girl()) %>% addTiles() %>%
      addMarkers(lng= girl()$lon, lat= girl()$lat#, clusterOptions = markerClusterOptions(maxClusterRadius=20)  
                 ,label = girl ()$miasto ,popup=paste0('<BIG>', girl()$nazwa,'</BIG><br/>Members: ', girl()$me,'<br/>Organizes: ', girl()$orgn,"<br/><img src = ", girl()$logo, " Width = 150><br/> <SMALL>source: wikipedia</SMALL> ")
      ))
  #render text information based on selected pin on map
  output$zz1<- renderText(paste(girl()$kraj[girl()$lon==input$map3_marker_click$lng]))
  output$zz2<- renderText(paste(girl()$miasto[girl()$lon==input$map3_marker_click$lng]))
  output$zz3<- renderText(paste(girl()$me[girl()$lon==input$map3_marker_click$lng]))
  output$zz4<- renderText(paste(girl()$orgn[girl()$lon==input$map3_marker_click$lng]))
  output$zz5<- renderText(paste(girl()$nazwa[girl()$lon==input$map3_marker_click$lng]))
  #render image of location based on selected pin on map
  output$image3 <- renderUI({
    tags$img(height = "300px", src = as.character( paste(girl()$wiki[girl()$lon==input$map3_marker_click$lng])))
  })
    #render image of group logo based on selected pin on map
  output$image33 <- renderUI({
    tags$img(height = "200px", src = as.character( paste(girl()$logo[girl()$lon==input$map3_marker_click$lng])))
  })
#render table
  output$table3 <- renderDataTable(girl()[girl()$lon==input$map3_marker_click$lng,], 
                                  options = list(scrollX = TRUE, autoWidth = TRUE))
#end of data related to R Girls
  
})
#server.r end
