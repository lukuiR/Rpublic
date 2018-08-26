

#####
#server
######################

# server.R

library(quantmod)
library(ggplot2)
library(plotly)
library(DT)

library(RColorBrewer)
#read cvs
aa2=read.csv("cc.csv", header = TRUE, sep=",")

shinyServer(function(input, output, session) {
  set.seed(122)
  
  ##
  
  observeEvent(input$cont, {
    cont <- input$cont
    
    # Available model options based on cont
    choices <- aa2[aa2$continent %in% cont,3]
    
    # Update the kraj input object to only display wanted choices                  
    updateSelectInput(session, "kraj", choices = choices)
  })
  
  
  
  ##########
  
  ######graph
  output$zzz1<- renderText("zzz")
 ###map
  
  cff11=read.csv("girl.csv", header = TRUE, sep=",",colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  cff1 <- reactive({cff11[ gsub("\\s", "", cff11$kraj) %in% if(is.null(input$kraj)){if(is.null(input$cont)){aa2$country} else (aa2[aa2$continent %in% input$cont,3])} else (input$kraj),] 
    
  }) 
  
  output$map <- renderLeaflet(
    leaflet(cff1()) %>% addTiles() %>%
      addMarkers(lng=cff1()$lon, lat=cff1()$lat#, clusterOptions = markerClusterOptions(maxClusterRadius=20)  
                 ,label = cff1()$miasto ,popup=paste0('<BIG>',cff1()$nazwa,'</BIG><br/>Members: ',cff1()$me,'<br/>Organizes: ',cff1()$orgn,"<br/><img src = ", cff1()$logo, " Width = 150><br/> <SMALL>source: wikipedia</SMALL> ")
      ))
  

  
  ######graph
                           
  output$image <- renderUI({
    tags$img(height = "200px", src = as.character( paste(cff1$wiki[cff1$lon==input$map_marker_click$lng])))
  })
  

  output$zzz2 <- renderDataTable(cff1()[cff1()$lon==input$map_marker_click$lng,], 
                                  options = list(scrollX = TRUE, autoWidth = TRUE))
###############2
  cff2=read.csv("user.csv", header = TRUE, sep=",",colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
  cff2$lon[is.na(cff2$lon)] <- 0
  cff2$lat[is.na(cff2$lat)] <- 0
  
  cff12 <- reactive({cff2[ gsub("\\s", "", cff2$kraj) %in% if(is.null(input$kraj)){if(is.null(input$cont)){aa2$country} else (aa2[aa2$continent %in% input$cont,3])} else (input$kraj),] 
    
  }) 
  
  output$map2 <- renderLeaflet(
    leaflet(cff12()) %>% addTiles() %>%
      addMarkers(lng=cff12()$lon, lat=cff12()$lat#, clusterOptions = markerClusterOptions(maxClusterRadius=20)  
                 ,label = cff12()$miasto ,popup=paste0('<BIG>',cff12()$nazwa,'</BIG><br/>Members: ',cff12()$members,'<br/>Organizes: ',cff12()$organizer,"<br/><img src = ", cff12()$logo, " Width = 150><br/> <SMALL>source: wikipedia</SMALL> ")
      ))

  
  output$zzz22 <- renderDataTable(cff12()[cff12()$lon==input$map2_marker_click$lng ,], 
                                 options = list(scrollX = TRUE, autoWidth = TRUE))
  
  
#########2  
  
  ###############3
  
  cff123=read.csv("conf.csv", header = TRUE, sep=",",colClasses=c("NULL",NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))

  
  dataInput <- reactive({cff123[cff123$year %in%  input$yearr & gsub("\\s", "", 
                                    cff123$id) %in% if(is.null(input$kraj)){if(is.null(input$cont)){aa2$country} else (aa2[aa2$continent %in% input$cont,3])} else (input$kraj),] 
  
    })

  
  output$map23 <- renderLeaflet(
    leaflet(dataInput()) %>% addTiles() %>%
      addMarkers(lng=dataInput()$lon, lat=dataInput()$lat#, clusterOptions = markerClusterOptions(maxClusterRadius=20)  
                 ,label = dataInput()$city ,popup=paste0('<BIG>',dataInput()$nazwa,'</BIG><br/>Web: ',dataInput()$web,'<br/>Date: ',dataInput()$date,'<br/>Year: ',dataInput()$year,"<br/><img src = ", dataInput()$wiki, " Width = 150><br/> <SMALL>source: wikipedia</SMALL> ")
      ))
  
  output$zzz223 <- renderDataTable(dataInput()[dataInput()$lon==input$map23_marker_click$lng,], 
                                  options = list(scrollX = TRUE, autoWidth = TRUE))
  
  
  #########3
 url1<- reactive({"webshot.png"
    webshot(as.character( paste(y[y$lon==input$map_marker_click$lng,'wiki'])), delay = 0.5)
    if (is.null(input$map_marker_click$lng)){NULL}
    else("webshot.png")
  })

  
  output$image2 <- renderImage({
    try(list(
        src = url1(),
        contentType = "image/png",
        
        width = 400,
        alt = "Face"
      ),options(show.error.messages = FALSE))
    
  }, deleteFile = FALSE)
  
  #######test
  output$selected_var <- renderText({ 
    paste(input$yearr,",")
  })

  
})