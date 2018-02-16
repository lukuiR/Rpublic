

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


shinyServer(function(input, output, session) {
  set.seed(122)
  
  ######graph
  output$zzz1<- renderText("zzz")
 ###map
  
  data <- reactiveValues(clickedMarker=NULL)
  cff1=y
  
  output$map <- renderLeaflet(
    leaflet(cff1) %>% addTiles() %>%
      addMarkers(lng=cff1$lon, lat=cff1$lat, clusterOptions = markerClusterOptions(maxClusterRadius=20)  
                 ,label = cff1$miasto ,popup=paste0('<BIG>',y$nazwa,'</BIG><br/>Members: ',y$me,'<br/>Organizes: ',y$orgn,"<br/><img src = ", y$logo, " Width = 150><br/> <SMALL>source: wikipedia</SMALL> ")
      ))
  
  #observeEvent(input$map_marker_click,{
   # data$clickedMarker <- input$map_marker_click
    #zzz <- input$map_marker_click
    #if(data$clickedMarker$lng == 22.5914){
      #updateTabItems(session, "dashboard", "tab1")
      
    #}
  #}
  #)
 
  
  ######graph
  output$zzz1<- renderText(zzz <- as.character( paste(y[y$lon==input$map_marker_click$lng,'wiki']))
                           
                           )
  output$image <- renderUI({
    tags$img(height = "200px", src = as.character( paste(y$wiki[y$lon==input$map_marker_click$lng])))
  })
  

  output$zzz2 <- renderDataTable(y[y$lon==input$map_marker_click$lng,], 
                                  options = list(scrollX = TRUE, autoWidth = TRUE))
###############

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
  
  #######
  
})