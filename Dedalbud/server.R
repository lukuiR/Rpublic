

#####
#server
######################

# server.R

library(quantmod)
library(ggplot2)
library(plotly)
library(DT)
library(RColorBrewer)

#c4<-lat#c(48.234,48.230,48.228)
#c5<-lon#c(16.360,16.366,16.365)


c4<-c(51.1334)
c5<-c(15.919)
c3<-c("Zlotoryja, Grunwaldzka 21")
c2<-c("Darmowa")
c1<-c(10)
cff1<-data.frame(c3,c4,c5)
colnames(cff1)<-c("miasto","lat","lng")



c4<-c(51.1334,51.1334,51.1334)
c5<-c(15.919,15.919,15.919)
c2<-c("Darmowa","towar za 500-2000zł","powyżej 2000zł" )
c1<-c(10.5,25.1,50.3)
c6<-c("green","blue","red")
cff2<-data.frame(c4,c5,c2,c1,c6)
colnames(cff2)<-c("lat","lng","legenda","pop","kol")

shinyServer(function(input, output, session) {
  set.seed(122)
  
  
  #####map
  data <- reactiveValues(clickedMarker=NULL)
  

    
  output$map <- renderLeaflet(
    leaflet(cff1) %>% addTiles() %>%
      addMarkers(lng=cff1$lng, lat=cff1$lat, clusterOptions = markerClusterOptions(maxClusterRadius=2)  
                 ,label = cff1$miasto 
      ))
  observeEvent(input$map_marker_click,{
    data$clickedMarker <- input$map_marker_click
    if(data$clickedMarker$lng == 15.919){
      updateTabItems(session, "tabs", "dashboard")
    }
  }
  )
  
  cities <- read.csv(textConnection("
City,Lat,Long,Pop
                                    
                                    >1000zł,51.1334,15.919,855300
                                    501-1000zł,51.1334,15.919,240600
                                    200-500zł║,51.1334,15.919,82501
                                    Darmowa,51.1334,15.919,14596
 
                                    "))
  
  output$map1 <- renderLeaflet(leaflet(cities) %>% addTiles() %>% 
                                 setView(lat = 51.1334, lng = 16, zoom = 10)%>%
                                 addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                                            radius = ~sqrt(Pop) * 30,popup = ~City, color = ~c6, fillOpacity = 0.1
                                 )
    )
  

  
  
  
  ###############  
  
  output$image2 <- renderImage({
    
          list(
        src = "www/f1.png",
        contentType = "image/png",
        
        width = 400,
        alt = "Face"
      )
    
  }, deleteFile = FALSE)
  
  
  
  
  #######
  
  #######koszyk
  mtcars2 = cbind(iris[,5:2],row.names(iris[,5:2]))
  
  
  
  # download the filtered data

  output$x4 = DT::renderDataTable(mtcars2[,1:4], server = TRUE)
  
  output$x5 = renderPrint({
    cat('\n\nSelected rows:\n\n')
    cat(input$x4_rows_selected, sep = ', ')
    cat('\n\nSelected Last rows:\n\n')
    cat(tail(input$x4_rows_selected, n=1))
    cat('\n\nSelected cars:\n\n')
    cat(row.names( mtcars2[input$x4_rows_selected,]), sep = ', ')
    cat('\n\nSelected cars2:\n\n')
    cat(mtcars2[input$x4_rows_selected,2], sep = ', ')
    cat('\n\nSelected cars3:\n\n')
    cat(mtcars2[input$x4_rows_selected,3], sep = ', ')
    
  })
  

  
  output$x6 = DT::renderDataTable( 
    datatable(mtcars2[input$x4_rows_selected,1:4], escape = FALSE, options = list(dom = 't', paging = FALSE, ordering = FALSE)), 
    server = TRUE)
  

 #####new table
  
  vals=reactiveValues()
 #  if(is.null(input$x4_rows_selected)){
    vals$Data=data.table(
      mtcars2[1:5,]
  
    )
 #  }
    
    
    
 
  ##The Body is classic, we just used the group button to improve the render
  ##And to show the user the actions are related
  output$MainBody=renderUI({
    
      box(width=12,
          column(12,dataTableOutput("Main_table")),
          tags$script(HTML('$(document).on("click", "input", function () {
                           var checkboxes = document.getElementsByName("row_selected");
                           var checkboxesChecked = [];
                           for (var i=0; i<checkboxes.length; i++) {
                           if (checkboxes[i].checked) {
                           checkboxesChecked.push(checkboxes[i].value);
                           }
                           }
                           Shiny.onInputChange("checked_rows",checkboxesChecked);})')),
          tags$script("$(document).on('click', '#Main_table button', function () {
                  Shiny.onInputChange('lastClickId',this.id);
                  Shiny.onInputChange('lastClick', Math.random()) });")
          
      )
    
    
  })
  
  
  ##The code may seem weird but we will modify it later
  output$Main_table=renderDataTable({ DT=vals$Data[,1:4] 
  DT[["Actions"]]<-
    paste0('
               <div class="btn-group" role="group" aria-label="Basic example">
               <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(vals$Data),'>Usun</button>
               <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(vals$Data),'>Zmien</button>
               </div>
               
               ')
  datatable(DT, escape=F)
  
  })
  ##addd
  observeEvent(input$x4_rows_selected,{
    new_row=mtcars2[tail(input$x4_rows_selected, n=1),]
    vals$Data<-rbind(vals$Data,new_row)
  })
  
  observeEvent(input$Del_row_head,{
    row_to_del=as.numeric(gsub("Row","",input$checked_rows))
    vals$Data=vals$Data[-row_to_del]}
  )
  
  observeEvent(input$lastClick,
               {
                 if (input$lastClickId%like%"delete")
                 {
                   row_to_del=as.numeric(gsub("delete_","",input$lastClickId))
                   vals$Data=vals$Data[-row_to_del]
                 }
                 else if (input$lastClickId%like%"modify")
                 {
                   showModal(modal_modify)
                 }
               }
  )
  ##
  modal_modify=modalDialog(
    fluidPage(
      h3(strong("Row modification"),align="center"),
      hr(),
      dataTableOutput('row_modif'),
      actionButton("save_changes","Zapisz"),
      tags$script(HTML("$(document).on('click', '#save_changes', function () {
                       var list_value=[]
                       for (i = 0; i < $( '.new_input' ).length; i++)
                       {
                       list_value.push($( '.new_input' )[i].value)
                       }
                       Shiny.onInputChange('newValue', list_value)
});"
                           ))
      
      
      ),
    size="m"
    )
  ##co modyfikuje   
  output$row_modif<-renderDataTable({
    selected_row=as.numeric(gsub("modify_","",input$lastClickId))
    old_row=vals$Data[selected_row]
    row_change=list()
    for (i in colnames(old_row)[1:4])
    {
      if (is.numeric(vals$Data[[i]]))
      {
        row_change[[i]]<-paste0('<input class="new_input" type="number" id=new_',i,'><br>')
      }
      else
        row_change[[i]]<-paste0('<input class="new_input" type="text" id=new_',i,'><br>')
    }
    row_change[[1]]=vals$Data[selected_row,1]
    row_change[[2]]=vals$Data[selected_row,2]
    row_change[[4]]=vals$Data[selected_row,4]
    row_change[[5]]=vals$Data[selected_row,5]
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT=rbind(old_row,row_change)
    rownames(DT)<-c("Current values","New values")
    DT[,1:4]
    
  },escape=F,options=list(dom='t',ordering=F),selection="none"
  )
  ##     
  observeEvent(input$newValue,
               {
                 newValue=lapply(input$newValue, function(col) {
                   if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
                     as.numeric(as.character(col))
                   } else {
                     col
                   }
                 })
                 DF=data.frame(lapply(newValue, function(x) t(data.frame(x))))
                 #colnames(DF)=colnames(vals$Data)
                 vals$Data[as.numeric(gsub("modify_","",input$lastClickId)),3]<-DF
                 vals$Data[as.numeric(gsub("modify_","",input$lastClickId)),4]<-as.numeric( DF)*as.numeric(vals$Data[as.numeric(gsub("modify_","",input$lastClickId)),2])
               }
  )
  ######
  
  
  
})