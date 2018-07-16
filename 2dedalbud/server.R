#####
#server
######################

#dt ext https://rstudio.github.io/DT/extensions.html
###

# server.R

library(quantmod)
library(ggplot2)
library(plotly)
library(DT)
library(RColorBrewer)

ssss=paste(as.character(Sys.time()), as.character(runif(1, 0, 2)))

#coord
{

c4<-c(51.1334)
c5<-c(15.919)
c3<-c("Zlotoryja, Grunwaldzka 21")
c2<-c("Darmowa")
c1<-c(10)
cff1<-data.frame(c3,c4,c5)
colnames(cff1)<-c("miasto","lat","lng")



c4<-c(51.1334,51.1334,51.1334)
c5<-c(15.919,15.919,15.919)
c2<-c("Darmowa","towar za 500-2000zĹ‚","powyĹĽej 2000zĹ‚" )
c1<-c(10.5,25.1,50.3)
c6<-c("green","blue","red")
cff2<-data.frame(c4,c5,c2,c1,c6)
colnames(cff2)<-c("lat","lng","legenda","pop","kol")
}

shinyServer(function(input, output, session) {
  set.seed(122)
  
  

  #####map
  {
  
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
                                    
                                    >1000zĹ‚,51.1334,15.919,855300
                                    501-1000zĹ‚,51.1334,15.919,240600
                                    200-500zĹ‚â•‘,51.1334,15.919,82501
                                    Darmowa,51.1334,15.919,14596
                                    
                                    "))
  
  output$map1 <- renderLeaflet(leaflet(cities) %>% addTiles() %>% 
                                 setView(lat = 51.1334, lng = 16, zoom = 10)%>%
                                 addCircles(lng = ~Long, lat = ~Lat, weight = 1,
                                            radius = ~sqrt(Pop) * 30,popup = ~City, color = ~c6, fillOpacity = 0.1
                                 )
  )
  
  
  
  
  

  
  }
  #######
  
  #######koszyk
  
  #data prep
  {
  mtcars2 = read.csv('dat3.csv', header = TRUE, sep=",",quote ='"')
  mtcars2=mtcars2[,-5]
  mtcars2$Ilosc=0
  mtcars2[,3]=as.numeric(gsub(",", ".", mtcars2[,3]))
  mtcars2$Dodaj=paste0('
           <div class="btn-group" role="group" aria-label="Basic example">
         <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(mtcars2),'>Do koszyka</button>
         </div>
         
         ')
  
  # download the filtered data
  }
  #prod
  {
  
    tow=reactiveValues()
    tow$Data=data.table(
      mtcars2

    )
    
    
  #output$prod = DT::renderDataTable(datatable(mtcars2 ,rownames = FALSE ,escape=F),server = TRUE)
  output$prod=renderUI({
    
    box(width=12,
        column(12,dataTableOutput("Main_prod")),
        tags$script(HTML('$(document).on("click", "input", function () {
                         var checkboxes = document.getElementsByName("row_selected");
                         var checkboxesChecked = [];
                         for (var i=0; i<checkboxes.length; i++) {
                         if (checkboxes[i].checked) {
                         checkboxesChecked.push(checkboxes[i].value);
                         }
                         }
                         Shiny.onInputChange("checked_rows",checkboxesChecked);})')),
        tags$script("$(document).on('click', '#Main_prod button', function () {
                    Shiny.onInputChange('lastClickId',this.id);
                    Shiny.onInputChange('lastClick', Math.random()) });")
        
        )
    
    
    }) 
 
  output$Main_prod=renderDataTable({ DT=tow$Data[]
                                      datatable(DT[,-1] ,rownames = FALSE,
                                                options = list(
                                        deferRender = TRUE,
                                        scrollX = TRUE,
                                        columnDefs = list(
                                          list(orderable = FALSE, className = 'details-control', targets = 0)
                                        )
                                      ) 
                                               ,escape=F)
  
  })
  
  
  vals=reactiveValues()
  vals$Data=data.table(
    head(mtcars2,0)
    
  )
  
  observeEvent(input$lastClick,
               {
                 selected_row=as.numeric(gsub("modify_","",input$lastClickId))
                    new_row=tow$Data
                    new_row$Ilosc[selected_row]=1
                    tow$Data<-new_row
                    vals$Data<-tow$Data[tow$Data$Ilosc>0,]
                    vals$Data$Dodaj=paste0('
           <div class="btn-group" role="group" aria-label="Basic example">
                                         <button type="button" class="btn btn-secondary modify"id=modify_',1:nrow(vals$Data),'>Dodaj</button>
                                         </div>
                                         
                                         ')
                    
                    loggit("INFO", "lastClick",paste('modify: ',gsub("modify_","",input$lastClickId)) ,ssss, app = "input")
               }
  )
  
  }

  #####new table
   {

  output$KKosz=renderUI({

    box(width=12,
        column(12,dataTableOutput("Main_kosz")),
        tags$script(HTML('$(document).on("click", "input", function () {
                         var checkboxes = document.getElementsByName("row_selected");
                         var checkboxesChecked = [];
                         for (var i=0; i<checkboxes.length; i++) {
                         if (checkboxes[i].checked) {
                         checkboxesChecked.push(checkboxes[i].value);
                         }
                         }
                         Shiny.onInputChange("checked_rows",checkboxesChecked);})')),
        tags$script("$(document).on('click', '#Main_kosz button', function () {
                    Shiny.onInputChange('lastClickkId',this.id);
                    Shiny.onInputChange('lastClickk', Math.random()) });")

        )


    })


  ##The code may seem weird but we will modify it later
  output$Main_kosz=renderDataTable({ DT=vals$Data[]
  if(dim(vals$Data[])[1]>0){
    DT[["Suma"]]<-vals$Data$Ilosc*vals$Data[,3]
  DT[["Actions"]]<-
    paste0('
           <div class="btn-group" role="group" aria-label="Basic example">
           <button type="button" class="btn btn-secondary delete" id=delete_',1:nrow(vals$Data),'>Usun</button>
           </div>

           ')
  }
  datatable(DT[,-1],
                         options = list(
                           dom = 'Bfrti',
                           autoWidth = FALSE,
                           deferRender = TRUE,
                           scrollX = TRUE,
                           scrollY = "51vh",
                           scroller = TRUE,
                           scollCollapse = TRUE,
                           fixedHeader = TRUE,
                           columnDefs = list(
                             list(orderable = FALSE, className = 'details-control', targets = 0)
                           )
                         ) ,escape=F)

  })


  observeEvent(input$lastClickk,
               {
                 if (input$lastClickkId%like%"delete")
                 {
                   row_to_del=as.numeric(gsub("delete_","",input$lastClickkId))
                   
                   tow$Data$Ilosc[as.numeric(vals$Data[row_to_del,1])]=0
                   vals$Data=vals$Data[-row_to_del,]
            
                 }
                 else if (input$lastClickkId%like%"modify")
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
    selected_row=as.numeric(gsub("modify_","",input$lastClickkId))
    old_row=vals$Data[selected_row,2:5]
    row_change=list()
    row_change[[1]]=vals$Data[selected_row,2]
    row_change[[2]]=vals$Data[selected_row,3]
    row_change[[3]]=vals$Data[selected_row,4]
    row_change[[4]]=paste0('<input class="new_input" type="number" id=new_',4,'><br>')
    row_change=as.data.table(row_change)
    setnames(row_change,colnames(old_row))
    DT=rbind(old_row,row_change)
    rownames(DT)<-c("Current values","New values")
    DT

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
                 vals$Data[as.numeric(gsub("modify_","",input$lastClickkId)),5]<-DF
               }
  )
  ######
  
  }
  
  observe({
    loggit("INFO", paste('tab: ',input$tabs),ssss, app = "tab")
  })
  
  session$onSessionEnded(function(){loggit("INFO", "app has stopped",ssss, app = "stop")})
  
  })
