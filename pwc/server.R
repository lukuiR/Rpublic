

#####
#server
######################

# server.R

library(quantmod)
library(ggplot2)
library(plotly)

library(RCurl)
library(plyr)
library(leaflet)
library(RColorBrewer)

mydat <- fread(paste('http://aerisqualitas.org/coyote/csv/',Sys.Date(),'.csv',sep = ""))
dat1<-head(mydat,1)



c4<-c(51.2527)#,
#51.110,
#51.119)
c5<-c(22.5914)#,
#17.030,
#15.909)
c3<-c("SP nr 10 w Lubinie")#,
#    "Wroclaw",
#  "Zlotoryja")

cff1<-data.frame(dat1[,1],dat1[,7],dat1[,8],dat1[,9],dat1[,10],c3,c4,c5)
colnames(cff1)<-c("a","aa","a1","aa1","aaa","miasto","lat","lng")



formularz <- getURL("https://raw.githubusercontent.com/whyRconf/konkursy/master/dane_z_formularza_rejestracyjnego.csv")
dane <- read.csv(text = formularz)

d3=as.data.frame(table(iconv(dane[,16] ,"UTF-8" , "ASCII//TRANSLIT") ))
colnames(d3)=c("miasto","freq")



dat12=as.data.frame(table(gsub(' ','',(gsub('"','',sapply(unlist(strsplit(as.matrix(dane)[,11], ",")),tolower))))))
dat14=head(dat12[order(-dat12[,2]),],10)
dat14=rbind(dat14,data.frame(Var1="Other",Freq=sum(dat12[,2])-sum(dat14[,2])))

dat2=as.data.frame(table(gsub(' ','',(gsub('"','',sapply(unlist(strsplit(as.matrix(dane)[,10], ",")),tolower))))))
dat11=head(dat2[order(-dat2[,2]),],10)
dat11=rbind(dat11,data.frame(Var1="Other",Freq=sum(dat2[,2])-sum(dat11[,2])))


colors <- c('rgb(211,94,96)', 'rgb(128,133,133)', 'rgb(144,103,167)', 'rgb(171,104,87)', 'rgb(114,147,203)')
d1=cbind(iconv(dane[,17] ,"UTF-8" , "ASCII//TRANSLIT") ,dane[,18])
d2=count(d1[complete.cases(d1),])



map1=matrix(c('Warszawa',	 	52.260,	21.020,
             'Lodz'	,	51.770,	19.460,
             'Krakow'	,	50.060,	19.960,
             'Wroclaw'	,	51.110,	17.030,
             'Poznan'	,	52.400,	16.900,
             'Gdansk'	,	54.360,	18.640,
             'Czestochowa'	,50.810,	19.130,
             'Gdynia'	,	54.520,	18.530,
             'Katowice'	,	50.260,	19.020,
             'Bialystok'	,	53.140,	23.160,
             'Lublin'	,	51.240,	22.570,
             'Bydgoszcz'	,	53.120,	18.010,
             'Bialowieza' ,52.701111, 23.866667,
             'Cisownica', 49.722778, 18.761389,
             'Hasselt', 50.916667, 5.333333,
             'Heidelberg',49.416667, 8.716667,
             'Komorow', 52.144444, 20.815278,
             'Londyn', 51.5, -0.116667,
             'Plock', 52.547222, 19.7,
             'Proszowice', 50.193139, 20.288717,   
             'Pruszkow',52.165556, 20.805556,
             'Rybnik',50.098611, 18.545,
             'Sliema', 35.912222, 14.504167,
             'Sopot', 54.441944, 18.559722,
             'Tomaszow Maz',   51.531389, 20.008889,
             'Torun',  53.022222, 18.611111,
             'Wiatowice' ,  49.949444, 20.213333,
             'Wygledy'   ,52.259167, 20.702778,
             'Zielona Gora',   51.939722, 15.505,
             'Zyrardow' , 52.054167, 20.444444), ncol=3, byrow=TRUE)


c41<-c(as.numeric( map1[,2]))#,
#51.110,
#51.119)
c51<-c(as.numeric(map1[,3]))#,
#17.030,
#15.909)
c31<-c(map1[,1])#,
#    "Wroclaw",
#  "Zlotoryja")

cff2<-data.frame(c31,c41,c51)
colnames(cff2)<-c("miasto","lat","lng")

d1=cbind(iconv(dane[,16] ,"UTF-8" , "ASCII//TRANSLIT") ,as.character( dane[,14]))
d2=count(d1[complete.cases(d1),])
d2=d2[d2$x.2=="Kobieta",]
colnames(d2)<-c("miasto",  "x.2",  "truee")
d4=merge(d3,d2,all = TRUE)

d4$rat <- ifelse(is.na( d4$truee)==TRUE, 1, 1-d4$truee/d4$freq)

cff2=merge(cff2, d4 )

cff2$size <- ifelse(cff2$freq > 60, '>60', ifelse(cff2$freq > 19, '60-20', ifelse(cff2$freq > 5, '19-6', '5-1')))

cff2$col <- ifelse(cff2$rat > 0.9, 'red', ifelse(cff2$rat > 0.6, 'orange', ifelse(cff2$rat > 0.3, 'purple', 'navy')))


html_legend <- "<img src='''>Marker size<br/><img src='''>Ilość uczestników<br/>
<img src='http://img.freepik.com/free-icon/original_318-115225.jpg?size=338c&ext=jpg'
style='width:6px;height:6px;'><5<br/>
<img src='http://img.freepik.com/free-icon/original_318-115225.jpg?size=338c&ext=jpg'
style='width:11px;height:11px;'>6-19<br/>
<img src='http://img.freepik.com/free-icon/original_318-115225.jpg?size=338c&ext=jpg'
style='width:22px;height:22px;'>20-60<br/>
<img src='http://img.freepik.com/free-icon/original_318-115225.jpg?size=338c&ext=jpg' 
style='width:35px;height:35px;'>>60"





shinyServer(function(input, output, session) {
 
  
  
  #####map

  
  
  output$map <- renderLeaflet(
    leaflet(cff2) %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron) %>% 
      addCircleMarkers(
        radius = ~ifelse(cff2$freq > 60, 20, ifelse(cff2$freq > 20, 11, ifelse(cff2$freq > 5, 6, 3))),
        color = cff2$col,
        stroke = FALSE, fillOpacity = 0.7,
        label = paste(cff2$miasto,"-",cff2$freq,"-",round(100*cff2$rat,2),"%")
      )%>%leaflet::addLegend("bottomright", 
                             colors =c("red","orange","purple","navy"),
                             labels=c(">90%","61-90%","31-60%","30%>"), 
                             title= "Proporcja Meżczyzn do Kobiet",
                             opacity = 1)%>%
      addControl(html = html_legend, position = "bottomleft", className = "info legend"))
  

  
  
  
  


  ###############
  #plot3
  
  
  
  output$plot3 <- renderPlotly(
    plot_ly(dat11,labels = dat11$Var1, values = dat11$Freq, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            #hoverinfo = 'text',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = TRUE)  
  )

 
  ###############    
  #plot5
  
  
  
  output$plot5 <- renderPlotly(
    plot_ly(dat14,labels = dat14$Var1, values = dat14$Freq, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            #hoverinfo = 'text',
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            #The 'pull' attribute can also be used to create space between the sectors
            showlegend = FALSE)  
 
  )
  ###############  
  
  
  
  
  
  #######
  
  
  
})