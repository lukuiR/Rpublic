

#####
#server
######################

# server.R

library(quantmod)
library(ggplot2)
library(plotly)

library(RColorBrewer)
#read cvs
mydat <-read.csv(file="www/2017-03-27.csv", header=TRUE, sep=";") #fread(paste('http:// adres www /csv/','2017-03-27','.csv',sep = ""))
mydat <- mydat[,c(1,10,8)]
names(mydat) <- c("Time","a1","a2")


shinyServer(function(input, output, session) {
  set.seed(122)
 
  
  ######graph

  mydat<- mydat%>%
    mutate(Time= as.POSIXct(strptime(mydat$Time,"%Y-%m-%d %H:%M:%S")))%>% 
    mutate(a1=round(as.numeric(sub(",", ".", a1, fixed = TRUE))))%>%
    mutate(a2=round(as.numeric(sub(",", ".", a2, fixed = TRUE)),1))
  
  output$plot1 <- renderPlotly(
    plot_ly(x=mydat$Time, y=mydat$a1,mode = "markers", type = 'scatter',    showlegend = FALSE,
            marker = list(colorscale = list(c(0, "green"),c(0.5,"yellow"),c(1,"red")),
                          cauto = F,
                          cmin = 0,
                          cmax = 3000, color = mydat$a1
                          ), 
                          
           hoverinfo="x+y"
    )%>%
      
      layout(       
        xaxis = list( 
                      tickangle = 0,  family = "Roboto",  tickformat="%H:%M", 
          showgrid = F),      
        yaxis = list( range =c(0,3000) , 
                      title = "Stężenie CO<sub>2</sub> [ppm]")  
      )%>% 
      config(displayModeBar = F)
    
  )

  ###############   
  #plot4
  
  
  
  output$plot4 <- renderPlotly(
    plot_ly(x=mydat$Time, y=mydat$a2,mode = "markers", type = "scatter",
            marker = list(colorscale = list(c(0, "blue"),c(0.3, "green"),c(0.5,"yellow"),c(1,"red")),
                          cauto = F,
                          cmin = 0,
                          cmax = 40, color =mydat$a2
            ), showlegend = FALSE
    )%>%
      
      layout(   
        xaxis = list( #showticklabels = TRUE,
          tickangle = 0,  family = "Roboto",  tickformat="%H:%M", 
          showgrid = F),     
        yaxis = list(    range=c(0,40),   
                         title = "Temperatura [<sup>o</sup>C]")   
      )  %>% 
      config(displayModeBar = F)
  )
  ###############    
 
  
  output$image2 <- renderImage({
    if (mydat$a1[dim(mydat)[1]]<701){
      return(list(
        src = "www/f1.png",
        contentType = "image/png",
        
        width = 400,
        alt = "Face"
      ))}
    
   else if (mysat$a1[dim(mydat)[1]]<1201) {
     return(list(
       src = "www/f2.png",
       contentType = "image/png",
       
       width = '40%',
       alt = "Face"
     ))} else if (mydat$a1[dim(mydat)[1]]<1601) {
       return(list(
         src = "www/f3.png",
         contentType = "image/png",
         
         width = '40%',
         alt = "Face"
       ))} else if (mydat$a1[dim(mydat)[1]]<2001) {
         return(list(
           src = "www/f4.png",
           contentType = "image/png",
           
           width = '40%',
           alt = "Face"
         ))}
    else{
      return(list(
        src = "www/f5.png",
        contentType = "image/png",
       
        wight ='40%',
        alt = "Face"
      ))}
    
  }, deleteFile = FALSE)

  #######
  
})