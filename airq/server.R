# server.R
#info https://rstudio.github.io/leaflet/shiny.html
library(quantmod)
library(ggplot2)
library(plotly)
library(sp)
library(lattice)
library(colorRamps)
library(colorspace)
library(RColorBrewer)
shinyServer(function(input, output) {
  
  # shp file frome data folder
  sstate <- readOGR("./data/BristolANPR_StudyArea.shp",layer = "BristolANPR_StudyArea")
  anpr <- readOGR("./data/ANPR_Traffic.shp",layer = "ANPR_Traffic")
  anpr1 <- readOGR("./data/aaa.shp",layer = "aaa")
  
  
  #csv file from data folder
  db1<-read.csv("./data/5_Links.csv",header = TRUE)
  proc_use1<-read.csv("./data/dat1.csv",header = TRUE)
  tf_avg1<-read.csv("./data/dat2.csv",header = TRUE)
  info1<-read.csv("./data/info1.csv",header = TRUE)
  interval<-read.csv("./data/interval.csv",header = TRUE)
  time1<-read.csv("./data/time.csv",header = TRUE)
  tfstime<-read.csv("./data/tfstime.csv",header = TRUE)
  
  
  # merge shp and csv by "Source_nam" attribute column
  sssstate <- merge(sstate, tf_avg1, by='Source_nam')
  ssstate <- merge(sssstate, proc_use1, by='Source_nam')
  
  
  ###############################################################################################
  #first tab traffic flow/speed
  
  # filtered data
  
  # use input prom button filter "dataset"
  # reactive filtered data
  filteredData <- reactive({tf<-cbind(tf_avg1[,1],tf_avg1[,as.numeric( input$dataset) ])
  })
  
  # reactive filtered data
  filteredData1 <- reactive({
    
    tf<-cbind(tf_avg1[,1],tf_avg1[,as.numeric( input$dataset)])
    nam1="aa"
    if(input$dataset==9){nam1="Average Speed"}
    else{nam1="Average Total Flow"}
    colnames(tf)=c("Source_nam",nam1)
    # names inside the bracket do not means anything
    sssstate <- merge(sstate,tf, by='Source_nam')
    sssstate <- merge(sssstate, tf_avg1, by='Source_nam')
    sssstate <- merge(sssstate, proc_use1, by='Source_nam')
    
  })
  # map legend color
  coll <- reactive({ nam1="aa"
  if(input$dataset>8){nam1=brewer.pal(7,"RdYlGn")}
  else{nam1=colorRamps::green2red}
  nam1
  })
  
  
  # creating popup for ink on map
  clr <- rep("grey", dim(ssstate)[1])
  fldr <- tempfile()
  dir.create(fldr)
  pop <- lapply(seq(dim(ssstate)[1]), function(i) {
    df <-data.frame(group = c("LGV", "HGV"),
                    value = c(as.numeric(sub("%", "",ssstate$All.LDV....[i])), as.numeric(sub("%", "",ssstate$All.HDV....[i]))))
    p<- ggplot(df, aes(x="", y=value, fill=group))+
      geom_bar(width = 1, stat = "identity")+ scale_fill_brewer("Blues") + coord_polar("y", start=0)+ theme_void()+
      geom_text(aes(y = df[,2]/3 + c(0, cumsum(df[,2])[-length(df[,2])]), 
                    label = paste(df[,2],"%",sep = "")), size=5)
    
    svg(filename = paste(fldr, "test.svg", sep = "/"), 
        width = 250 * 0.01334, height = 250 * 0.01334)
    print(p)
    dev.off()
    tst <- paste(readLines(paste(fldr, "test.svg", sep = "/")), collapse = "")
    return(tst)
    
  })
  
  
  
  # map code output
  output$map <- renderLeaflet({
    m <- mapview(filteredData1(),lwd=6,color = coll()#colorRamps::green2red
                 ,popup =pop, zcol=c(5), legend=TRUE)
    #joining street layer with points
    m1<-mapview(anpr1,color ="Purple")
    m2<-mapview(anpr, color ="Black")
    m<-m1+m
    m<-m2+m
    m@map
  })
  
  # observe changes in filter
  observe({
    dat<-filteredData1()
    leafletProxy("map", data = dat) 
  })
  
  # end tab traffic flow/speed
  
  ###################################################################################################################
  
  #tab emision profile
  
  # data with filter "range" input
  filteredinterval<- reactive({
    zz<-interval[interval$time>input$range[1]&interval$time<input$range[2],]
    
    group1<-aggregate(as.numeric( zz$Emission.gkm), by=list(Source_nam=zz$Source_nam), FUN=mean)
    group2<-aggregate(as.numeric( zz$Petrol.Emission), by=list(Source_nam=zz$Source_nam), FUN=mean)
    group3<-aggregate(as.numeric( zz$Diesel.Emission), by=list(Source_nam=zz$Source_nam), FUN=mean)
    group<-cbind(group1[,1],group1[,2],group2[,2],group3[,2])
    colnames(group)<-c("Source_nam","Avg Emission","Avg Petrol Emission","Avg Diesel Emission")
    sssstate <- merge(sstate, group, by='Source_nam')
    
  })
  
  # petro/diesel popup
  clr1 <- rep("grey", dim(ssstate)[1])
  fldr1 <- tempfile()
  dir.create(fldr1)
  pop1 <- lapply(seq(dim(ssstate)[1]), function(i) {
    group22<-aggregate(as.numeric( interval$Petrol.Emission), by=list(Source_nam=interval$Source_nam), FUN=mean)
    group33<-aggregate(as.numeric( interval$Diesel.Emission), by=list(Source_nam=interval$Source_nam), FUN=mean)
    
    df <-data.frame(group = c("Petrol", "Diesel"),
                    value = c(round(group22[i,2]*100/(group22[i,2]+group33[i,2]), digits = 0), round(group33[i,2]*100/(group22[i,2]+group33[i,2]), digits = 0)))
    p<- ggplot(df, aes(x="", y=value, fill=group))+
      geom_bar(width = 1, stat = "identity")+ scale_fill_brewer("Blues") + coord_polar("y", start=0)+ theme_void()+
      geom_text(aes(y = df[,2]/3 + c(0, cumsum(df[,2])[-length(df[,2])]), 
                    label = paste(df[,2],"%",sep = "")), size=5)
    
    svg(filename = paste(fldr, "test.svg", sep = "/"), 
        width = 250 * 0.01334, height = 250 * 0.01334)
    print(p)
    dev.off()
    tst <- paste(readLines(paste(fldr, "test.svg", sep = "/")), collapse = "")
    return(tst)
    
  })
  
  #map ootput
  output$mapplot <- renderLeaflet({
    
    m <- mapview(filteredinterval(),lwd=6,popup =pop1, color = colorRamps::matlab.like, zcol=c(5), legend=TRUE)
    
    m1<-mapview(anpr1,color ="Purple")
    m2<-mapview(anpr, color ="Black")
    
    m<-m1+m
    m<-m2+m
    m@map
  })
  observe({
    leafletProxy("mapplot", data = filteredData())
    
  })
  
  #end tab emision profile
  ###################################################################
  
  # tabl Spare emision
  
  # data input
  coo <- reactive({
    inFile <- input$file1
    if (is.null(inFile))
      return(NULL)
    zz=read.csv(inFile$datapath, header = input$header,
                sep = input$sep, quote = input$quote)
    zz
  })
  # merge shp with imported csv
  importData <- reactive({
    sssstate <- merge(sstate, coo(), by='Source_nam')
    
  })
  
  # render map
  output$mapimport <- renderLeaflet({
    
    m <- mapview(if(length(importData()) ==0){filteredinterval()} 
                 else{importData()},lwd=6, color = colorRamps::matlab.like, 
                 zcol=if(length(importData()) ==0){c(5)} else{c(as.numeric(input$radio))}, legend=TRUE)
    
    m1<-mapview(anpr1,color ="Purple")
    m2<-mapview(anpr, color ="Black")
    m<-m1+m
    m<-m2+m
    m@map
  })
  
  observe({
    leafletProxy("mapimport", data = importData())
    
  })
  # render tabble below map of imported data
  output$cont2<- renderDataTable(coo(),  options = list(autoWidth = TRUE)
                                 
  )
  #end tab Spare Emission
  ################################################################################
  
  
  # tab graph
  # data transformation 
  tt<-cbind(as.numeric(time1[,1]),as.integer(time1[,2]),as.integer(time1[,3]))
  colnames(tt)<-c("time","flow","Sat")
  
  ##plot1
  output$trendPlot <- renderPlotly({p<-plot_ly(x=c(tt[1:6,1],tt[20:24,1]) ,y=c(tt[1:6,2],tt[20:24,2]),name="off",type="bar")
  p<-add_trace(x=tt[7:9,1],y=tt[7:9,2],name="am", type="bar")
  p<-add_trace(x=tt[10:15,1],y=tt[10:15,2],name="ip",type="bar")
  p<-add_trace(x=tt[16:19,1],y=tt[16:19,2],name="pm",type="bar")
  p<-add_trace(x=tt[,1],y=tt[,3],name="SAT (excluded)")
  layout(p, xaxis = list(title = "Hour"),yaxis = list(title = "Traffic Emission"))
  })
  #plot2
  output$trendPlot2 <- renderPlotly({p<-plot_ly(x=time1[,1] ,y=time1[,10],name="Weekday")
  p<-add_trace(x=time1[,1] ,y=time1[,8],name="Sat")
  p<-add_trace(x=time1[,1] ,y=time1[,9],name="Sun")
  layout(p, xaxis = list(title = "Hour"),yaxis = list(title = "Traffic Emission"))
  })
  #plot3
  output$trendPlot3 <- renderPlotly({p<-plot_ly(x=proc_use1[,1] ,y=proc_use1[,4],name=colnames(proc_use1[4]), type = "bar")
  p<-add_trace(x=proc_use1[,1] ,y=proc_use1[,5],name=colnames(proc_use1[5]), type = "bar")
  p<-add_trace(x=proc_use1[,1] ,y=proc_use1[,32],name=colnames(proc_use1[32]), type = "bar")
  p<-add_trace(x=proc_use1[,1] ,y=proc_use1[,7],name=colnames(proc_use1[7]), type = "bar")
  p<-add_trace(x=proc_use1[,1] ,y=proc_use1[,8],name=colnames(proc_use1[8]), type = "bar")
  p<-add_trace(x=proc_use1[,1] ,y=proc_use1[,12],name=colnames(proc_use1[12]), type = "bar")
  p<-add_trace(x=proc_use1[,1] ,y=proc_use1[,9],name=colnames(proc_use1[9]), type = "bar")
  p<-add_trace(x=proc_use1[,1] ,y=proc_use1[,10],name=colnames(proc_use1[10]), type = "bar")
  p<-add_trace(x=proc_use1[,1] ,y=proc_use1[,11],name=colnames(proc_use1[11]), type = "bar")
  p<-add_trace(x=proc_use1[,1] ,y=proc_use1[,13],name=colnames(proc_use1[13]), type = "bar")
  p<-add_trace(x=proc_use1[,1] ,y=proc_use1[,15],name=colnames(proc_use1[15]), type = "bar")
  layout(p,barmode = "stack", xaxis = list(title = "Link ID"),yaxis = list(title = "Traffic Emission per Link[%]"))
  })
  #filtered data for plot 4 and 5
  tfstime1 <- reactive({ subset(tfstime, tfstime$Source_nam %in% c(input$checkGroup))
  })
  #plot4
  output$trendPlot4 <- renderPlotly({p<-plot_ly(x=tfstime1()[ ,2] ,y=tfstime1()[,3],group = tfstime1()$Source_nam)#, group = tfstime1$Source_nam)
  layout(title = "Flow/Speed",xaxis = list(title = "Hour"),yaxis = list(title = "Traffic Flow") )
  
  })
  #plot5
  output$trendPlot5 <- renderPlotly({p<-plot_ly(x=tfstime1()[,2] ,y=tfstime1()[,4], group = tfstime1()$Source_nam)
  layout(title = "Flow/Speed",xaxis = list(title = "Hour"),yaxis = list(title = "Speed")
  )
  })
  #end tab
  ############################################################
  
})