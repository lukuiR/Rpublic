library(shiny)
library(shinydashboard)


library(rvest)

UrlPage <- html ("http://eyeonhousing.org/2012/11/gdp-growth-in-the-third-quarter-improved-but-still-slow/")
ImgNode <- UrlPage %>% html_node("img.wp-image-5984")
link <- html_attr(ImgNode, "src")

plot(0:1,0:1,type="n",ann=FALSE,axes=FALSE)
rasterImage(link,0,0,1,1)

library(leaflet)

leaflet(data = quakes[1:20,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))

greenLeafIcon <- makeIcon(
  iconUrl = link,#"http://leafletjs.com/examples/custom-icons/leaf-green.png",
  iconWidth = 90, iconHeight = 95,
  iconAnchorX = 90, iconAnchorY = 94
)

leaflet(data = quakes[1:4,]) %>% addTiles() %>%
  addMarkers(~long, ~lat, icon = greenLeafIcon)




thepage1 = readLines('https://en.wikipedia.org/wiki/Wrocław')

mypattern = 'href="([^<]*)"class="image"'
dataline = grep(mypattern,thepage1[1:length(thepage)],value=TRUE)
dataline[1]
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern,dataline)

matches = mapply(getexpr,dataline,gg)
result = gsub(mypattern,'\\1',matches)
names(result) = NULL
result=data.frame(grep(mypattern,thepage1[1:length(thepage1)]), result)
names(result)<-c("nr","country")
result[dim(result)[1]+1,1]=length(thepage1)


grep('image',thepage)
thepage[1]



##gilrs

thepage1 <- readLines("budapeszt/03-Rladies.Rmd.txt",encoding ="UTF-8")


mypattern = '## ([^<]*)'
dataline = grep(mypattern,thepage1[1:length(thepage1)],value=TRUE)
dataline[1]
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern,dataline)

matches = mapply(getexpr,dataline,gg)
result = gsub(mypattern,'\\1',matches)
names(result) = NULL
result=data.frame(grep(mypattern,thepage1[1:length(thepage1)]), result)
names(result)<-c("nr","country")
result[dim(result)[1]+1,1]=length(thepage1)

yd=data.frame(year=integer(),
              pat=character(),
              date=character())          

for (i in 2:dim(result)[1]){
  mypattern = '* ([^<]*):'
  
  
  datalines2 = grep(mypattern,thepage1[result[i-1,1]:result[i,1]],value=TRUE)
  
  
  names(datalines2) = NULL
  yd2=data.frame(result[i-1,2],datalines2)
  
  for (j in 1:length(datalines2)) {
    mypattern2 = "[*]([^<]*)*[:]([^<]*)*[[]([^<]*)*[]][(]([^<]*)*[)]([^<]*)*"
    
    datalines = grep(mypattern2,datalines2[j],value=TRUE)
    getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
    gg = gregexpr(mypattern2,datalines)
    matche = mapply(getexpr,datalines,gg) 
    result1 = gsub(mypattern2,'\\1',matche)
    if(length(result1)==0)
    {
      result1="NA"
    }
    result2 = gsub(mypattern2,'\\4',matche)
    if(length(result2)==0)
    {
      result2="NA"
    }
    result3 = gsub(mypattern2,'\\3',matche)
    if(length(result3)==0)
    {
      result3="NA"
    }
    result4 = gsub(mypattern2,'\\6',matche)
    if(length(result4)==0)
    {
      result4="NA"
    }
    
    yd2[j,3]= result1
    yd2[j,4]= result2
    yd2[j,5]= result3
    yd2[j,6]= result4
    
  }
  
  yd=rbind(yd,yd2)
}

###############
a=read.csv(file='budapeszt/states.csv',header = TRUE,sep = ";")
yd[,7]=sub('([^<]*), ',"",yd[,3])
colnames(yd)<-c("kraj","all","miasto","web","nazwa","city","id")
yd$city=sub(',([^<]*)',"",yd$miasto)
yd$id <- as.character(yd$id)
a$id <- as.character(a$id)

yd=merge(x=yd, y=a, by="id",all.x = TRUE)

for (i in 1:dim(yd)[1]){
  
  
  #thepage = readLines(paste('https://en.wikipedia.org/wiki/',yd[i,3],sep=""))
  
  thepage = try(readLines(paste('https://en.wikipedia.org/wiki/',yd$miasto[i],sep="")))
  thepage=thepage[grep('og:image',thepage)[1]]
  mypattern = '"https://([^<]*)"/>'
  dataline = grep(mypattern,thepage,value=TRUE)
  dataline[1]
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,dataline)
  
  matches = mapply(getexpr,dataline,gg)
  result = gsub(mypattern,'\\1',matches)
  names(result) = NULL
  
  if(length(result)==0){
    
    thepage = try(readLines(paste('https://en.wikipedia.org/wiki/',yd$city[i],',_',yd$kraj[i],sep="")))

    thepage=thepage[grep('og:image',thepage)[1]]
    mypattern = '"https://([^<]*)"/>'
    dataline = grep(mypattern,thepage,value=TRUE)
    dataline[1]
    getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
    gg = gregexpr(mypattern,dataline)
    
    matches = mapply(getexpr,dataline,gg)
    result = gsub(mypattern,'\\1',matches)
    names(result) = NULL
    
    if(length(result)==0){
      
      thepage = try(readLines(paste('https://en.wikipedia.org/wiki/',yd$city[i],',_',yd$state[i],sep="")))
      
      thepage=thepage[grep('og:image',thepage)[1]]
      mypattern = '"https://([^<]*)"/>'
      dataline = grep(mypattern,thepage,value=TRUE)
      dataline[1]
      getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
      gg = gregexpr(mypattern,dataline)
      
      matches = mapply(getexpr,dataline,gg)
      result = gsub(mypattern,'\\1',matches)
      names(result) = NULL
    }
  }
  print(paste('https://',result, sep=""))
  yd[i,9]=paste('https://',result, sep="")
}



thepage = readLines(paste('https://en.wikipedia.org/wiki/',yd[i,3]))
thepage[grep('image',thepage)[2]]
