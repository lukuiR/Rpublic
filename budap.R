#1 girl            250 useR           510 conf
#girls info
thepage1 <- readLines("budapeszt/03-Rladies.Rmd.txt",encoding ="UTF-8")

mypattern = '## ([^<]*)'
dataline = grep(mypattern,thepage1[1:length(thepage1)],value=TRUE)
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


yd[yd$V4=='https://www.meetup.com/R-Ladies-Brussels/members/',4]='https://www.meetup.com/R-Ladies-Brussels/'


#meetup info
me=1
orgn='a'
desc='a'

for (i in 1:dim(yd)[1]){
  
  thepage = try(readLines(yd$V4[i],encoding ="UTF-8"))
  mypattern='<a href=([^<]*) class="groupHomeHeader-memberLink padding--left display--inlineBlock atLarge_padding--none"><p class="text--hint text--tiny"><span>([^<]*)</span></p><p class="text--bold"><span>([^<]*)</span>'
  dataline = grep(mypattern,thepage,value=TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,dataline)
  
  matches = mapply(getexpr,dataline,gg)
  result = gsub(mypattern,'\\3',matches)
  names(result) = NULL
  if(length(result)==0){
    result=NA
  }
  me[i]=try(result)
  
  mypattern='<a href="([^<]*)" class="orgInfo-name"><p class="text--hint text--tiny"><span>([^<]*)</span></p><span class="text--bold text--small">([^<]*)</span>'
  dataline = grep(mypattern,thepage,value=TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,dataline)
  
  matches = mapply(getexpr,dataline,gg)
  result = gsub(mypattern,'\\3',matches)
  names(result) = NULL
  if(length(result)==0){
    result=NA
  }
  orgn[i]=try(result)
  
  
  
  
}

yd$me<-NULL
yd$orgn<-NULL
colnames(yd)<-c("kraj","all","miasto","web","nazwa","twiter")
yd=data.frame(yd,me,orgn)

#wiki image

a=read.csv(file='budapeszt/states.csv',header = TRUE,sep = ";")
yd$id=sub('([^<]*), ',"",yd$miasto)

yd$city=sub(',([^<]*)',"",yd$miasto)
yd$id <- as.character(yd$id)
a$id <- as.character(a$id)

yd=merge(x=yd, y=a, by="id",all.x = TRUE)
yd$wiki=NA

for (i in 1:dim(yd)[1]){
  
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
  yd$wiki[i]=paste('https://',result, sep="")
}
#logo


#geo info
ad<-yd$city
ad<-paste(ad,',',yd$kraj)
z=data.frame()
for(i in 1:length(ad)){
  z=rbind(z, nominatim_osm(ad[i]))
  
}
yd=data.frame( yd,z)

##logo
kk='a'

for (i in 1:dim(yd)[1]){
  
  #thepage = readLines(paste('https://en.wikipedia.org/wiki/',yd[i,3],sep=""))
  
  thepage = try(readLines(y$web[i]))
  thepage=thepage[grep('image',thepage)[1]]
  mypattern = 'https:([^<]*)jpeg'
  dataline = grep(mypattern,thepage,value=TRUE)
  dataline[2]
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,dataline)
  
  matches = mapply(getexpr,dataline,gg)
  result = gsub(mypattern,'\\1',matches)[1]
  names(result) = NULL
  kk[i]=paste('https:',result,'jpeg', sep="")
  if(is.na(result)){
    mypattern = 'https:([^<]*)png'
    dataline = grep(mypattern,thepage,value=TRUE)
    dataline[1]
    getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
    gg = gregexpr(mypattern,dataline)
    
    matches = mapply(getexpr,dataline,gg)
    result = gsub(mypattern,'\\1',matches)[1]
    names(result) = NULL
    kk[i]=paste('https:',result,'png', sep="")
    
    if(is.na(result)){
      thepage = try(readLines(y$web[i]))
      mypattern = '([^<]*)profile_image([^<]*)'
      dataline = grep(mypattern,thepage,value=TRUE)
      mypattern = '"https:([^<]*)g\"'
      getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
      gg = gregexpr(mypattern,dataline)
      
      matches = mapply(getexpr,dataline,gg)
      result = gsub(mypattern,'\\1',matches)[1]
      names(result) = NULL
      kk[i]=paste('https:',result,'g', sep="")
    }
  }
  
}

y$logo=kk
##
library(leaflet)

leaflet(data = y) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup=paste0('<BIG>',y$nazwa,'</BIG><br/>Members: ',y$me,'<br/>Organizes: ',y$orgn,"<br/><img src = ", y$logo, " Width = 150><br/> <SMALL>source: wikipedia</SMALL> "))







#r useR info
list<-list.files(path="budapeszt/" ,pattern = "\\.txt$")[2:7]

yd=data.frame(year=integer(),
              pat=character(),
              date=character())
for(v in 1:6){
thepage1 <- readLines(paste("budapeszt/",list[v], sep=""), encoding ="UTF-8")


mypattern = '### ([^<]*)'
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

          

for (i in 2:dim(result)[1]){
  mypattern = '* ([^<]*):'
  
  
  datalines2 = grep(mypattern,thepage1[result[i-1,1]:result[i,1]],value=TRUE)
  
  
  names(datalines2) = NULL
  if(length(datalines2)>1){
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
  
  yd=rbind(yd,yd2)}
}
}
yd[,6]=sub(' ([^<]*)',"",yd[,1])
yd[,1]=sub('([^<]*) ?[{-}]',"",yd[,1])
yd[81:96,1]='UK'
yd[114:177,1]='USA'

#meetup info
me=1
orgn='a'
desc='a'

for (i in 1:dim(yd)[1]){
  
  thepage = try(readLines(yd$V4[i],encoding ="UTF-8"))
  mypattern='<a href=([^<]*) class="groupHomeHeader-memberLink padding--left display--inlineBlock atLarge_padding--none"><p class="text--hint text--tiny"><span>([^<]*)</span></p><p class="text--bold"><span>([^<]*)</span>'
  dataline = grep(mypattern,thepage,value=TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,dataline)
  
  matches = mapply(getexpr,dataline,gg)
  result = gsub(mypattern,'\\3',matches)
  names(result) = NULL
  if(length(result)==0){
    result=NA
  }
  me[i]=try(result)
  
  mypattern='<a href="([^<]*)" class="orgInfo-name"><p class="text--hint text--tiny"><span>([^<]*)</span></p><span class="text--bold text--small">([^<]*)</span>'
  dataline = grep(mypattern,thepage,value=TRUE)
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,dataline)
  
  matches = mapply(getexpr,dataline,gg)
  result = gsub(mypattern,'\\3',matches)
  names(result) = NULL
  if(length(result)==0){
    result=NA
  }
  orgn[i]=try(result)
  
  
  
  
}

yd$members<-NULL
yd$orgnizer<-NULL

yd=data.frame(yd,me,orgn)
colnames(yd)<-c("kraj","all","miasto","web","nazwa","id1","members","organizer")

#wiki image

a=read.csv(file='budapeszt/states.csv',header = TRUE,sep = ";")
yd$id=sub('([^<]*), ',"",yd$miasto)

yd$city=sub(',([^<]*)',"",yd$miasto)
yd$id <- as.character(yd$id)
a$id <- as.character(a$id)

yd=merge(x=yd, y=a, by="id",all.x = TRUE)
yd$wiki=NA

for (i in 1:dim(yd)[1]){
  
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
  yd$wiki[i]=paste('https://',result, sep="")
}


#geo info
ad<-yd$city
ad<-paste(ad,',',yd$kraj)
z=data.frame()
for(i in 1:length(ad)){

  x=nominatim_osm(ad[i])  
if(length(x)>1){
  z[i,1]=x[1]
  z[i,2]=x[2]
}

}
yd=data.frame( yd,z)

y=read.csv("usex.csv", header = TRUE, sep=",")
y=y[,3:16]
y$id= as.character(y$id)
y$web= as.character(y$web)
y$nazwa= as.character(y$nazwa)
y$city= as.character(y$city)
y$wiki= as.character(y$wiki)
##logo
kk='a'

for (i in 1:dim(yd)[1]){
  print(i)
    thepage = try(readLines(as.character( yd$web[i])))
  thepage=thepage[grep('image',thepage)[1]]
  mypattern = 'https:([^<]*)jpeg'
  dataline = grep(mypattern,thepage,value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,dataline)
  
  matches = mapply(getexpr,dataline,gg)
  result = gsub(mypattern,'\\1',matches)[1]
  names(result) = NULL
  kk[i]=paste('https:',result,'jpeg', sep="")
  if(is.na(result)){
    mypattern = 'https:([^<]*)png'
    dataline = grep(mypattern,thepage,value=TRUE)
    dataline[1]
    getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
    gg = gregexpr(mypattern,dataline)
    
    matches = mapply(getexpr,dataline,gg)
    result = gsub(mypattern,'\\1',matches)[1]
    names(result) = NULL
    kk[i]=paste('https:',result,'png', sep="")
    
    if(is.na(result)){
      thepage = try(readLines(as.character( y$web[i])))
      mypattern = '([^<]*)profile_image([^<]*)'
      dataline = grep(mypattern,thepage,value=TRUE)
      mypattern = '"https:([^<]*)g\"'
      getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
      gg = gregexpr(mypattern,dataline)
      
      matches = mapply(getexpr,dataline,gg)
      result = gsub(mypattern,'\\1',matches)[1]
      names(result) = NULL
      kk[i]=paste('https:',result,'g', sep="")
    }
  }
  
}

yd$logo=kk
##

write.csv(yd,file="user.csv")

library(leaflet)

leaflet(data = yd) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup=paste0('<BIG>',yd$nazwa,'</BIG><br/>Members: ',yd$me,'<br/>Organizes: ',yd$orgn,"<br/><img src = ", yd$logo, " Width = 150><br/> <SMALL>source: wikipedia</SMALL> "))

###################3 conf

thepage <- readLines("budapeszt/01-events.Rmd.txt",encoding ="UTF-8")


mypattern = '## ([^<]*) {-} '
dataline = grep(mypattern,thepage[1:length(thepage)],value=TRUE)
dataline[1]
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern,dataline)

matches = mapply(getexpr,dataline,gg)
result = gsub(mypattern,'\\1',matches)
names(result) = NULL
result=data.frame(grep(mypattern,thepage[1:length(thepage)]), result)
names(result)<-c("nr","year")
result[dim(result)[1]+1,1]=length(thepage)

yd=data.frame(year=integer(),
              pat=character(),
              date=character())      


for (i in 2:dim(result)[1]){
  mypattern = '* ([^<]*):'
  
  
  datalines2 = grep(mypattern,thepage[result[i-1,1]:result[i,1]],value=TRUE)
  
  
  names(datalines2) = NULL
  yd2=data.frame(result[i-1,2],datalines2)
  
  for (j in 1:length(datalines2)) {
    mypattern2 = "[*]([^<]*)*[:]([^<]*)*[[]([^<]*)*[]][(]([^<]*)*[)]([^<]*)*[,]([^<]*)*[.]([^<]*)*"
    
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
    result4 = gsub(mypattern2,'\\5',matche)
    if(length(result4)==0)
    {
      result4="NA"
    }
    result5 = gsub(mypattern2,'\\6',matche)
    if(length(result5)==0)
    {
      result5="NA"
    }
    result6 = gsub(mypattern2,'\\7',matche)
    if(length(result6)==0)
    {
      result6="NA"
    }
    yd2[j,3]= result1
    yd2[j,4]= result2
    yd2[j,5]= result3
    yd2[j,6]= result4
    yd2[j,7]= result5
    yd2[j,8]= result6
    
  }
  
  yd=rbind(yd,yd2)
}

colnames(yd)<-c("year","all","date","web","nazwa","miasto","kraj","twitter")


yd$miasto= sub('[.]',"",yd$miasto)
yd$id=sub('([^<]*), ',"",yd$kraj)
yd$city=sub(',([^<]*)',"",yd$kraj)

yd$id <- as.character(yd$id)
a$id <- as.character(a$id)

yd=merge(x=yd, y=a, by="id",all.x = TRUE)
yd$wiki=NA

for (i in 1:dim(yd)[1]){
  
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
    
    thepage = try(readLines(paste('https://en.wikipedia.org/wiki/',yd$miasto[i],',_',yd$kraj[i],sep="")))
    
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
      
      thepage = try(readLines(paste('https://en.wikipedia.org/wiki/',yd$city[i],',_',yd$id[i],sep="")))
      
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
  }
  yd$wiki[i]=paste('https://',result, sep="")
}


#geo info
ad<-yd$miasto
ad<-paste(ad,',',yd$kraj)
z=data.frame()
for(i in 1:length(ad)){
  
  x=nominatim_osm(ad[i])  
  if(length(x)>1){
    z[i,1]=x[1]
    z[i,2]=x[2]
  }
  
}
yd=data.frame( yd,z)

write.csv(yd, file="conf.csv")

library(leaflet)

leaflet(data = yd) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup=paste0('<BIG>',yd$nazwa,'</BIG><br/>Web: ',yd$web,'<br/>Date: ',yd$date,'<br/>Year: ',yd$year,"<br/><img src = ", yd$wiki, " Width = 150><br/> <SMALL>source: wikipedia</SMALL> "))

