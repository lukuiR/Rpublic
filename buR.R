#https://www.r-bloggers.com/free-ticket-to-erum/

#http://statistics.berkeley.edu/computing/r-reading-webpages

thepage <- readLines("budapeszt/01-events.Rmd.txt",encoding ="UTF-8")


mypattern4 = '* ([^<]*): ([^<]*)'
dataline4 = grep(mypattern,thepage[1:length(thepage)],value=TRUE)
dataline4[1]
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern,datalines)

matches = mapply(getexpr,datalines,gg)
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

substring(mypattern2,datalines,datalines+attr(datalines,'match.length')-1)

'* ([^<]*): ([^<]*)'
mypattern = '*([^<]*) ([^<]*): ([^<]*)'
datalines = grep(mypattern,yd[1:dim(yd)[1],2],value=TRUE)

getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern,datalines)

matches = mapply(getexpr,datalines,gg)
result1 = gsub(mypattern,'\\2',matche)
names(result1) = NULL
result1[1:15]

yd=data.frame(yd,result1)

mypattern2 = "*[*]([^<]*)*[:]{1}[ ]?[[]*{1}([^<]*)*[]]{1}[(]{1}([^<]*)*[)]{1}[.]{1}([^<]*)*[,]{1}([^<]*)*[.]{1}([^<]*)*[.]{1}([^<]*)*"
datalines = dataline4[1] #grep(mypattern2,datalines2[j],value=TRUE)
getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern2,datalines)
matche = mapply(getexpr,datalines,gg) 
result1 = gsub(mypattern2,'\\5',matche)
matche
result1

