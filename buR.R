#https://www.r-bloggers.com/free-ticket-to-erum/

#http://statistics.berkeley.edu/computing/r-reading-webpages

thepage <- readLines("budapeszt/01-events.Rmd.txt",encoding ="UTF-8")


mypattern = '## ([^<]*) {-} '
datalines = grep(mypattern,thepage[1:length(thepage)],value=TRUE)

getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern,datalines)

matches = mapply(getexpr,datalines,gg)
result = gsub(mypattern,'\\1',matches)
names(result) = NULL
result=data.frame(grep(mypattern,thepage[1:length(thepage)]), result)
names(result)<-c("nr","year")

yd=data.frame(year=integer(),
              pat=character())      

for (i in 2:dim(result)[1]){
  mypattern = '* ([^<]*):'
  datalines = grep(mypattern,thepage[result[i-1,1]:result[i,1]],value=TRUE)
  
  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
  gg = gregexpr(mypattern,datalines)
  
  matches = mapply(getexpr,datalines,gg)
  names(matches) = NULL
  yd2=data.frame(result[i-1,2],matches)
  yd=rbind(yd,yd2)
}

mypattern = '* ([^<]*): ([^<]*)'
datalines = grep(mypattern,thepage[1:length(thepage)],value=TRUE)

getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)
gg = gregexpr(mypattern,datalines)

matches = mapply(getexpr,datalines,gg)
result1 = gsub(mypattern,'\\2',matches)
names(result1) = NULL
result1[1:10]




matches[2]
