#meetup info
me=1
orgn='a'
desc='a'
i=17
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
yd=data.frame(yd,me,orgn)
