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