#https://app.codility.com/programmers/lessons/4-counting_elements/perm_check/

#binary function
binary<-function(p_number) {
  bsum<-0
  bexp<-1
  while (p_number > 0) {
    digit<-p_number %% 2
    p_number<-floor(p_number / 2)
    bsum<-bsum + digit * bexp
    bexp<-bexp * 10
  }
  return(bsum)
}

#codility lesson 1
binary_gap = function(n){
  l=as.integer( log2(n)+1)
  bin_representation =  intToBits(n)[1:l]
  max_gap = 0
  gap_counter = 0
  gap_started = FALSE
  
  for (i in bin_representation){
    if (i == 1){
      if (gap_counter > max_gap){
        max_gap = gap_counter
      }
      gap_counter = 0
      gap_started = TRUE
    }
    else if(gap_started == TRUE){ 
      gap_counter = gap_counter + 1
    }
  }
  print(max_gap)
}

#lesson 2 .1

cycrot = function(a,k){
  l = length(a)
  for (i in 1:l) {
    if((i+k)%%l==0){
      a2[l]=a[i]
    }
    else
      a2[(i+k)%%l]=a[i]
  }
  print(a2)
}

#lesson 2 .2
odo = function(a){
  x=a[1]
  for (i in 2:length(a)){ 
    if(a[i] %in% x){
    x=setdiff(x, a[i])
  }
  else
    x=union(x,a[i])
  }
  print(x)
}


#lesson 3 .3 differen
ad = function(a){
  z=sum(a)
  di=z
  prev=0
  for (i in 2:length(a)) {
    prev=prev+a[i-1]
    nt=z-prev
    di=min(abs(di),abs(prev-nt))
  }
  print(di)
}

#les4 ep

#If we know that all the elements are in the set {0, 1, . . . , m}, then the array used for counting should be of size m + 1.

countt=function(a,m){
  cnt=rep[0,length(m+1)]
  for (i in 1:length(a)){
    cnt[a[i]]=cnt[a[i]]+1
  }
  print(cnt)
}

swp=function(a,b,m){
  sa=sum(a)
  sb=sum(b)
  d=sb-sa
  if(d%%2=1){
    print(FALSE)
  }
  cnt=countt(a,m)
  for (i in length(a)) {
    if((b[i]-d>=0 )&(b[i]-d<=m)&(cnt[b[i]-d]>0)){
      print(TRUE)
    }
    print(FALSE)
  }
  
}
#4.1

countt=function(a,m){
  cnt=rep(0,m+1)
  for (i in 1:length(a)){
    cnt[a[i]]=cnt[a[i]]+1
  }
  return(cnt)
}

perm=function(a){
  b=countt(a,length(a))
  for (i in 1:length(a)) {
    if(b[i]<1){
      return(0)
    }
  }
  return(1)
}

#4.2 frog
fr=function(x,a){
  xx=rep(0,x)
  y=0
  for (i in 1:length(a)) {
    if(a[i]<=x){
      if(xx[a[i]]==0){
        xx[a[i]]=a[i]
        y=y+1
      }
    }
    if(y==x){
      return(i-1)
    }
  }
  return(-1)
}

#4.3
msint=function(a){
  b=rep(0,length(a))
  x=0
  for (i in 1:length(a)) {
    if((a[i]>0)&(a[i]<=length(a))){
      if(b[a[i]]==0){
        b[a[i]]=1
      }
    }
  }
  for (i in 1:length(a)) {
    if(b[i]==0){
      return(i)
    }
  }
  return(length(a)+1)
}

#5
#exp
pxsum <- function(a) {
  b=rep(0,length(a)+1)
  for (i in 2:length(a)+1) {
    b[i]=b[i-1]+a[i-1]
  }
  return(b)
}

cntpx <- function(a,x,y) {
  return(a[y+1]-a[x])
}


#5.1
pcar<-function(a){
  e=0
  w=0
  for (i in 1:length(a)) {
    if(a[i]==0){
      e=e+1
    }
    else{
      w=w+e
    }
  }
  return(w)
}

#5.3
# sum for each letter separetly
#5.4
#check al 2 and 3 elements set find smalest

#6.4
nlogn<-function(a){
  act=rep(0,length(a))
  dac=rep(0,length(a))
  for (i in 1:length(a)) {
    act[i]=i-1-a[i]
    dac[i]=i-1+a[i]
  }
  act=sort(act)
  dac=sort(dac)
  x=1 
  y=1
  z=0
  tot=0
  for (i in 1:length(a)) {
    while ((x<=length(a))&(act[x]<=dac[y])) {
      x=x+1
      z=z+1
    }
    z=z-1
    tot=tot+z
    y=y+1
  }
  return(tot)
}
nlogn(c(1,5,2,1,4,0))

#O(N) sol

din<-function(a){
  act=rep(0,length(a))
  dac=rep(0,length(a))
  n=length(a)
  for (i in 1:length(a)) {
    ll=i-a[i]
    if(ll<1){
      ll=1
    }
    act[ll]=act[ll]+1
    ul=i+a[i]
    if(ul>n){
      ul=n
    }
    dac[ul]=dac[ul]+1
  }

  x=0 
  z=0
  for (i in 1:n) {
    x=x+act[i]
    
    while ((x>0)&(dac[i]>0)) {
      z=z+x-1
      x=x-1
      dac[i]=dac[i]-1 
    }
       
  }
  return(z)
}
din(c(1,5,2,1,4,0))

#7
#stack
sstact=c()
sz=0
ppush <- function(x) {
  sstack[sz+1]=x
  sz=sz+1
}
ppop <- function(x) {
  if(sz>0){
    sz=sz-1
    sstact=sstact[1:sz]
    return(sstact[sz])
  }
  else
    return(NULL)
}
#que
queue =c()
qhead=0
qtail = 0
qpush <- function(x) {
  qtail = qtail + 1
  queue[qtail] = x
  sz=sz+1
}
qpop <- function(x) {
  if(qtail>0){
    qhead = qhead + 1
    sstact=sstact[2:qtail]
    return(queue[1])
  }
  else
    return(NULL)
}

#8 leader

lid<-function(a){
  z=a[1]
  zl=1
  for(i in 2:length(a)){
    if(zl==0){
      z=a[i]
    }
    if(zl>0){
      if(z==a[i]){
        zl=zl+1
      }
      else{
        zl=zl-1
      }
    }
     
  }
  zl=0
  for(i in 1:length(a)){
    if(z==a[i]){
      zl=zl+1
    }
  }
  if(zl>length(a)/2){
    return(z)
  }
}

#9

mss<-function(a){
  ms=a[1]
  me=a[1]
  for(i in 2:length(a)){
    me=max(0,me+a[i])
    ms=max(ms,me)
  }
  return(ms)
}




