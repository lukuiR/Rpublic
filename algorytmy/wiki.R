#https://github.com/mkearney/wiki_data

library('data.table')

untar('https://dumps.wikimedia.org/other/pageviews/2018/2018-12/pageviews-20181231-230000.gz')


t<-read.table(gzfile(fread(paste("https://dumps.wikimedia.org/other/pageviews/2019/2019-01/pageviews-20190128-230000.gz"))))[1]


file_name<-"pageviews-20190124-120000.gz"


cpl<-read.table(gzfile("clickstream-plwiki-2018-12.tsv.gz"), fill = TRUE)

cen<-read.table(gzfile("clickstream-enwiki-2018-12.tsv.gz"), fill = TRUE)

source("read_wiki_data.R")

pwe<-read.table(gzfile(file_name), fill = TRUE)

x <- wiki_data(file_name, skip = 100, n_max = 100)

unique( pwe$V1[pwe$V1 %like% 'app'])

i=0
j=0
while (i==0) {
  x <- wiki_data(file_name, skip = j, n_max = 1)
  if(x[1]!="pl"){
    i=1
  }
  j=100000+j
}


x <- wiki_data(file_name, skip = j-50000, n_max = 1)

byDayTime[order(-byDayTime$count),][1:100,]


list1 <- list.files("wik")[1:2]

list1 <- list.files("wik")[c(-1,-2,-3)]
setwd("C:/Users/Luke/Rpublic/")

list1 <- list.files("dat")

# prev: the result of mapping the referer URL to the fixed set of values described above
# curr: the title of the article the client requested
# type: describes (prev, curr)
# - link: if the referer and request are both articles and the referer links to the request
# - external: if the referer host is not en(.m)?.wikipedia.org
# - other: if the referer and request are both articles but the referer does not link to the request. 
#   This can happen when clients search or spoof their refer.
# n: the number of occurrences of the (referer, resource) pair

cpl<-read.table(gzfile("clickstream-plwiki-2018-12.tsv.gz"), fill = TRUE)

cen<-read.table(gzfile("clickstream-enwiki-2018-12.tsv.gz"), fill = TRUE)

raw<-read.table(gzfile(file_name), fill = TRUE)

raw=0
plpv=0
enpv=0
enpv1 <- data.frame(enpv)

file_name<-"pageviews-20190128-230000.gz"
file_name<-"pageviews-20190124-120000.gz"
setwd("C:/Users/Luke/Rpublic/wik")
setwd("C:/Users/Luke/Rpublic/wik/dat")
raw <- read.table(gzfile(list1[1]), fill = TRUE)

#lines:
# wiki code (subproject.project)
# article title
# requests
# response bytes

for (i in c(19:24)) {
  raw <- read.table(gzfile(list1[i]), fill = T)
  raw$h <- i-1
  plpv <-rbind(plpv,raw[raw$V1==c("pl","pl.m"),])
  enpv <-rbind(enpv,raw[raw$V1==c("en","en.m"),])
  print(i)
}

raw <- read.table(gzfile(i), fill=T, sep=c("\t"))
i=list1[2]

enpv1$V3 <- as.numeric(as.character(enpv1$V3))
enpv1$V3[is.na(enpv1$V3)] <- 0

enpv1[order(-enpv1$V3),][1:700,]
zz=aggregate(enpv1$V3, by=list(enpv1$V1,enpv1$V2),FUN = "sum")

enpv=0
raw=0
##################
raw$h=12

plpv <-raw[raw$V1==c("pl","pl.m"),]
enpv <-raw[raw$V1==c("en","en.m"),]
#data cleaning
enpv$V3 <- as.numeric(as.character(enpv$V3))
enpv$V3[is.na(enpv$V3)] <- 0

plpv$V3 <- as.numeric(as.character(plpv$V3))
plpv$V3[is.na(plpv$V3)] <- 0
#q1
summary(plpv)
summary(enpv)
#q2
agpl <- aggregate(plpv$V3, by=list(plpv$V2),FUN = "sum")

stopl3 <- as.character(agpl[order(-agpl$x),][1:250,1])

agen <- aggregate(enpv$V3, by=list(enpv$V2),FUN = "sum")
stoen3 <- as.character(agen[order(-agen$x),][1:250,1])

seten4 <- enpv[enpv$V2 %in% sto,]
setpl4 <- plpv[plpv$V2 %in% stop,]

#q3
aghen4=aggregate(enpv$V3, by=list(enpv$h),FUN = "sum")
aghpl4=aggregate(plpv$V3, by=list(plpv$h),FUN = "sum")
aghen4<-aghen4[-1,]
aghpl4<-aghpl4[-1,]

agpl=aggregate(plpv$V3, by=list(plpv$h, plpv$V2),FUN = "sum")
agen=aggregate(enpv$V3, by=list(enpv$h, enpv$V2),FUN = "sum")

#q4
#W dokumentacji odnalazĹ‚em, ĹĽe moĹĽemy wyciÄ…gnÄ…c informacje tylko dla standardowego kanaĹ‚u i molinego.
#Dane dla aplikacji moĹĽna wyciÄ…gnÄ…Ä‡ koĹĽystajÄ…c z API. Ale nie znajduje siÄ™ to w podanym ĹşrĂłdle danych.
aghen=aggregate(enpv$V3, by=list(enpv$V1),FUN = "sum")
aghpl=aggregate(plpv$V3, by=list(plpv$V1),FUN = "sum")

agpl=aggregate(plpv$V3, by=list(plpv$V1, plpv$V2),FUN = "sum")
agen=aggregate(enpv$V3, by=list(enpv$V1, enpv$V2),FUN = "sum")
#q5
#KaĹĽda godzina to okoĹ‚o 7 mln rekordow danych, z czego potrzebujemy okoĹ‚o 1,5 mln rekordĂłw
#Gdzie tak naprawde potrzebujemy tylko zagregowane informacje.
#MiaĹ‚em problem z komputerem, ĹĽeby przetworzyÄ‡ wszystkie dane na raz.
#Dodatkowo pojawiĹ‚y siÄ™ bĹ‚edy w danych plus problem z codowaniem i polskimi znakami.
#Nie wpĹ‚ywa to na ostateczne wyniki, wiÄ™c nie traciĹ‚em czasu na poprawianie tego.
#Ale moĹĽna by to w przyszĹ‚oĹ›ci zrobiÄ‡ lepiej, np.ostawiÄ‡ bazÄ™ danych.

#c1
cplt<-read.table(gzfile("clickstream-plwiki-2018-12.tsv.gz"), fill = TRUE, encoding = 'UTF-8')

cen<-read.table(gzfile("clickstream-enwiki-2018-12.tsv.gz"), fill = TRUE)

cpl$V4 <- as.numeric(as.character(cpl$V4))
cpl$V4[is.na(cpl$V4)] <- 0

cen$V4 <- as.numeric(as.character(cen$V4))
cen$V4[is.na(cen$V4)] <- 0

summary(cen)
agcen=aggregate(cen$V4, by=list(cen$V2),FUN = "sum")
agcen[order(-agcen$x),][1:100,]
#100th en = 2018_in_film 885430 1st pl = RMS_Titanic 280000

#c3
set <- cpl[cpl$V2 %in% agcpl[order(-agcpl$x),][1:100,1],]
agrpl <- aggregate(set$V4, by=list(set$V1),FUN = "sum")
write.csv(agrpl[order(-agrpl$x),][1:100,], "agrpl.csv")

set <- cen[cen$V2 %in% agcen[order(-agcen$x),][1:100,1],]
agren <- aggregate(set$V4, by=list(set$V1),FUN = "sum")
write.csv(agren[order(-agren$x),][1:100,], "agren.csv")



########

enpv$V3 <- as.numeric(as.character(enpv$V3))
enpv$V3[is.na(enpv$V3)] <- 0

plpv$V3 <- as.numeric(as.character(plpv$V3))
plpv$V3[is.na(plpv$V3)] <- 0

colnames(stoen1) <- "v1"

sto <- unique(union( union(union(stoen1, stoen2), stoen3), stoen4))
stop <- unique(union( union(union(stopl1, stopl2), stopl3), stopl4))

aghen5<-rbind(aghen,aghen2,aghen3,aghen4)
aghpl5<-rbind(aghpl,aghpl2,aghpl3,aghpl4)

seten5 <- rbind(seten1,seten2,seten3,seten4)
setpl5 <- rbind(setpl1,setpl2,setpl3,setpl4)

seten5$V1 <- as.character(seten5$V1)
seten5$V4 <- as.numeric(as.character(seten5$V4))

setpl5$V2 <- as.character(setpl5$V2)

agpl <- aggregate(setpl5$V3, by=list(setpl5$V2),FUN = "sum")
plsto <- agpl[order(-agpl$x),][1:100,]
agen <- aggregate(seten5$V3, by=list(seten5$V2),FUN = "sum")
ensto <- agen[order(-agen$x),][1:100,]
#data cleaning
enpv$V3 <- as.numeric(as.character(enpv$V3))
enpv$V3[is.na(enpv$V3)] <- 0

plpv$V3 <- as.numeric(as.character(plpv$V3))
plpv$V3[is.na(plpv$V3)] <- 0
agpl <- aggregate(plpv$V3, by=list(plpv$V2),FUN = "sum")
stopl <- agpl[order(-agpl$x),][1:100,]

Zad3.

spl<- plpv[plpv$V2 %in% stopl$Group.1,]
sen<- enpv[enpv$V2 %in% ensto$Group.1,]

aghpl <- aggregate(spl$V3, by=list(spl$h),FUN = "sum")

aghen <- aggregate(sen$V3, by=list(sen$h),FUN = "sum")

aghpl<-as.data.frame(aghpl)


