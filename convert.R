temp = list.files(pattern="*.csv")
myfiles = lapply(temp, read.delim)

for (i in 1:length(temp)) assign(paste('ne' ,i), read.csv(temp[i],header=FALSE, sep=","))

for (i in 1:length(temp)) write.csv(read.delim(temp[i],header=FALSE, sep=","),row.names = FALSE,
                                    file=paste("tamenew",i,".csv", sep=""))

name=c(c(1:23660),c(1:23660))
n<-matrix(name,ncol=2)
write.csv(n, row.names = FALSE, col.names = FALSE, file="nmt_name_list.csv")


kr=read.csv("Thames_screenline_addiotional_points.csv",header=TRUE, sep=",")
coordinates(kr) <- c("Easting", "Northing")
proj4string(kr) <- CRS("+init=epsg:4326")
CRSargs(CRS(proj4string(kr)))
CRS.new <- CRS("+init=epsg:27700")
pt <- spTransform(kr, CRS.new)
krr<-cbind(pt$Easting,pt$Northing,kr[,3:8])
write.csv(pt,file = "Thames_screenline_addiotional_points2.csv")

temp = list.files(pattern="*.xlsx",full.names=T)

for (i in 1:length(temp)) {a<-read.xlsx(temp[i],sheet = 4, rows = 3:18, cols =1:12)
                        b[i,1:5]<-c(a[2,3],as.numeric(a[9,2]),as.numeric(a[10,2]),a[1,5],a[2,5])}
write.csv(datxlxs,file = "MCC all.csv")


coordinates(b) <- c("y", "x")
proj4string(d4) <- CRS("+init=epsg:4326")
CRSargs(CRS(proj4string(d4)))
CRS.new <- CRS("+init=epsg:27700")
pt <- spTransform(d4, CRS.new)
c<-data.frame(b)
b[,2]<-as.numeric(b[,2])
c[,3]<-as.numeric(c[,3])
meuse<-c
d5<-data.frame(pt$d,pt$d2,d,d2,b)
coordinates(d4) <- c("d", "d2")
d3=cbind(d,d2)


ble<-read.csv("Raw-count-data-minor-roads.csv",header=TRUE, sep=",")

wb = XLConnect::loadWorkbook(temp[50])
sh = XLConnect::getSheets(wb)
sh1=sh[grep("MCC",sh)]
df = readWorksheet(wb, sheet = sh[grep("MCC",sh)], header = FALSE)

wb = XLConnect::loadWorkbook(temp[1])
 sh = XLConnect::getSheets(wb)
 sh1=sh[grep("MCC",sh)]

 df = readWorksheet(wb, sheet = sh1, header = FALSE)
 datxlxs=rbind(df)

for (i in 2:length(temp)) {print(i)  
  wb = XLConnect::loadWorkbook(temp[i])
      sh = XLConnect::getSheets(wb)
      sh1=sh[grep("MCC",sh)]
      for (j in length(sh1)) {
        df = readWorksheet(wb, sheet = sh1[j], endCol = 40, header = FALSE)
        datxlxs=rbind(datxlxs,df)
        
      }
}
  
for (aa in 1:12) {print(i)
  
}
  
  
  a<-read.xlsx(temp[1],sheet = 3, rows = 4:18, cols =1:15)
b[i,1:5]<-c(a[2,3],as.numeric(a[9,2]),as.numeric(a[10,2]),a[1,5],a[2,5])

#temp[i] c4,5,6 a8,d8,g8,j8,m8


temp = list.files(pattern="*.xlsx",full.names=T)

for (i in 1:length(temp)) {a1<-read.xlsx(temp[i],sheet = 3, rows = 7:8, cols =1:4)
b[i,1]<-temp[i]
b[i,2]<-as.numeric(a1[1])
b[i,3]<-as.numeric(a1[2])}
#,as.numeric(a[9,2]),as.numeric(a[10,2]),a[1,5],a[2,5])}



options(java.parameters = "-Xmx4g" )
library(XLConnect)
write.csv(pt,file = "ATC2.csv")
#read atc
for (i in 1:5){
     #length(temp)) {
wb = XLConnect::loadWorkbook(temp[i])
sh = XLConnect::getSheets(wb)
sh1=sh[grep("Site",sh)]

df = XLConnect::readWorksheet(wb, sheet = sh1,endCol = 15, endRow = 8, header = FALSE)
tabl[i,]=c(temp[i],df[4,3],df[5,3],df[6,3],df[8,1],df[8,4],df[8,7],df[8,10],df[8,13])
}
options(java.parameters = "-Xmx4g" )

colnames(tabl)=c("file","rd","dirA","dirB","x","y","Stdate","endDate","PSL110%")

xx=as.numeric(b[,2])
yy=as.numeric(b[,3])
atc=data.frame(xx,yy,b)

coordinates(atc) <- c("yy", "xx")
proj4string(atc) <- CRS("+init=epsg:4326")
#CRSargs(CRS(proj4string(atc)))
CRS.new <- CRS("+init=epsg:27700")
pt <- spTransform(atc, CRS.new)


package = "openxlsx" # no java memory problem