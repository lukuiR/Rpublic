prod1=read.csv("edzisia/produkty.csv",header=TRUE, sep=";")
opt=read.csv("edzisia/dieta.csv",header=TRUE, sep=";")

library(lpSolveAPI)

mylp<-make.lp(2,2)
set.column(mylp,1,c(221,80))
set.column(mylp,2,c(119,3))
set.constr.type(mylp,rep(">=",2))
set.rhs(mylp, c(2500,100))
set.objfn(mylp, c(301,122))
solve(mylp)
get.objective(mylp)
get.variables(mylp)
get.constraints(mylp)

dim(opt)[2]
for (i in 3:12) {
  prod1[,i] <- round( as.numeric( sub(",", ".", prod1[,i]) ),3)
}
 prod=prod1[1:28,]

mylp<-make.lp(dim(opt)[2],dim(prod)[1])
for(i in 1:dim(prod)[1]){
  set.column(mylp,i, prod[i,c(-1,-2)])
}
set.constr.type(mylp,rep(">=",dim(opt)[2]))
set.rhs(mylp, opt[1,1:dim(opt)[2]])
set.objfn(mylp,rowSums(  prod[,3:(dim(prod)[2])]))
set.type(mylp, 1:dim(prod)[1], "integer")
set.type(mylp, 2, "integer")

lp.control(mylp, timeout = 150)

#write.lp(mylp,'model.lp',type='lp')

solve(mylp)
get.objective(mylp)
aaa<-get.variables(mylp)
aaa
get.constraints(mylp)

xx=as.data.frame(aaa)


amydat <- fread(paste('http://aerisqualitas.org/coyote/csv/',Sys.Date(),'.csv',sep = ""))



library(data.table)
mydat <- fread('http://www.wik.pwr.wroc.pl/aeris/coyote/csv/2017-01-23.csv')
head(mydat,20)[,2]



rsconnect::deployApp('~/r/Rgraphics/tomm')

mydat$`deviceID:115 type:10 variable:4`

xx<- as.POSIXct(strptime(mydat$Time, "%Y-%m-%d %H:%M:%S"))
as.numeric(sub(",", ".", mydat$`deviceID:115 type:2 variable:4`, fixed = TRUE))

plot_ly(xx, as.numeric(sub(",", ".", mydat$`deviceID:115 type:2 variable:4`, fixed = TRUE)), xlab="Time", ylab="Random value")

plot_ly(x=xx, y=as.numeric(sub(",", ".", mydat$`deviceID:115 type:20 variable:4`, fixed = TRUE)) )


plot_ly(mydat, x=mydat$Time, y=mydat$`deviceID:115 type:20 variable:4`)


plot_ly(x=xx, y=as.numeric(sub(",", ".", mydat$`deviceID:115 type:20 variable:4`, fixed = TRUE)) )%>%
  
  layout(                        # all of layout's properties: /r/reference/#layout
    title = "Unemployment", # layout's title: /r/reference/#layout-title
    xaxis = list(           # layout's xaxis is a named list. List of valid keys: /r/reference/#layout-xaxis
      title = "Time",      # xaxis's title: /r/reference/#layout-xaxis-title
      showgrid = F),       # xaxis's showgrid: /r/reference/#layout-xaxis-showgrid
    yaxis = list(           # layout's yaxis is a named list. List of valid keys: /r/reference/#layout-yaxis
      title = "deviceID:115 type:20 variable:4")     # yaxis's title: /r/reference/#layout-yaxis-title
  )
