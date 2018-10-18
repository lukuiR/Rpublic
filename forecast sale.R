#https://www.theanalysisfactor.com/r/
#https://www.datascience.com/blog/introduction-to-forecasting-with-arima-in-r-learn-data-science-tutorials
#https://otexts.org/fpp2/what-can-be-forecast.html


library('forecast')
library('tseries')
library(tidyverse)
library(lubridate)


item_cat <-read.csv('~/R Skrypty/data/predi/item_categories.csv',header = TRUE,stringsAsFactors=FALSE)
item <-read.csv('~/R Skrypty/data/predi/items.csv',header = TRUE,stringsAsFactors=FALSE)
shops <-read.csv('~/R Skrypty/data/predi/shops.csv',header = TRUE,stringsAsFactors=FALSE)
sales_tra <-read.csv('~/R Skrypty/data/predi/sales_train.csv',header = TRUE,stringsAsFactors=FALSE)


test <-read.csv('~/R Skrypty/data/predi/test.csv',header = TRUE,stringsAsFactors=FALSE)
head(test)
ss <-read.csv('~/R Skrypty/data/predi/sample_submission.csv',header = TRUE,stringsAsFactors=FALSE)
head(ss)

sales_tra$Date = as.Date(sales_tra$date, "%d.%m.%Y")

ggplot(sales_tra, aes(Date, cnt_ma30)) + geom_line() + scale_x_date('month')  + ylab("Daily Bike Checkouts") +
  xlab("")

sales_tra$cnt_ma = ma(sales_tra$item_cnt_day, order=7) # using the clean count with no outliers
sales_tra$cnt_ma30 = ma(sales_tra$item_cnt_day, order=30)

count_ma = ts(na.omit(sales_tra$cnt_ma), frequency=30)
decomp = stl(count_ma, s.window="periodic")
deseasonal_cnt <- seasadj(decomp)
plot(decomp)

adf.test(count_ma, alternative = "stationary")


summary(sales_tra)

ag_dat1 <- group_by(sales_tra ,m=months(sales_tra$Date), item_id) %>% 
  summarise(
    n = sum(item_cnt_day)
  )%>%
  spread(key = m, value = n)



mi=sales_tra[sales_tra$item_cnt_day<0,]

head(mi)

sales_tra$yymm = year(sales_tra$Date)*100+month(sales_tra$Date)

ran <- NULL
ran$yymm <- unique(sales_tra$yymm)
ran$ra <- rank(ran$yymm)
sales_tra <- merge(sales_tra,ran)

ag_dat <- group_by(sales_tra, ra ,item_id ,shop_id) %>% 
  summarise(
    n = sum(item_cnt_day)
  )

ag_dat2=data.frame(ag_dat)

#regresion

#https://www.r-bloggers.com/15-types-of-regression-you-should-know/

model = lm(n ~ .,data = ag_dat)
lm_coeff = model$coefficients
lm_coeff
summary(model)

for (i in dim(test)[1]) {
  ag = ag_dat2%>%
    filter(item_id==test[i,3], shop_id==test[1,2])
  x=ag$ra
  y = ag$n
  }
i=1

model1 = lm(y ~x) 
model1$fit
model1$coeff

new_x = cbind(x,x^2)

model2 = lm(y~new_x) 
model2$fit
model2$coeff

w <- 1 + x^2+x^4
fit <- lm(y ~ x)
model2 <- lm(y ~ x^2, weights = w)

library(ggplot2) 
ggplot(data = ag) + geom_point(aes(x = ra,y = n)) +
  geom_line(aes(x = ra,y = model1$fit),color = "red") +
  geom_line(aes(x = ra,y = model2$fit),color = "blue") +
  theme(panel.background = element_blank())

################################x^3

new_x = cbind(x,x^2)
new_xx = cbind(xx,xx^2)


model2 = lm(y~new_x) 
model2$fit
model2$coeff


ggplot(data = data) + geom_point(aes(x = Area,y = Price)) +
  geom_line(aes(x = Area,y = model1$fit),color = "red") +
  geom_line(aes(x = Area,y = model2$fit),color = "blue") +
  theme(panel.background = element_blank())


###########
zz=0
for (i in 1:dim(test)[1]) {
  print(i)
  ag = ag_dat2%>%
    filter(item_id==test[i,3], shop_id==test[i,2])
  x=ag$ra
  y = ag$n
  if(length(y)>1 ){
    if(x[length(x)]>32){
    model1 = lm(y ~x) 
    new <- data.frame(x = seq(30, 35, 1))
    ss$it[i] <- predict(lm(y ~ x), new)[6]
  }
  }
  else{
    ss$it[i] <- 0
  }
}



ag = ag_dat2%>%
  filter(item_id==test[i,3], shop_id==test[1,2])
x=ag$ra
y = ag$n

model1 = lm(y ~x) 
model1$fit
model1$coeff

new <- data.frame(x = seq(30, 35, 1))
predict(lm(y ~ x), new)[6]

#xx=30:35
#xx2=xx^2
#z=c(xx,xx2)

Time <- x
Time2 <- Time^2
Time3 <- Time^3

quadratic.model <-lm(y ~ Time + Time2+ Time3)

#timevalues <- seq(30, 35, 1)

#predictedcounts <- predict(quadratic.model,list(Time=timevalues, Time2=timevalues^2, Time3=timevalues^3))

ggplot(data = ag) + geom_point(aes(x = ra,y = n)) +
  geom_line(aes(x = ra,y = model1$fit),color = "red") +
  geom_line(aes(x = ra,y = quadratic.model$fit),color = "blue") +
  theme(panel.background = element_blank())





zz=colnames(ss)
zz=c(zz[1],zz[3],zz[2])
colnames(ss)<-zz


ss2=ss
ss2$item_cnt_month=round(ss$item_cnt_month[20])

zz <- colnames(ss)
zz <- zz[c(1,3,2)]


ss$it <- NULL
write.csv(ss,file = 'ss2.csv', row.names = FALSE)
