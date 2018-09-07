#https://www.r-bloggers.com/15-types-of-regression-you-should-know/
library(datasets)
model = lm(Fertility ~ .,data = swiss)
lm_coeff = model$coefficients
lm_coeff
summary(model)

data = read.csv("poly.csv")
x = data$Area 
y = data$Price

model1 = lm(y ~x) 
model1$fit
model1$coeff

new_x = cbind(x,x^2)

model2 = lm(y~new_x) 
model2$fit
model2$coeff

library(ggplot2) 
ggplot(data = data) + geom_point(aes(x = Area,y = Price)) +
  geom_line(aes(x = Area,y = model1$fit),color = "red") +
  geom_line(aes(x = Area,y = model2$fit),color = "blue") +
  theme(panel.background = element_blank())

################################x^3

new_x = cbind(x,x^2,x^3)

model2 = lm(y~new_x) 
model2$fit
model2$coeff

w <- x + x^2
fit <- lm(y ~ x)
model2 <- lm(y ~ x, weights = w)


ggplot(data = data) + geom_point(aes(x = Area,y = Price)) +
  geom_line(aes(x = Area,y = model1$fit),color = "red") +
  geom_line(aes(x = Area,y = model2$fit),color = "blue") +
  theme(panel.background = element_blank())

###########
