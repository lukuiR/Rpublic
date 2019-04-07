#https://cran.r-project.org/web/packages/OneR/vignettes/OneR.html

library(tidyverse)
library(readxl)
library(lubridate)

#read data
db <- read_excel("crif/zadanie_20171117.xlsx",1,col_names = TRUE)
str(db)

summary(df)

#remove bad class
db <- db[db$performance!='UNKNOWN',]

#Deal with na value
db$ip_country[db$ip_country=='-']='na'

#unify data
db$ip_country[db$ip_country=='pl']='PL'
db$ip_country[db$ip_country=='Pl']='PL'
#data format
db$application_date=ymd(db$application_date)
db=db1
db$mth=month(db$application_date)
db=db[,c(-6,-7)]
db$ratio=db$loan_amount/db$earnings

df <- db %>%
  mutate_if(is.character, as.factor) %>%
  select(performance, everything())

  #mutate(performance = factor(performance, levels = c("GOOD", "BAD")))



library(OneR)

library(plotly)

plot_ly(db, x=~db$age, color=~db$performance, mode='bar')

plot_ly(db, x=~db$earnings, color=~db$performance, mode='bar')

plot_ly(db, x=~db$loan_amount, color=~db$performance, mode='bar')

plot_ly(db, x=~db$pl_state, color=~db$performance, mode='bar')

plot_ly(db, x=~db$channel, color=~db$performance, mode='bar')

plot_ly(db, x=~db$ip_country, color=~db$performance, mode='bar')#random

plot_ly(db, x=~as.Date(db$application_date), color=~db$performance, mode='bar')

plot_ly(db, x=~db$gender, color=~db$performance, mode='bar')

db$pl_state[db$pl_state=='?']= 'Mazowieckie'

df=df[,-7]

#bining
bdb <-  optbin(performance ~., data =df)

model <- OneR(performance ~., data =df[,-2], verbose = TRUE)

#plotly
model <- OneR(data, verbose = TRUE)



library("DALEX")

# create a linear model
lm_model <- lm(as.numeric(as.factor( df$performance)) ~ age +   loan_amount  , data = df)
summary(lm_model)

# create a random forest model
library("randomForest")

set.seed(3) 

rf_model <- randomForest(performance ~ age + ratio+mth+  earnings + gender +   pl_state +channel +  loan_amount, data = df)

rf_model

# 1. To use DALEX you need an explainer

explainer_lm <- explain(lm_model,
                        data = db[1:200,-9], y = db$performance)
explainer_lm

explainer_rf <- explain(rf_model,
                        data = df, y = df$performance)

xx=df

explainer_rf

explainer_rf  <- explain(rf_model, data = data, y = data$performance)
model_performance(explainer_rf)

mp_rf <- model_performance(explainer_rf)
mp_rf


#
# Model explainers - Continuous variable response
#
# Variable effect
## for construction.year

sv_rf  <- variable_response(explainer_rf,
                            variable =  "construction.year", type = "pdp")
plot(sv_rf)

sv_lm  <- variable_response(explainer_lm,
                            variable =  "construction.year", type = "pdp")

sv_svm  <- variable_response(explainer_svm,
                             variable =  "construction.year", type = "pdp")
plot(sv_rf, sv_lm, sv_svm)

## for surface

sv_rf  <- variable_response(explainer_rf,
                            variable =  "age", type = "pdp")
sv_lm  <- variable_response(explainer_lm,
                            variable =  "age", type = "pdp")
plot(sv_rf, sv_lm)

#
# Model explainers - Discrete variable response
#
## for district

svd_rf  <- variable_response(explainer_rf,
                             variable = "district", type = "factor")
svd_lm  <- variable_response(explainer_lm,
                             variable = "district", type = "factor")

plot(svd_rf, svd_lm)

#
# Model explainers - Performance
#

# root mean square
predicted_mi2_lm <- predict(apartments_lm_model, apartmentsTest)
sqrt(mean((predicted_mi2_lm - apartmentsTest$m2.price)^2))
## [1] 283.0865

# root mean square
predicted_mi2_rf <- predict(apartments_rf_model, apartmentsTest)
sqrt(mean((predicted_mi2_rf - apartmentsTest$m2.price)^2))
## [1] 283.3479


# Model performance

mp_lm <- model_performance(explainer_lm)
mp_lm

explainer_rf  <- explain(HR_rf_model, data = HR_data, y = HR_data$left)
model_performance(explainer_rf)

mp_rf <- model_performance(explainer_rf)
mp_rf

plot(mp_lm, mp_rf, geom = "boxplot")
plot(mp_lm, mp_rf)

mp_rf <- model_performance(explainer_rf)

library(ggplot2)
ggplot(mp_rf, aes(observed, diff)) + geom_point() + geom_smooth(se = FALSE) +
  xlab("Observed") + ylab("Predicted - Observed") +
  ggtitle("Diagnostic plot for the random forest model") + theme_mi2()

ggplot(mp_lm, aes(observed, diff)) + geom_point() + geom_smooth(se = FALSE) +
  xlab("Observed") + ylab("Predicted - Observed") +
  ggtitle("Diagnostic plot for the linear model") + theme_mi2()


# with auditor

isFALSE <- function(x) x == FALSE

library(auditor)
audit_rf <- audit(explainer_rf)
plotResidual(audit_rf, variable = "gender")

audit_lm <- audit(explainer_lm)
plotResidual(audit_lm, variable = "construction.year")


#
# Model explainers - variable importance
#

vi_rf <- variable_importance(explainer_rf, loss_function = loss_root_mean_square)
vi_rf

plot(vi_rf)

vi_lm <- variable_importance(explainer_lm, loss_function = loss_root_mean_square)
vi_lm

plot(vi_lm, vi_rf)



# 5. Outlier detection

mp_rf <- model_performance(explainer_rf)

library("ggplot2")
ggplot(mp_rf, aes(observed, diff)) + geom_point() +
  xlab("Observed") + ylab("Predicted - Observed") +
  ggtitle("Diagnostic plot for the random forest model") + theme_mi2()

# 6. break Down

which.min(mp_rf$diff)
## 1161
new_apartment <- apartmentsTest[which.min(mp_rf$diff), ]
new_apartment


new_apartment_rf <- single_prediction(explainer_rf,
                                      observation = new_apartment)
new_apartment_lm <- single_prediction(explainer_lm,
                                      observation = new_apartment)
plot(new_apartment_lm, new_apartment_rf)


#
# Excercises
# Try this yourself!
#


###############################################################################################################3


rf_model <- randomForest(performance ~ age +   earnings + gender +   pl_state +channel + ip_country + loan_amount, data = df)



library("DALEX")
library("gbm")

gbm_model <- gbm(as.numeric( df$performance)-1 ~ age +   earnings + gender +   pl_state +channel + ip_country + loan_amount, data = df, n.trees = 1000)

explainer_gbm <- explain(apartments_gbm_model,
                         data = apartmentsTest[,2:6], y = apartmentsTest$m2.price,
                         predict_function = function(m, d) predict(m, d, n.trees = 1000))

library("nnet")
nnet_model <- nnet(as.numeric(df$performance)-1 ~ age +   earnings + gender +   pl_state +channel + ip_country + loan_amount, data = df,
                              linout=TRUE,
                              size = 50, maxit=100)

explainer_nnet <- explain(apartments_nnet_model,
                          data = apartmentsTest[,2:6], y = apartmentsTest$m2.price)

library("e1071")
svm_model <- svm(performance ~ age +   earnings + gender +   pl_state +channel + ip_country + loan_amount, data = df)

explainer_svm <- explain(svm_model,
                         data = df[,3:10], y = df$performance)

library("caret")
mapartments <- model.matrix(m2.price ~ ., data = apartments)
mapartmentsTest <- model.matrix(m2.price ~ ., data = apartmentsTest)
apartments_knn_model <- knnreg(mapartments, apartments[,1], k = 5)

explainer_knn <- explain(apartments_knn_model,
                         data = mapartmentsTest, y = apartmentsTest$m2.price)

# Model performance

mp_knn <- model_performance(explainer_knn)
mp_svm <- model_performance(explainer_svm)
mp_gbm <- model_performance(explainer_gbm)
mp_nnet <- model_performance(explainer_nnet)
plot(mp_gbm, mp_nnet, mp_svm, mp_knn, geom = "boxplot")

# all models

plot(mp_gbm, mp_nnet, mp_svm, mp_knn, mp_lm, mp_rf, geom = "boxplot")
plot(mp_gbm, mp_nnet, mp_svm, mp_knn, mp_lm, mp_rf)
plot(mp_svm, mp_lm, mp_rf)