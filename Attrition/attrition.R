#https://pbiecek.github.io/DALEX/articles/vignette_titanic.html

library(tidyverse)
library(readxl)

hr_data_raw <- read_excel(path = "Attrition/WA_Fn-UseC_-HR-Employee-Attrition.xlsx")
head(hr_data_raw)

hr_data <- hr_data_raw %>%
  mutate_if(is.character, as.factor) %>%
  select(Attrition, everything())

hr_data <- na.omit(hr_data)

summary(hr_data)

hr_data<-hr_data%>%select( "Attrition", "Age","BusinessTravel","DailyRate","Department","DistanceFromHome","Education","EducationField",
                           "EnvironmentSatisfaction","Gender",  "HourlyRate", "JobInvolvement","JobLevel",               
                           "JobRole" ,"JobSatisfaction","MaritalStatus","MonthlyIncome","MonthlyRate","NumCompaniesWorked", "OverTime","PercentSalaryHike",
                           "PerformanceRating", "RelationshipSatisfaction", "StandardHours", "StockOptionLevel" ,"TotalWorkingYears" ,"TrainingTimesLastYear",   
                           "WorkLifeBalance" ,"YearsAtCompany","YearsInCurrentRole","YearsSinceLastPromotion","YearsWithCurrManager"  )

library("randomForest")

model_hr_rf <- randomForest(Attrition == "No" ~ .,  data = hr_data)
model_hr_rf

library("DALEX")
explain_hr_rf <- explain(model_hr_rf, 
                         data = hr_data[,-1],
                         y = hr_data$Attrition == "No", 
                         label = "Random Forest v7")
vi_rf <- variable_importance(explain_hr_rf)
head(vi_rf)
plot(vi_rf)

vr_age  <- variable_response(explain_hr_rf, variable =  "Age")
head(vr_age)

plot(vr_age, use_facets = TRUE)


new_passanger <- hr_data[15,-1] #yes
new_passanger2 <- hr_data[15,-1] #no

sp_rf <- single_prediction(explain_hr_rf, new_passanger)
plot(sp_rf)
sp_rf <- single_prediction(explain_hr_rf, new_passanger2)
plot(sp_rf)
