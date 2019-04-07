library(h2o)        # Professional grade ML pkg
library(lime)       # Explain complex black-box ML models
# Initialize H2O JVM
h2o.init()
# Split data into Train/Validation/Test Sets
hr_data_h2o <- as.h2o(df)

split_h2o <- h2o.splitFrame(hr_data_h2o, c(0.7, 0.15), seed = 1234 )

train_h2o <- h2o.assign(split_h2o[[1]], "train" ) # 70%
valid_h2o <- h2o.assign(split_h2o[[2]], "valid" ) # 15%
test_h2o  <- h2o.assign(split_h2o[[3]], "test" )  # 15%
y <- "performance"
x <- setdiff(names(train_h2o), y)

automl_models_h2o <- h2o.automl(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  leaderboard_frame = valid_h2o,
  max_runtime_secs  = 100
)

automl_models_h2o <-h2o.randomForest(
  x = x, 
  y = y,
  training_frame    = train_h2o,
  validation_frame = valid_h2o,
  nfolds=10,
  ntrees=50,
  binomial_double_trees=TRUE
)

# Extract leader model
automl_leader <- automl_models_h2o@leader

# Predict on hold-out set, test_h2o
pred_h2o <- h2o.predict(object = automl_models_h2o, newdata = test_h2o)

pred_h2o <- h2o.predict(object = automl_leader, newdata = test_h2o)

test_performance <- test_h2o %>%
  tibble::as_tibble() %>%
  select(performance) %>%
  add_column(pred = as.vector(pred_h2o$predict)) %>%
  add_column(pr_good = as.vector(pred_h2o$GOOD)) %>%
  mutate_if(is.character, as.factor)
test_performance


confusion_matrix <- test_performance[test_performance$pr_good>0,1:2] %>%
  table() 
confusion_matrix

# Performance analysis
tn <- confusion_matrix[1]
tp <- confusion_matrix[4]
fp <- confusion_matrix[3]
fn <- confusion_matrix[2]

accuracy <- (tp + tn) / (tp + tn + fp + fn)
misclassification_rate <- 1 - accuracy
recall <- tp / (tp + fn)
precision <- tp / (tp + fp)
null_error_rate <- tn / (tp + tn + fp + fn)

tibble(
  accuracy,
  misclassification_rate,
  recall,
  precision,
  null_error_rate
) %>% 
  transpose() 

class(automl_leader)



# Setup lime::model_type() function for h2o
model_type.H2OBinomialModel <- function(x, ...) {
  # Function tells lime() what model type we are dealing with
  # 'classification', 'regression', 'survival', 'clustering', 'multilabel', etc
  #
  # x is our h2o model
  
  return("classification")
}


# Setup lime::predict_model() function for h2o
predict_model.H2OBinomialModel <- function(x, newdata, type, ...) {
  # Function performs prediction and returns dataframe with Response
  #
  # x is h2o model
  # newdata is data frame
  # type is only setup for data frame
  
  pred <- h2o.predict(x, as.h2o(newdata))
  
  # return probs
  return(as.data.frame(pred[,-1]))
  
}


# Test our predict_model() function
predict_model(x = automl_models_h2o #automl_leader
              , newdata = as.data.frame(test_h2o[1:6,-1]), type = 'raw') %>%
  tibble::as_tibble()


# Run lime() on training set
explainer <- lime::lime(
  as.data.frame(train_h2o[,-1]), 
  model          = automl_models_h2o #automl_leader
  , 
  bin_continuous = FALSE)



# Run explain() on explainer
explanation <- lime::explain(
  as.data.frame(test_h2o[1:6,-1]), 
  explainer    = explainer, 
  n_labels     = 1, 
  n_features   = 5,
  kernel_width = 0.5)


plot_features(explanation) +
  labs(title = "HR Predictive Analytics: LIME Feature Importance Visualization",
       subtitle = "Hold Out (Test) Set, First 10 Cases Shown")



library("titanic")
titanic <- titanic_train[,c("Survived", "Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")]
titanic$Survived <- factor(titanic$Survived)
titanic$Sex <- factor(titanic$Sex)
titanic$Embarked <- factor(titanic$Embarked)
levels(titanic$Embarked)[1] = "N"

titanic=df
titanic <- na.omit(titanic)
head(titanic)
titanic$id=NULL

library("randomForest")
rf_model <- randomForest(performance ~ .,  data = titanic)

rf_model <- randomForest(Survived ~ .,  data = titanic)
rf_model
levels(titanic$performance)=c("0","1")

rf_model <- randomForest(Survived ~ .,  data = titanic)
rf_model

library("DALEX")
rf_explain <- explain(rf_model, data = titanic[,-1],
                      y = titanic$Survived == "1", label = "Random Forest v7")

vi_rf <- variable_importance(rf_explain, type = "difference")
head(vi_rf)

plot(vi_rf)

vr_age  <- variable_response(rf_explain, variable =  "Age")

head(vr_age)

plot(vr_age, use_facets = TRUE)

vr_pclass  <- variable_response(rf_explain, variable =  "Pclass")
plot(vr_pclass, use_facets = TRUE)

vr_fare  <- variable_response(rf_explain, variable =  "Fare")
plot(vr_fare, use_facets = TRUE)

vr_embarked  <- variable_response(rf_explain, variable =  "Embarked")
plot(vr_embarked)

new_passanger <- data.frame(
  Pclass = 1,
  Sex = factor("male", levels = c("female", "male")),
  Age = 8,
  SibSp = 0,
  Parch = 0,
  Fare = 72,
  Embarked = factor("C", levels = c("N","C","Q","S"))
)

sp_rf <- single_prediction(rf_explain, new_passanger)
plot(sp_rf)




