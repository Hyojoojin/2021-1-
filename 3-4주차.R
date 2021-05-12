library(tidyverse)
library(data.table)
library(magrittr)
setwd("C:/Users/wlsgy/OneDrive/바탕 화면/Label_encdoing 최종의최종")
library(plyr)
library(mlr)
################
#데이터불러오기#
################
mice_train <- fread('mice_train.csv',
                 header=TRUE, 
                 data.table = F)
mice_test <- fread('mice_test.csv',
                   header = TRUE, 
                   data.table = F)
hotdeck_train <- fread('hotdeck_train.csv',
                   header = TRUE, 
                   data.table = F)
hotdeck_test <- fread('hotdeck_test.csv',
                       header = TRUE, 
                       data.table = F)

nafactor_train <- fread('nafactor_train.csv',
                           header = TRUE, 
                           stringsAsFactors = TRUE, 
                           data.table = F)
nafactor_test <- fread('nafactor_test.csv',
                           header = TRUE, 
                           stringsAsFactors = TRUE, 
                           data.table = F)
test_set <- fread('test_set_features.csv',
                  header=TRUE, 
                  data.table = F)

lrn.rfsrc = makeLearner("multilabel.randomForestSRC", predict.type = "prob")

###################
#MICE train & test#
###################

mice_train_labels <- mice_train[, c("h1n1_vaccine", "seasonal_vaccine")]
mice_new_train <- mice_train %>% select(-c("respondent_id","h1n1_vaccine", "seasonal_vaccine"))

mice_train_labels <- sapply(mice_train_labels, as.logical)

mice_new_train <- cbind(mice_new_train, mice_train_labels)

mice_train_labels <- colnames(mice_train_labels)

#Step 1: Create a task.
data.task <- makeMultilabelTask(data = mice_new_train, target = mice_train_labels)

data.task

#Parameter tuning
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)
rancontrol <- makeTuneControlRandom(maxit = 70)
set_cv <- makeResampleDesc("CV",iters = 5)

rf_tune <- tuneParams(learner = "multilabel.randomForestSRC", resampling = set_cv, 
                      task = data.task, 
                      par.set = rf_param, control = rancontrol)
rf_tune$x
#ntree: 177, mtry: 9, nodesize: 10
rf_tune$y #hamloss: 0.1909048
rf.tree <- setHyperPars(lrn.rfsrc, par.vals = rf_tune$x)
#Step 2 : Train the model
modelRFSRC <- train(rf.tree, data.task, subset = 1:21360)
modelRFSRC

#Step 3: Prediction
predRFSRC <- predict(modelRFSRC, task = data.task, subset = 21360:26707)

#Step 4: Performance check
performance <- performance(predRFSRC, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))
accuracy_rfsrc <- performance[4]
accuracy_rfsrc <- round(accuracy_rfsrc*100, 2)
hamloss_rfsrc <-performance[1]

cat('Accuracy:',accuracy_rfsrc)
cat('Hamloss:', hamloss_rfsrc)
#Accuracy : 71.91
#Hamloss: 0.1912865

#Step 5: Test data

predictions <- predict(modelRFSRC, newdata = mice_test)
predictions <- as.data.frame(predictions)

colnames(predictions) <- mice_train_labels
predictions %<>% select(c(h1n1_vaccine, seasonal_vaccine))

mice_predict <- cbind(respondent_id = test_set$respondent_id, 
                     h1n1_vaccine = predictions$h1n1_vaccine, 
                     seasonal_vaccine = predictions$seasonal_vaccine)
mice_predict
write.csv(mice_predict, file = "mice_predict.csv", row.names = FALSE)


######################
#Hotdeck train & test#
######################

hotdeck_train_labels <- hotdeck_train[, c("h1n1_vaccine", "seasonal_vaccine")]
hotdeck_new_train <- hotdeck_train %>% select(-c("respondent_id","h1n1_vaccine", "seasonal_vaccine"))

hotdeck_train_labels <- sapply(hotdeck_train_labels, as.logical)

hotdeck_new_train <- cbind(hotdeck_new_train, hotdeck_train_labels)

hotdeck_train_labels <- colnames(hotdeck_train_labels)

#Step 1: Create a task.
data.task <- makeMultilabelTask(data = hotdeck_new_train, target = hotdeck_train_labels)

data.task

#Parameter tuning
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)
rancontrol <- makeTuneControlRandom(maxit = 70)
set_cv <- makeResampleDesc("CV",iters = 5)

rf_tune <- tuneParams(learner = "multilabel.randomForestSRC", resampling = set_cv, 
                      task = data.task, 
                      par.set = rf_param, control = rancontrol)

rf_tune$x
#ntree 485, mtry 9, nodesize 10
rf_tune$y #hamloss 0.1956603
rf.tree <- setHyperPars(lrn.rfsrc, par.vals = rf_tune$x)

#Step 2 : Train the model
modelRFSRC <- train(rf.tree, data.task, subset = 1:21360)
modelRFSRC

#Step 3: Prediction
predRFSRC <- predict(modelRFSRC, task = data.task, subset = 21360:26707)

#Step 4: Performance check
performance <- performance(predRFSRC, measures = list(multilabel.hamloss, multilabel.subset01, multilabel.f1, multilabel.acc))
accuracy_rfsrc <- performance[4]
accuracy_rfsrc <- round(accuracy_rfsrc*100, 2)
hamloss_rfsrc <- performance[1]
cat('Accuracy:',accuracy_rfsrc)
cat('Hamloss:', hamloss_rfsrc)
#Accuracy: 71.03
#Hamloss: 0.1968025

#Step 5: Test data

predictions <- predict(modelRFSRC, newdata = hotdeck_test)
predictions <- as.data.frame(predictions)

colnames(predictions) <- hotdeck_train_labels
predictions %<>% select(c(h1n1_vaccine, seasonal_vaccine))

hotdeck_predict <- cbind(respondent_id = test_set$respondent_id, 
                      h1n1_vaccine = predictions$h1n1_vaccine, 
                      seasonal_vaccine = predictions$seasonal_vaccine)
hotdeck_predict
write.csv(hotdeck_predict, file = "hotdeck_predict.csv", row.names = FALSE)

#######################
#nafactor train & test#
#######################

set.seed(1234)

nafactor_train_labels <- nafactor_train[, c("h1n1_vaccine", "seasonal_vaccine")]
nafactor_new_train <- nafactor_train %>% select(-c("X","respondent_id","h1n1_vaccine", "seasonal_vaccine"))

nafactor_train_labels <- sapply(nafactor_train_labels, as.logical)

nafactor_new_train <- cbind(nafactor_new_train, nafactor_train_labels)

nafactor_train_labels <- colnames(nafactor_train_labels)
nafactor_new_train %>% head()
#Step 1: Create a task.
data.task <- makeMultilabelTask(data = nafactor_new_train, target = nafactor_train_labels)

data.task

#Parameter tuning
rf_param <- makeParamSet(
  makeIntegerParam("ntree",lower = 50, upper = 500),
  makeIntegerParam("mtry", lower = 3, upper = 10),
  makeIntegerParam("nodesize", lower = 10, upper = 50)
)
rancontrol <- makeTuneControlRandom(maxit = 50)
set_cv <- makeResampleDesc("CV",iters = 10)

rf_tune <- tuneParams(learner = "multilabel.randomForestSRC", resampling = set_cv, 
                      task = data.task, 
                      par.set = rf_param, control = rancontrol)
rf_tune$x # ntree 236, mtry 10 nodesize 17
rf_tune$y #hamloss 0.183042

rf.parameter <- list(ntree = 236, mtry = 10, nodesize = 17)
rf.tree <- setHyperPars(lrn.rfsrc, par.vals = rf.parameter)

#Step 2 : Train the model
modelRFSRC <- train(rf.tree, data.task, subset = 1:21360)
modelRFSRC

#Step 3: Prediction
predRFSRC <- predict(modelRFSRC, task = data.task, subset = 21360:26707)

#Step 4: Performance check
performance <- performance(predRFSRC, 
                           measures = list(multilabel.hamloss, multilabel.subset01, 
                                           multilabel.f1, multilabel.acc))
accuracy_rfsrc <- performance[4]
accuracy_rfsrc <- round(accuracy_rfsrc*100, 2)
hamloss_rfsrc <- performance[1]
cat('Accuracy:',accuracy_rfsrc)
cat('Hamloss:', hamloss_rfsrc)
#accuracy : 72.3
#hamloss: 0.1860



#Step 5: Test data
nafactor_test %<>% select(-respondent_id)
predictions <- predict(modelRFSRC, newdata = nafactor_test)
predictions <- as.data.frame(predictions)

colnames(predictions) <- nafactor_train_labels
predictions %<>% select(c(h1n1_vaccine, seasonal_vaccine))

nafactor_predict <- cbind(respondent_id = test_set$respondent_id, 
                         h1n1_vaccine = predictions$h1n1_vaccine, 
                         seasonal_vaccine = predictions$seasonal_vaccine)
nafactor_predict
write.csv(nafactor_predict, file = "nafactor_predict.csv", row.names = FALSE)

########
#시각화#
########

setwd("C:/Users/wlsgy/OneDrive/바탕 화면/onehot_encoding 최종의최종")
onehot_nafactor_train <- fread('onehot_nafactor_train.csv',
                               header=TRUE, 
                               data.table = F)

acc.hamloss <- data.frame(
  hamloss = c(0.1912865, 0.1956603, 0.1860), 
  accuracy = c(71.91, 71.03, 72.3), 
  type = c("MICE", "Hotdeck", "NAfactor")
)

hamloss_plot <- acc.hamloss %>% 
  ggplot(aes(x = type, y = hamloss))+
  geom_bar(stat = 'identity', fill = "#6397D6")+theme_bw()

accuracy_plot <- acc.hamloss %>% 
  ggplot(aes(x = type, y = accuracy))+
  geom_bar(stat = 'identity', fill = "#E892C4")+theme_bw()

hamloss_plot
accuracy_plot

#NAfactor logistic#


nafactor_train_h1n1 <- onehot_nafactor_train %>% select(-c(respondent_id,
                                                    seasonal_vaccine))
nafactor_train_seasonal <- onehot_nafactor_train %>% select(-c(respondent_id,
                                                        h1n1_vaccine))

logistic_nafactor <- glm(h1n1_vaccine~., data = nafactor_train_h1n1)
summary(logistic_nafactor)
variable_select_h1n1 <- step(logistic_nafactor, direction = "both")

summary(variable_select_h1n1)
logistic_nafactor_2 <- glm(seasonal_vaccine~., data = nafactor_train_seasonal)
variable_select_seasonal <- step(logistic_nafactor_2, direction = "both")
summary(variable_select_seasonal)
summary(logistic_nafactor_2)

library(caret)
h1n1_imp <- varImp(logistic_nafactor, scale = FALSE)
h1n1_imp$variable = rownames(h1n1_imp)
h1n1_imp %>% select(variable, Overall) %>% arrange(-Overall)

plot(h1n1_imp, top = 10)
seasonal_imp <- varImp(logistic_nafactor_2, scale = FALSE)
seasonal_imp$variable = rownames(seasonal_imp)
seasonal_imp %>% select(variable, Overall) %>% arrange(-Overall)
