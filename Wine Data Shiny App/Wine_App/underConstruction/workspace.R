library(caret)
library(e1071)
library(MASS)
library(nnet)

setwd("/home/flocf/Documents/git/SPL/")

WhiteWine = read.csv("Wine_App/data/winequality-white.csv", sep = ";")
RedWine   = read.csv("Wine_App/data/winequality-red.csv", sep = ";")

# scale data
WhiteWine[-12] = scale(WhiteWine[-12], center = TRUE, scale = TRUE)
RedWine[-12] = scale(RedWine[-12], center = TRUE, scale = TRUE)

## Splitting into Tets & training set
# White Wine
#set.seed(123)
intrain = createDataPartition(y = WhiteWine$quality, p = 0.8, list = FALSE)
White_train = WhiteWine[intrain, ]
White_test = WhiteWine[- intrain, ]
# Red Wine
#set.seed(123)
intrain = createDataPartition(y = RedWine$quality, p = 0.8, list = FALSE)
Red_train = RedWine[intrain, ]
Red_test = RedWine[- intrain, ]
# Remove intrain
rm(intrain)

## One-vs-Rest SVM ######################################################
# WhiteWine
class_probs = list()
svm_models = list()
accuracy = numeric()
for (i in sort(unique(WhiteWine$quality))) {
  training = data.frame(y = as.factor(ifelse(White_train$quality == i, 1, 0)), White_train[,-12])
  testing = data.frame(y = as.factor(ifelse(White_test$quality == i, 1, 0)), White_test[,-12])
  
  svm_model = svm(y ~ ., data = training, kernel = "poly", degree = 2, coef0 = 1, probability = TRUE)
  svm_models[[as.character(i)]] = svm_model
  pred = predict(svm_model, testing, probability = TRUE)
  class_probs[[as.character(i)]] = attr(pred, "probabilities")[, 2]
  
  accuracy = c(accuracy, sum(as.numeric(svm_model$fitted) == as.numeric(training$y))/nrow(training))
}

White_pred_svm = apply(as.data.frame(class_probs), 1, FUN = which.max) + 2

# RedWine
class_probs = list()
for (i in sort(unique(RedWine$quality))) {
  training = data.frame(y = as.factor(ifelse(Red_train$quality == i, 1, 0)), Red_train[-12])
  testing = data.frame(y = as.factor(ifelse(Red_test$quality == i, 1, 0)), Red_test[-12])
  
  svm_model = svm(y ~ ., data = training, probability = TRUE)
  pred = predict(svm_model, testing, probability = TRUE)
  class_probs[[as.character(i)]] = attr(pred, "probabilities")[, 2]
}

Red_pred_svm = apply(as.data.frame(class_probs), 1, FUN = which.max) + 2

## Random Forest ##########################################################
# WhiteWine
set.seed(123)
model <- train(
  as.factor(quality) ~ .,
  tuneGrid = data.frame(mtry = c(2,3,7), splitrule = 'extratrees', min.node.size = 2),
  # tuneGrid = data.frame(decay= 0),
  # tuneGrid = data.frame(method= "logistic"),
  # tuneLength = 10,
  data = White_train, method = "ranger",
  trControl = trainControl(method = "cv", number = 2, verboseIter = TRUE)
)
plot(model)
sum(predict(model, White_test[,-12])==White_test$quality)/nrow(White_test)
model$results

## Simple Regression ######################################################
# WhiteWine
lr_model_white = lm(quality ~ ., data = White_train)
White_pred_lr  = round(predict(lr_model_white, newdata = White_test))

# RedWine
lr_model_red = lm(quality ~ ., data = Red_train)
Red_pred_lr  = round(predict(lr_model_red, newdata = Red_test))

## Naive Bayes ######################################################
# WhiteWine
class_probs = list()
for (i in sort(unique(WhiteWine$quality))) {
  training = data.frame(y = as.factor(ifelse(White_train$quality == i, 1, 0)), White_train[-12])
  testing = data.frame(y = as.factor(ifelse(White_test$quality == i, 1, 0)), White_test[-12])
  
  nb_model = naiveBayes(y ~ ., data = training)
  class_probs[[as.character(i)]] = predict(nb_model, testing, type = "raw")[, 2]
}

White_pred_nb = apply(as.data.frame(class_probs), 1, FUN = which.max) + 2

# RedWine
class_probs = list()
for (i in sort(unique(RedWine$quality))) {
  training = data.frame(y = as.factor(ifelse(Red_train$quality == i, 1, 0)), Red_train[-12])
  testing = data.frame(y = as.factor(ifelse(Red_test$quality == i, 1, 0)), Red_test[-12])
  
  nb_model = naiveBayes(y ~ ., data = training)
  class_probs[[as.character(i)]] = predict(nb_model, testing, type = "raw")[, 2]
}

Red_pred_nb = apply(as.data.frame(class_probs), 1, FUN = which.max) + 2

## Orderit Logit ######################################################
# WhiteWine
olog_model_white = polr(as.factor(quality) ~ ., data = White_train, Hess=TRUE)
White_pred_olog = predict(olog_model_white, newdata = White_test)

# RedWine
olog_model_red = polr(as.factor(quality) ~ ., data = Red_train, Hess=TRUE)
Red_pred_olog = predict(olog_model_red, newdata = Red_test)

## Multinomial Logit ######################################################
# WhiteWine
mlog_model_white = multinom(as.factor(quality) ~ ., data = White_train)
White_pred_mlog = predict(mlog_model_white, newdata = White_test)

# RedWine
mlog_model_red = multinom(as.factor(quality) ~ ., data = Red_train)
Red_pred_mlog = predict(mlog_model_red, newdata = Red_test)

print("********* SVM - White Wine:")
print(sum(White_pred_svm==White_test$quality)/nrow(White_test))

print("********* LR - White Wine:")
print(sum(White_pred_lr==White_test$quality)/nrow(White_test))

print("********* NB - White Wine:")
print(sum(White_pred_nb==White_test$quality)/nrow(White_test))

print("********* OLOG - White Wine:")
print(sum(White_pred_olog==White_test$quality)/nrow(White_test))

print("********* MLOG - White Wine:")
print(sum(White_pred_mlog==White_test$quality)/nrow(White_test))

print("********* SVM - Red Wine:")
print(sum(Red_pred_svm==Red_test$quality)/nrow(Red_test))

print("********* LR - Red Wine:")
print(sum(Red_pred_lr==Red_test$quality)/nrow(Red_test))

print("********* NB - Red Wine:")
print(sum(Red_pred_nb==Red_test$quality)/nrow(Red_test))

print("********* OLOG - Red Wine:")
print(sum(Red_pred_olog==Red_test$quality)/nrow(Red_test))

print("********* MLOG - Red Wine:")
print(sum(Red_pred_mlog==Red_test$quality)/nrow(Red_test))

# trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3, classProbs = TRUE)
# set.seed(3233)
# 
# svm_Linear = train(y ~., data = training, method = "svmLinear",
#                     trControl = trctrl,
#                     preProcess = c("center", "scale"),
#                     tuneLength = 10)
# 
# predict = predict(svm_Linear, newdata = testing, probability = TRUE)