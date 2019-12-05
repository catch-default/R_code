# Random Forest with Scaled data

setwd("C://Users//inmyr//OneDrive//바탕 화면//BI_팀플")

scaled.train.df <- read.csv("scaled.train.df.csv")
scaled.test.df <- read.csv("scaled.test.df.csv")

View(scaled.train.df)
dim(scaled.train.df)

library(caret)
library(Metrics)
library(MASS)
library(caret)
library(e1071)
library(ROCR)
library(randomForest)

# 수업ver.

# nodesize=5 : 
rf <- randomForest(as.factor(Y) ~ ., data = scaled.train.df, ntree = 500, 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)

## confusion matrix
rf.pred <- predict(rf, scaled.test.df)
confusionMatrix(rf.pred, as.factor(scaled.test.df$Y), positive="1")

######################################

# 블로그 ver.
# https://rpubs.com/phamdinhkhanh/389752

library(randomForest)
library(caret)

# Load Dataset
x <- scaled.train.df[,-22]
y <- scaled.train.df[,22]


# Create model with default paramters

# mtry : feature 수
# ntree : tree 수

# 각 매개 변수의 권장 기본값과 mtry = floor (sqrt (ncol (x))) ntree = 500 을 사용

control <- trainControl(method="repeatedcv", number=10, repeats=3)
seed <- 7
metric <- "Accuracy"
set.seed(seed)
mtry <- sqrt(ncol(x)) #tree 하나에 들어가는 feature의 수
tunegrid <- expand.grid(.mtry=mtry)
rf_default <- train(as.factor(Y)~., data=scaled.train.df, method="rf", metric=metric, tuneGrid=tunegrid, trControl=control)
print(rf_default)

## variable importance plot
varImp(rf_default)

## confusion matrix
rf_default.pred <- predict(rf_default, scaled.test.df)
confusionMatrix(rf_default.pred, as.factor(scaled.test.df$Y), positive="1")

#####################################
# caret을 이용한 튜닝 : mtry만 조정-> 최종 정확도에 많은 영향 미침
# ntree : 튜닝 중요도가 mtry보다 낮음, 그리고 이건 손으로 할 수 있음

# Random Search
# caret을 이용하여 mtry를 무작위로 검색

control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(seed)
mtry <- sqrt(ncol(x))
rf_random <- train(as.factor(Y)~., data=scaled.train.df, method="rf", metric=metric, tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

#####################################


customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
    randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
    predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes

# train model
control <- trainControl(method="repeatedcv", number=3, repeats=1)
tunegrid <- expand.grid(.mtry=c(1:10), .ntree=c(500,1000,1500))
set.seed(seed)
custom <- train(as.factor(Y)~., data=scaled.train.df, method=customRF, metric=metric, tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)

##########################################
# mtry=2, ntree=1000

rf <- randomForest(as.factor(Y) ~ ., data = scaled.train.df, ntree = 1000, 
                   mtry = 2, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)

## confusion matrix
rf.pred <- predict(rf, scaled.test.df)
confusionMatrix(rf.pred, as.factor(scaled.test.df$Y), positive="1")

varImp(rf)
