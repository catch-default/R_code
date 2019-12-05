library(readxl)
library(caret)
library(descr)
library(MASS)
library(psych)
library(corrplot)
library(dplyr)

card.df<-readxl::read_excel(path="default of credit card clients.xls", sheet="Data", col_names=TRUE)
card.df <- card.df[-1,]
card.df <- card.df[,-1]
attach(card.df)
#View(card.df)

# char 데이터 타입 numeric으로 바꾸기
str(card.df)

Variables <- colnames(card.df)

for(i in c(1,3,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)){
  a <- as.numeric(get(Variables[i]))
  card.df[,i] <- a
}


# Gender 와  Marital status 만 Factor 변수로 변환
for(i in c(2,4,24)){
  a <- as.factor(get(Variables[i]))
  card.df[,i] <- a
}

# X3 missing value -> NA 값으로 대체
freq <- table(card.df$X3) 
freq

card.df$X3[card.df$X3==0]<-NA
card.df$X3[card.df$X3==5]<-NA
card.df$X3[card.df$X3==6]<-NA

## X4 missing value -> NA 값으로 대체

freq <- table(card.df$X4) 
freq

card.df$X4[card.df$X4=="0"]<-NA

## NA값이 있는 데이터 행 지우기
card.df <- na.omit(card.df)

# missing value check
sum(is.na(card.df))



# Y(=output)의 빈도표와 막대그래프 그리기
y_table <- table(card.df$Y)
y_table

# normal&default client data set 나누기

normal.df <- subset(card.df, Y == "0")
default.df <- subset(card.df, Y == "1")

dim(normal.df)
dim(default.df)

# default clients 수(6,605)에 맞춰 normal clients down sampling
set.seed(1)
normal.sp.index <- sample(c(1:dim(normal.df)[1]), 6605)
normal.sp.df<- normal.df[normal.sp.index, ]

# nomal.sp.df와 default.df 합치기
card.df<-rbind(normal.sp.df,default.df)

#View(card.df)

# describe nomal.sp.df and default.df
describe(normal.sp.df)
describe(default.df)

#########EDA########
# ## boxplot 그리기
# 
# # X1
# ggplot(data = card.df, aes(y = card.df$X1,x= card.df$Y)) + geom_boxplot(fill = "orange")
# 
# #X2
# card.df %>% 
#   mutate(X2 = factor(X2)) %>% 
#   ggplot(aes(X2, fill = Y)) +
#   geom_bar(position = "dodge") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
#   scale_fill_brewer(palette = "Set1")
# 
# #X3
# card.df %>% 
#   mutate(X3 = factor(X3)) %>% 
#   ggplot(aes(X3, fill = Y)) +
#   geom_bar(position = "dodge") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
#   scale_fill_brewer(palette = "Set1")
# 
# # X4
# card.df %>% 
#   mutate(X4 = factor(X4)) %>% 
#   ggplot(aes(X4, fill = Y)) +
#   geom_bar(position = "dodge") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
#   scale_fill_brewer(palette = "Set1")
# 
# # X6
# card.df %>% 
#   mutate(X6 = factor(X6)) %>% 
#   ggplot(aes(X6, fill = Y)) +
#   geom_bar(position = "dodge") +
#   theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
#   scale_fill_brewer(palette = "Set1")
# 
# 
# ## correlation plot 그리기
# 
# # Amount of bill statement (NT dollar) correlation plot
# card.df.cor1 = cor(card.df[,c(12:17)], method = c("spearman"))
# corrplot(card.df.cor1)
# 
# # Amount of previous payment (NT dollar) correlation plot
# card.df.cor2 = cor(card.df[,c(18:23)], method = c("spearman"))
# corrplot(card.df.cor2)


########EDA########

# 데이터 전처리 : numeric variables scaling 하기

prep <- preProcess(card.df, "range")
scaled.card.df <- predict(prep, card.df)
View(scaled.card.df)

write.csv(card.df, file="card.df.csv",row.names=FALSE)
write.csv(scaled.card.df, file="scaled.card.df.csv",row.names=FALSE)

dim(card.df)
dim(scaled.card.df)

str(scaled.card.df)

# factor 변수 -> dummie 변수 만들기
library(dummies)
card.dum <- scaled.card.df[,c(2,4)]
card.dum <- as.data.frame(card.dum)

scaled.card.df <- scaled.card.df[, -c(2,4)]
card.dum <- dummy.data.frame(card.dum, sep=".", omit.constants=FALSE)

#더미변수 생성 후 각 변수마다 1개씩 제거
card.dum <- card.dum[,-c(2, 5)]

# scaled.card.df <- cbind(scaled.card.df,card.dum)
#dim(scaled.card.df)
# View(scaled.card.df)

#str(scaled.card.df)

# PCA
library(HSAUR)
summary(scaled.card.df)

options("scipen" = 999) 

for.pca <- scaled.card.df[,-22]

sum(is.na(for.pca))

var(for.pca)

hep.data.pca <- prcomp(for.pca, scale. =T)
hep.data.pca

summary(hep.data.pca)

class(hep.data.pca)

final.pca <- hep.data.pca$x[, 1:11]

#PCA 후 card.dum 으로 분리해두었던 factor 변수를 다시 scaled.card.df 에 이어 붙임
scaled.card.df <- cbind(scaled.card.df,card.dum)

y_data <- scaled.card.df[, "Y"]
y_data <- as.numeric(as.character(y_data))

#PCA 한 final.pca 에 Y 변수 및 Factor 변수 이어 붙여주기
final.pca <- cbind(final.pca, y_data)

final.pca <- cbind(final.pca,card.dum)

final.pca <- rename(final.pca, "Y" = "y_data")

##### PCA 적용 안한 data :  scaled.card.df
##### PCA 적용 한 data : final.pca

dim(scaled.card.df)
dim(final.pca)


# partition data
set.seed(12)
train.index <- sample(c(1:dim(final.pca)[1]), dim(final.pca)[1]*0.7)

pca.train.df <- final.pca[train.index, ]
pca.test.df <- final.pca[-train.index, ]

pca.train.df <- as.data.frame(pca.train.df)
pca.test.df <- as.data.frame(pca.test.df)


scaled.train.df <- scaled.card.df[train.index, ]
scaled.test.df <- scaled.card.df[-train.index, ]

scaled.train.df <- as.data.frame(scaled.train.df)
scaled.test.df <- as.data.frame(scaled.test.df)

#로지스틱 회귀

# 1. PCA data
pca.logit.reg <- glm(Y ~ ., data = pca.train.df, family = "binomial") 
options(scipen=999)
summary(pca.logit.reg)

pca.logit.reg.pred <- predict(pca.logit.reg, pca.test.df, type = "response")
summary(pca.logit.reg.pred)

library(caret)
pca.logit.pred <- predict(pca.logit.reg, pca.test.df)
confusionMatrix(factor(ifelse(pca.logit.pred > 0.5, 1, 0)), factor(pca.test.df$Y))

## 10-fold
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod.pca.reg <- train(Y ~., data = pca.train.df, method = "glm", family = "binomial", trControl = ctrl, tuneLength = 5)
mod.pca.reg.pred <- predict(mod.pca.reg, newdata = pca.test.df)
confusionMatrix(factor(ifelse(mod.pca.reg.pred > 0.5, 1, 0)), factor(pca.test.df$Y), positive = "1")


# 2. PCA 안한 데이터

scaled.logit.reg <- glm(Y ~ ., data = scaled.train.df, family = "binomial") 
options(scipen=999)
summary(scaled.logit.reg)

scaled.logit.reg.pred <- predict(scaled.logit.reg, scaled.test.df, type = "response")
summary(scaled.logit.reg.pred)

#library(caret)
scaled.logit.pred <- predict(scaled.logit.reg, scaled.test.df)
confusionMatrix(factor(ifelse(scaled.logit.pred > 0.5, 1, 0)), factor(scaled.test.df$Y), positive = "1")

## 10-fold
ctrl <- trainControl(method = "repeatedcv", number = 10, savePredictions = TRUE)
mod.scaled.reg <- train(Y ~., data = scaled.train.df, method = "glm", family = "binomial", trControl = ctrl, tuneLength = 5)
mod.scaled.reg.pred <- predict(mod.scaled.reg, newdata = scaled.test.df)
confusionMatrix(factor(ifelse(mod.scaled.reg.pred > 0.5, 1, 0)), factor(scaled.test.df$Y), positive = "1")

# Neural Net

# 1. PCA data
library(neuralnet)
# 
# pca.nn <- neuralnet(Y ~ ., data = pca.train.df, hidden = c(3, 3, 3), stepmax=1e6)
# pca.train.pred <- as.data.frame(compute(pca.nn, pca.train.df))
# # pca.train.pred2 <- predict(pca.nn, pca.test.df)
# #pca.train.class <- apply(ifelse(pca.train.pred$net.result > 0.5, 1 , 0))
# confusionMatrix(as.factor(ifelse(pca.train.pred$net.result > 0.5, 1, 0)), as.factor(pca.train.df$Y))
# 
# pca.test.pred <- compute(pca.nn, pca.test.df)
# confusionMatrix(as.factor(ifelse(pca.test.pred$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))
# 
# # confusionMatrix(as.factor(pca.train.class), as.factor(pca.train.df$Y))
# # class(pca.train.pred)
# 
# # 2. PCA 안한 데이터 
# scaled.nn <- neuralnet(Y ~ ., data = scaled.train.df, hidden = c(2, 5))
# scaled.train.pred <- compute(scaled.nn, scaled.train.df)
# # scaled.train.class <- apply(scaled.train.pred$net.result,1,which.max)-1
# confusionMatrix(as.factor(ifelse(scaled.train.pred > 0.5, 1, 0)), as.factor(scaled.train.df))
# 
# scaled.test.pred <- compute(scaled.nn, scaled.test.df)
# # scaled.test.class <- apply(scaled.test.pred$net.result, 1, which.max)-1
# confusionMatrix(as.factor(ifelse(scaled.test.pred > 0.5, 1, 0)), as.factor(scaled.test.df))

## 1. PCA data
pca.nn1.2 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(1,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e6)
pca.pred1.2 <- as.data.frame(compute(pca.nn1.2, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred1.2$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))

pca.nn2.2 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(2,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e6)
pca.pred2.2 <- as.data.frame(compute(pca.nn2.2, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred2.2$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))

pca.nn3.2 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(3,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e6)
pca.pred3.2 <- as.data.frame(compute(pca.nn3.2, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred3.2$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))


pca.nn1.2.1 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(1,2,1) ,linear.output=FALSE, threshold=0.01,stepmax = 1e7)
pca.pred1.2.1 <- as.data.frame(compute(pca.nn1.2.1, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred1.2.1$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))

pca.nn2.2.1 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(2,2,1) ,linear.output=FALSE, threshold=0.01,stepmax = 1e7)
pca.pred2.2.1 <- as.data.frame(compute(pca.nn2.2.1, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred2.2.1$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))

pca.nn3.2.1 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(3,2,1) ,linear.output=FALSE, threshold=0.01,stepmax = 1e6)
pca.pred3.2.1 <- as.data.frame(compute(pca.nn3.2.1, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred3.2.1$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))


pca.nn1.1.2 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(1,1,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e7)
pca.pred1.1.2 <- as.data.frame(compute(pca.nn1.1.2, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred1.1.2$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))

pca.nn2.1.2 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(2,1,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e6)
pca.pred2.1.2 <- as.data.frame(compute(pca.nn2.1.2, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred2.1.2$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))

pca.nn3.1.2 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(3,1,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e6)
pca.pred3.1.2 <- as.data.frame(compute(pca.nn3.1.2, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred3.1.2$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))


pca.nn1.3.3 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(1,3,3) ,linear.output=FALSE, threshold=0.01,stepmax = 1e6)
pca.pred1.3.3 <- as.data.frame(compute(pca.nn1.3.3, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred1.3.3$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))

pca.nn2.3.3 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(2,3,3) ,linear.output=FALSE, threshold=0.01,stepmax = 1e7)
pca.pred2.3.3 <- as.data.frame(compute(pca.nn2.3.3, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred2.3.3$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))

pca.nn3.3.3 <- neuralnet(Y~ ., data = pca.train.df, hidden = c(3,3,3) ,linear.output=FALSE, threshold=0.01,stepmax = 1e7)
pca.pred3.3.3 <- as.data.frame(compute(pca.nn3.3.3, pca.test.df))
confusionMatrix(as.factor(ifelse(pca.pred3.3.3$net.result > 0.5, 1, 0)), as.factor(pca.test.df$Y))



## 2. PCA 안한 데이터
scaled.nn1.2 <- neuralnet(Y~ ., data =scaled.train.df, hidden = c(1,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e7)
scaled.pred1.2 <- compute(scaled.nn1.2, scaled.test.df)
confusionMatrix(as.factor(ifelse(scaled.pred1.2 > 0.5, 1, 0)), as.factor(scaled.test.df))

scaled.nn2.2 <- neuralnet(Y~ ., data = scaled.train.df, hidden = c(2,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e6)
scaled.pred2.2 <- compute(scaled.nn2.2, scaled.test.df)
confusionMatrix(as.factor(ifelse(scaled.pred2.2 > 0.5, 1, 0)), as.factor(scaled.test.df))

scaled.nn3.2 <- neuralnet(Y~ ., data = scaled.train.df, hidden = c(3,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e6)
scaled.pred3.2 <- compute(scaled.nn3.2, scaled.test.df)
confusionMatrix(as.factor(ifelse(scaled.pred3.2 > 0.5, 1, 0)), as.factor(scaled.test.df))


scaled.nn1.2.1 <- neuralnet(Y~ ., data =scaled.train.df, hidden = c(1,2,1) ,linear.output=FALSE, threshold=0.01,stepmax = 1e7)
scaled.pred1.2.1 <- compute(scaled.nn1.2.1, scaled.test.df)
confusionMatrix(as.factor(ifelse(scaled.pred1.2.1 > 0.5, 1, 0)), as.factor(scaled.test.df))

scaled.nn2.2.1 <- neuralnet(Y~ ., data = scaled.train.df, hidden = c(2,2,1) ,linear.output=FALSE, threshold=0.01,stepmax = 1e7)
scaled.pred2.2.1 <- compute(scaled.nn2.2.1, scaled.test.df)
confusionMatrix(as.factor(ifelse(scaled.pred2.2.1 > 0.5, 1, 0)), as.factor(scaled.test.df))

scaled.nn3.2.1 <- neuralnet(Y~ ., data = scaled.train.df, hidden = c(3,2,1) ,linear.output=FALSE, threshold=0.01,stepmax = 1e6)
scaled.pred3.2.1 <- compute(scaled.nn3.2.1, scaled.test.df)
confusionMatrix(as.factor(ifelse(scaled.pred3.2.1 > 0.5, 1, 0)), as.factor(scaled.test.df))


scaled.nn1.1.2 <- neuralnet(Y~ ., data =scaled.train.df, hidden = c(1,1,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e7)
scaled.pred1.1.2 <- compute(scaled.nn1.1.2, scaled.test.df)
confusionMatrix(as.factor(ifelse(scaled.pred1.1.2 > 0.5, 1, 0)), as.factor(scaled.test.df))

scaled.nn2.1.2 <- neuralnet(Y~ ., data = scaled.train.df, hidden = c(2,1,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e7)
scaled.pred2.1.2 <- compute(scaled.nn2.1.2, scaled.test.df)
confusionMatrix(as.factor(ifelse(scaled.pred2.1.2 > 0.5, 1, 0)), as.factor(scaled.test.df))

scaled.nn3.1.2 <- neuralnet(Y~ ., data = scaled.train.df, hidden = c(3,1,2) ,linear.output=FALSE, threshold=0.01,stepmax = 1e7)
scaled.pred3.1.2 <- compute(scaled.nn3.1.2, scaled.test.df)
confusionMatrix(as.factor(ifelse(scaled.pred3.1.2 > 0.5, 1, 0)), as.factor(scaled.test.df))

