library(caret)
library(descr)
library(MASS)
library(psych)
library(corrplot)
library(dplyr )
library(HSAUR)
library(caret)


scaled.train.df <- read.csv("scaled.train.df.csv")
scaled.test.df <- read.csv("scaled.test.df.csv")

#########SVM e1071 패키지 사용해서 돌린것
library(e1071)
svm1 <- svm(Y ~ ., data = scaled.train.df)
a <- confusionMatrix(factor(ifelse(scaled.svm.pred > 0.5, 1, 0)), factor(scaled.test.df$Y), positive="1")
print(a$overall[1])
#accuarcy 0.7113



##########SVM kernlab 패키지 사용해서 돌린것
library(kernlab)
svm2 <- ksvm(Y ~., data=scaled.train.df, kernel="rbfdot", kpar="automatic", prob.model=TRUE)

scaled.svm2.pred <- predict(svm2, scaled.test.df)
#predict(svm2, scaled.test.df, type="probabilities")
ab <- confusionMatrix(factor(ifelse(scaled.svm2.pred > 0.5, 1, 0)), factor(scaled.test.df$Y), positive="1")
ab
#accuracy 0.7126



###########tune() 사용하여 파라미터 최적화
results <- tune(svm, Y ~ ., data = scaled.train.df, gamma=2^(-1:1), cost=2^(2:4))
#tune()의반환 값은 객체
#객체 속성 보려면 attributes()

#다음은 최적 파라미터 값을 빼내는 코드
attributes(results)
results$best.parameters

results$best.model
#튜닝결과 kernal = radial, cost = 4,8,16, gamma = 0.5, 1, 2, epsilon = 0.1
  
#튜닝결과대로 gamma와 cost 바꿔가며 실험   
svm5 <- svm(Y ~., data=scaled.train.df, type=NULL, kernel="radial", gamma=1, kpar="automatic", cost=1, prob.model=TRUE)

scaled.svm.pred <- predict(svm5, scaled.test.df)
abc <- confusionMatrix(factor(ifelse(scaled.svm.pred > 0.5, 1, 0)), factor(scaled.test.df$Y), positive="1")
abc
#accuarcy 0.6354 0.599 0.6053 ...
  

#kernal, gamma, cost를 바꿔가면서 실험 
svm4 <- svm(x, y=NULL, scaled=False, type=NULL, kernel="radial", gamma=if(is.vector(x)) 1 else 1/ncol(x), cost=1)


svm2 <- ksvm(Y ~., data=scaled.train.df, kernel="rbfdot", kpar="automatic", prob.model=TRUE)

svm2 <- ksvm(Y ~., data=scaled.train.df, kernel="polydot", kpar="automatic", prob.model=TRUE)

svm2 <- ksvm(Y ~., data=scaled.train.df, kernel="vanilladot", kpar="automatic", prob.model=TRUE)

svm2 <- ksvm(Y ~., data=scaled.train.df, kernel="tandot", kpar="automatic", prob.model=TRUE) 