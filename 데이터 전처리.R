library(readxl)
library(caret)
library(descr)
library(MASS)
library(psych)
library(corrplot)
library(dplyr)

card.df<-readxl::read_excel(path="default of credit card clients.xls", sheet="Data", col_names=TRUE)
card.df <- card.df[,-1]
names(card.df)[24] <- c("Y")

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
freq <- table(card.df$EDUCATION) 
freq

card.df$EDUCATION[card.df$EDUCATION==0]<-NA
card.df$EDUCATION[card.df$EDUCATION==5]<-NA
card.df$EDUCATION[card.df$EDUCATION3==6]<-NA

## X4 missing value -> NA 값으로 대체

freq <- table(card.df$MARRIAGE) 
freq

card.df$MARRIAGE[card.df$MARRIAGE=="0"]<-NA

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

scaled.card.df <- cbind(scaled.card.df,card.dum)
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


write.csv(pca.train.df, file="pca.train.df.csv",row.names=FALSE)
write.csv(pca.test.df, file="pca.test.df.csv",row.names=FALSE)


write.csv(scaled.train.df, file="scaled.train.df.csv",row.names=FALSE)
write.csv(scaled.test.df, file="scaled.test.df.csv",row.names=FALSE)

