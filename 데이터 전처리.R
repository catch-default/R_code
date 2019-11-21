library(readxl)
library(caret)
library(descr)
library(MASS)
library(psych)
library(corrplot)

card.df<-readxl::read_excel(path="C://Users//inmyr//OneDrive//바탕 화면//BI_팀플//default of credit card clients.xls", sheet="Data", col_names=TRUE)
card.df <- card.df[-1,]
card.df <- card.df[,-1]
attach(card.df)
View(card.df)

# char 데이터 타입 numeric으로 바꾸기
str(card.df)

Variables <- colnames(card.df)

for(i in c(1,5,12,13,14,15,16,17,18,19,20,21,22,23)){
  a <- as.numeric(get(Variables[i]))
  card.df[,i] <- a
}

for(i in c(2,3,4,6,7,8,9,10,11,24)){
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

card.df$X4[card.df$X4==0]<-NA

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

View(card.df)

# describe nomal.sp.df and default.df
describe(normal.sp.df)
describe(default.df)


## boxplot 그리기

# X1
ggplot(data = card.df, aes(y = card.df$X1,x= card.df$Y)) + geom_boxplot(fill = "orange")

#X2
card.df %>% 
  mutate(X2 = factor(X2)) %>% 
  ggplot(aes(X2, fill = Y)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  scale_fill_brewer(palette = "Set1")

#X3
card.df %>% 
  mutate(X3 = factor(X3)) %>% 
  ggplot(aes(X3, fill = Y)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  scale_fill_brewer(palette = "Set1")

# X4
card.df %>% 
  mutate(X4 = factor(X4)) %>% 
  ggplot(aes(X4, fill = Y)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  scale_fill_brewer(palette = "Set1")

# X6
card.df %>% 
  mutate(X6 = factor(X6)) %>% 
  ggplot(aes(X6, fill = Y)) +
  geom_bar(position = "dodge") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.6)) +
  scale_fill_brewer(palette = "Set1")


## correlation plot 그리기

# Amount of bill statement (NT dollar) correlation plot
card.df.cor1 = cor(card.df[,c(12:17)], method = c("spearman"))
corrplot(card.df.cor1)

# Amount of previous payment (NT dollar) correlation plot
card.df.cor2 = cor(card.df[,c(18:23)], method = c("spearman"))
corrplot(card.df.cor2)


# 데이터 전처리 : numeric variables scaling 하기




prep <- preProcess(card.df, "range")
scaled.card.df <- predict(prep, card.df)
View(scaled.card.df)

write.csv(card.df, file="card.df.csv",row.names=FALSE)
write.csv(scaled.card.df, file="scaled.card.df.csv",row.names=FALSE)

dim(card.df)
dim(scaled.card.df)
