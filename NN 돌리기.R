for.nn <- read.csv("C://Users//inmyr//OneDrive//바탕 화면//BI_팀플//scaled.dummie.card.df.csv")
View(for.nn)
dim(for.nn)

for.nn$normal <- for.nn$Y==0
for.nn$default <- for.nn$Y==1

str(for.nn)
for.nn<-for.nn[,-15]

nn <- neuralnet(normal + default ~ ., data = for.nn, linear.output = F, hidden = 3)
nn$weights
prediction(nn)
plot(nn, rep="best")
