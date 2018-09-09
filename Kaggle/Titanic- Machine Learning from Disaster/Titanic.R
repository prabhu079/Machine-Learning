read.csv("D:/RWorkSpace/Data/gender_submission.csv")
test<-read.csv("D:/RWorkSpace/Data/test.csv")
train<-read.csv("D:/RWorkSpace/Data/train.csv")
test$Survived <- rep(0,418)
kaggle<-data.frame(passengerId=test$PassengerId,Survived=test$Survived)
write.csv(kaggle,file = "allSaved.csv",row.names = FALSE)

prop.table(table(train$Sex,train$Survived),1)
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1

train$Child <- 0
train$Child[train$Age < 18] <- 1
aggregate(Survived ~ Child + Sex, data=train, FUN=sum)
aggregate(Survived ~ Child + Sex, data=train, FUN=length)
aggregate(Survived ~ Child + Sex , data=train, FUN=function(x) {sum(x)/length(x)})

train$Fare2<-'30+'
train$Fare2[train$Fare>=20 & train$Fare<30]<-'20-30'
train$Fare2[train$Fare>=10 & train$Fare<20]<-'10-20'
train$Fare2[train$Fare< 10]<- '<10'

aggregate(Survived ~ Fare2+Pclass+Sex,data=train,FUN=function(x) {sum(x)/length(x)})

install.packages('rpart')
install.packages('rpart.plot')
install.packages('rattle')
install.packages('RColorBrewer')

library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
             data=train,
             method="class")
plot(fit)
text(fit)
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class")
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "D:/RWorkSpace/Data/myfirstdtree.csv", row.names = FALSE)





