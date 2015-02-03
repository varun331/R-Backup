train = read.csv(file="~/Documents/Kaggle competition/Citibike/train.csv",head=TRUE)
#split the datetime column with date and time
train$date <- as.Date(train$datetime)
train$time <- as.numeric(format(as.POSIXct(train$datetime),format="%H"))
train$day <- weekdays(as.Date(train$datetime))
train$month <- as.numeric(format(as.POSIXct(train$datetime),format="%m"))
train$year <- as.numeric(format(as.POSIXct(train$datetime),format="%y"))

#exploring data using GGplot

library(ggplot2)
library(rattle)
library(plyr)

ggplot(train, aes(time,registered))+geom_point(color="chartreuse4")+
  facet_wrap(~year, ncol=2)

ggplot(train, aes(temp,registered,colour=year))+geom_line()

ggplot(train, aes(time,registered,colour="chartreuse4"))+
  geom_point(temp)+facet_wrap(~year,ncol=2)
# check for colrelation between temp and atem
cor(train$temp,train$atemp)

#choosing variable using forward stepwise selection
regfit.fwd=regsubsets(count~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+time+day+month,data=train,nvmax=13,method="forward")

#choosing variable using backward stepwise selection
regfit.bwd=regsubsets(count~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+time+day+month,data=train,nvmax=13,method="backward")

#creating test and training set

set.seed(1)
train.1=sample(1:nrow(train),nrow(train)/2)
train.1.1=data.frame(train[train.1,])
test.1=(-train.1)
test.1.1=data.frame(train[test.1,])
#additional code
train.1=sample(c(TRUE,FALSE),nrow(train),rep=TRUE)
test.1=(!train.1)
train.1=dataframe(train.1)


#cross validation selection
library(leaps)
regfit.best=regsubsets(count~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+time+day+month,data=train.1.1,nvmax=13)
test.mat=model.matrix(count~season+holiday+workingday+weather+temp+atemp+humidity+windspeed+time+day+month,data=test.1.1)
val.errors=rep(NA,13)
for(i in 1:13){
  coefi=coef(regfit.best,id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((train$count[test.1]-pred)^2)
}
which.min(val.errors)
coef(regfit.best,10)

# create a regression model with 10 variable
library(MASS)
library(ggplot2)
library(foreign)
model1 <- 
  glm.nb(count~season+workingday+as.factor(weather)+temp+humidity+time+as.factor(day)+month+year,data=train.1.1)
plot(model1$fitted.values,model1$residuals)


#plot predicted vs observed
res <- stack(data.frame(observed=train.1.1$count,Predicted=fitted(model1)))
res <- cbind(res,x=rep(train.1.1$datetime))
require("lattice")
xyplot(values~x,data=res,group = ind, auto.key = TRUE)
val.errors2=sum((train.1.1$count-model1$fitted.values)^2)/nrow(train.1.1)
val.errors2

#plot predicted vs oberserved for test.1.1
res1 <- stack(data.frame(observed=test.1.1$count,Predicted=predict))
res1 <- cbind(res1,x=rep(test.1.1$))



predict <- predict(model1,test.1.1,type="response")
val.errors3=mean((test.1.1$count-predict)^2)
val.errors3

# user model to predict actaul test file
test = read.csv(file="~/Documents/Kaggle competition/Citibike/test.csv",head=TRUE)
test$time <- as.numeric(format(as.POSIXct(test$datetime),format="%H"))
test$day <- weekdays(as.Date(test$datetime))
test$month <- as.numeric(format(as.POSIXct(test$datetime),format="%m"))
test$year <- as.numeric(format(as.POSIXct(test$datetime),format="%y"))
predict <- predict(model1,test,type="response")
#submit
submit <- data.frame(datetime=test$datetime,count=as.integer(predict))
write.csv(submit,file="~/Documents/submit.csv",row.names=FALSE)
