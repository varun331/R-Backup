library(xlsx)
rw <- read.csv("~/Documents/R Projects/McCormick.csv",header=T)
rw1 <- rw[,c(8,10,12,14,16,18,21,23,27,29,34,36,38,40,42,44,46,48)]
head(train)
library(ggplot2)
library(GGally)
--------------------------------------

ggpairs(train1,columns=c(10,32:36))

# train1$Brand1 <- factor(train1$Brand)
# qplot(Campaign.Name,Page.Likes,data=train1)
--------------------------------------------l
#  convert values to numric and remove NA


dat <- rw1[,c(1:18)]
dat <- na.omit(dat)
for (i in 1:18){dat[,c(i)] <- as.numeric(dat[,c(i)])}
# dat$CPA <- as.numeric(dat$CPA)

------------------------------------
#wss <- (nrow(dat)-1)*sum(apply(dat,2,var))
#for (i in 2:10) wss[i] <- sum(kmeans(dat,
#                                     centers=i)$withinss)
#plot(1:10, wss, type="b", xlab="Number of Clusters",
#     ylab="Within groups sum of squares")



-----------------------------------
#library(cluster)
#library(fpc)
#set.seed(1)
#clus <- kmeans(dat,centers=10)
#clusplot(dat,clus$cluster,color=T,shade=T,labels=2,lines=0)
#tb <- table(train1$ER,clus$cluster)
#qplot(dat[,10],dat[,35])+geom_point(aes(color=factor(clus$cluster)))
#plot(train$Placement.of.Question,train$ER)

#explore <- subset(train1,train1$Amount.Spent..USD.>2000)

-------------------------------------
set.seed(2)
sub <- sample(nrow(dat),floor(nrow(dat)*.5))
train <- dat[sub,]
test <- dat[-sub,]
rm(i,sub)
--------------------------------------
library(splines)
library(foreach)
library(gam)
gam.m1=gam(train[,10]~s(train[,5])+(train[,4])+(train[,9])+s(train[,11])+(train[,12])+(train[,13])
           +(train[,14])+(train[,15])+(train[,16])+s(train[,17])+(train[,18]),data=train)
summary(gam.m1)

gam.m2=gam(train[,10]~(train[,4])+s(train[,17]),data=train)
summary(gam.m2)
predict = predict(gam.m1,test)
predict1=predict(gam.m2,test)
results = data.frame(test[,10],predict)
library(Metrics)
mse(test[,10],predict)
mse(test[,10],predict1)
//gam.m1=gam(ER~s(Coded.Placement)+s(Coded.Campaign.Type),data=dat)
plot.gam(gam.m1,se=T,col="blue")


-----------------------------------------
  
library(randomForest)  
set.seed(1)
bag.train=randomForest(ER~.,data=train,importance=TRUE)
bag.train
yhat.bag=predict(bag.train,newdata = test)
mse(test[,10],yhat.bag)
varImpPlot(bag.train)

---------------------------------------------
library(survival)
library(lattice)
library(parallel)
library(gbm)
set.seed(1)
boost.train=gbm(ER~.,data=train,distribution = "gaussian",
                n.trees=5000)
summary(boost.train)
yhat.boost=predict(boost.train,newdata = test,n.trees = 5000)
(yhat.boost)
mse(test[,10],yhat.boost)

---------------------------------------------
install.packages("shiny")
library(shiny)
runApp("App-1",display.mode = "showcase")
