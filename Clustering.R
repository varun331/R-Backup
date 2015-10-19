library(xlsx)
rw <- read.csv("~/Documents/R Projects/McCormick.csv",header=T)
rw1 <- rw[,c(8,10,12,14,16,18,21,23,27,29,30,31,34,36,38,40,42,44,46,48,49:64)]
head(train)
library(ggplot2)
library(GGally)
--------------------------------------

ggpairs(train1,columns=c(10,32:36))

//train1$Brand1 <- factor(train1$Brand)
//qplot(Campaign.Name,Page.Likes,data=train1)
--------------------------------------------l

library(cluster)
library(fpc)
dat <- rw1[,c(1:36)]
dat <- na.omit(dat)
for (i in 1:36){dat[,c(i)] <- as.numeric(dat[,c(i)])}
//dat$CPA <- as.numeric(dat$CPA)

------------------------------------
wss <- (nrow(dat)-1)*sum(apply(dat,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(dat,
                                     centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")



-----------------------------------
set.seed(1)
clus <- kmeans(dat,centers=10)
clusplot(dat,clus$cluster,color=T,shade=T,labels=2,lines=0)
tb <- table(train1$ER,clus$cluster)
qplot(dat[,10],dat[,35])+geom_point(aes(color=factor(clus$cluster)))
plot(train$Placement.of.Question,train$ER)

explore <- subset(train1,train1$Amount.Spent..USD.>2000)

-------------------------------------
set.seed(2)
sub <- sample(nrow(dat),floor(nrow(dat)*.5))
train <- dat[sub,]
test <- dat[-sub,]

--------------------------------------
library(splines)
library(foreach)
library(gam)
gam.m1=gam(train[,10]~s(train[,5])+(train[,4])+(train[,9])+(train[,11])+s(train[,12])+s(train[,13])
           +(train[,14])+(train[,15])+(train[,16])+(train[,17])+(train[,18])+(train[,19])+(train[,20])
           +s(train[,21])+s(train[,22])+s(train[,23])+s(train[,24])+s(train[,25])+s(train[,26])+s(train[,27])
           +s(train[,28])+s(train[,29])+s(train[,30])+s(train[,31])+s(train[,32])+s(train[,33])+s(train[,34])
           +s(train[,35])+s(train[,36]),data=train)
summary(gam.m1)

gam.m2=gam(train[,10]~s(train[,12])+train[,16]+train[,19],data=train)
summary(gam.m2)
predict = predict(gam.m2,test)
predict1=predict(gam.m2,train)
results = data.frame(test[,10],predict)
library(Metrics)
mse(test[,10],predict)

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
library(gbm)
set.seed(1)
boost.train=gbm(ER~.,data=train,distribution = "gaussian",
                n.trees=5000)
summary(boost.train)
yhat.boost=predict(boost.train,newdata = test,n.trees = 5000)
mse(test[,10],yhat.boost)

---------------------------------------------
install.packages("shiny")
library(shiny)
runApp("App-1",display.mode = "showcase")
