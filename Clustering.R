library(xlsx)
train <- read.csv("~/Documents/R Projects/McCormick.csv",header=T)
train1 <- train[,c(8,10,12,14,16,18,21,23,27,29,30,31,34,36,38,40,42,44,46,48,49:64)]
head(train)
library(ggplot2)
library(GGally)
--------------------------------------

ggpairs(train1,columns=c(10,32:36))

//train1$Brand1 <- factor(train1$Brand)
//qplot(Campaign.Name,Page.Likes,data=train1)
--------------------------------------------l

ibrary(cluster)
library(fpc)
dat <- train1[,c(1:36)]
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
gam.m1=gam(dat[,10]~s(dat[,5])+(dat[,4])+(dat[,9])+(dat[,11])+s(dat[,12])+s(dat[,13])
           +(dat[,14])+(dat[,15])+(dat[,16])+(dat[,17])+(dat[,18])+(dat[,19])+(dat[,20])
           +s(dat[,21])+s(dat[,22])+s(dat[,23])+s(dat[,24])+s(dat[,25])+s(dat[,26])+s(dat[,27])
           +s(dat[,28])+s(dat[,29])+s(dat[,30])+s(dat[,31])+s(dat[,32])+s(dat[,33])+s(dat[,34])
           +s(dat[,35])+s(dat[,36]),data=dat)
summary(gam.m1)

//gam.m1=gam(ER~s(Coded.Placement)+s(Coded.Campaign.Type),data=dat)
plot.gam(gam.m1,se=T,col="blue")

