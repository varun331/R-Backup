Project1 = read.csv(file="~/Documents/R Projects/train.csv",head=TRUE)
test=read.csv(file="~/Documents/R Projects/test.csv",head=TRUE)
train <- Project1
attach(train)
plot(revenue,P1,col='1',pch=16)
points(revenue,P2,col='2',pch=16)
points(revenue,P3,col='3',pch=15)
points(revenue,P4,col='4',pch=15)
points(revenue,P5,col='5',pch=15)
points(revenue,P6,col='6',pch=15)

P=c(1:49)

for (i in 6:49){
plot(revenue,train[,i],col=P[i],pch=16)}

# Change the train date data into day/ month/ year
library(lubridate)
train$day<-as.factor(day(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$month<-as.factor(month(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$year<-as.factor(year(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))

#Change the test data into day/month/ year
test$day<-as.factor(day(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))
test$month<-as.factor(month(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))
test$year<-as.factor(year(as.POSIXlt(test$Open.Date, format="%m/%d/%Y")))

# change the train city, city group and type as numeric
train$City1 <- as.numeric(train$City)
train$City.Group1 <- as.numeric(train$City.Group)
train$Type1 <- as.numeric(train$Type)

# change the test city, city group and type as numeric
test$City1 <- as.numeric(test$City)
test$City.Group1 <- as.numeric(test$City.Group)
test$Type1 <- as.numeric(test$Type)

# subset train data by year
train.1 = subset(train, year=='1999'|year=='2000'|year=='2013'|year=='2012')
train.1 = train.1[,c(1,6:42,44:49,43)]
train.2 = train.1[,c(2:38)]
set.seed(600)
sub<- sample(nrow(train),floor(nrow(train)*.5))
train.1 = train[sub,]

# subset test data by year
test.2=subset(test,year=='1995'|year=='2001'|year=='2003'|Type1=='4')
test.1=subset(test,year!='1995'& year!='2001'& year!='2003'&Type1!='4')
test.1=test.1[,c("P2","P3","P4","P5","P11","P21","P22","P27","P29","P30","P31","P32","year","City.Group1","Type1")]


train.1$day <- NULL
train.1$City1<- NULL
library(Correlplot)
library(calibrate)
library(MASS)
library(ellipse)
library(mgcv)
library(gam)
library(splines)
train.2 <- as.numeric(train.1[,c(1:4)]
# fit a coorelation matrix to identify variable that have least cooraeltion
ctab <- cor(train.1)

# fit a GAM model


gam1=lm(revenue~P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15
        +P16+P17+P18+P19+P20+P21+P22+P23+P24+P25+P26+P27+P28+P29
        +P30+P31+P32+P33+P34+P35+P36+P37+year+City1+City.Group1
        +Type1,data=train.1)

gam1=lm(revenue~ns(P1)+ns(P2)+ns(P3)+ns(P4)+ns(P5)+ns(P6)+ns(P7)+ns(P8)
        +ns(P9)+ns(P10)+ns(P11)+ns(P12)+ns(P13)+ns(P14)+ns(P15)
        +ns(P16)+ns(P17)+ns(P18)+ns(P19)+ns(P20)+ns(P21)+ns(P22)
        +ns(P23)+ns(P24)+ns(P25)+ns(P26)+ns(P27)+ns(P28)+ns(P29)
        +ns(P30)+ns(P31)+ns(P32)+ns(P33)+ns(P34)+ns(P35)+ns(P36)
        +ns(P37)+year++City.Group1
        +Type1,data=train.1)



gam.1 = gam(revenue~s(P1),data=train.1)
gam.2 = gam(revenue~s(P1)+s(P2),data=train.1)
gam.3 = gam(revenue~s(P1)+s(P2)+s(P3),data=train.1)
gam.4 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4),data=train.1)           
gam.5 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5),data=train.1)             
gam.6 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6),data=train.1)             
gam.7 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7),data=train.1)
gam.8 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8),data=train.1)
gam.9 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9),data=train.1)
gam.10 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10),data=train.1)
gam.11 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11),data=train.1)
gam.12 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11) +s(P12),data=train.1)
gam.13 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13),data=train.1)
gam.14 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14),data=train.1)
gam.15 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15),data=train.1)
gam.16 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16),data=train.1)
gam.17 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17),
             data=train.1)
gam.18 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18),data=train.1)
gam.19 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19),data=train.1)
gam.20 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20),data=train.1)
gam.21 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21),data=train.1)
gam.22 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22),data=train.1)
gam.23 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23),data=train.1)
gam.24 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24),data=train.1)
gam.25 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24)+s(P25),data=train.1)
gam.26 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24)+s(P25)+s(P26),data=train.1)
gam.27 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24)+s(P25)+s(P26)+s(P27),data=train.1)
gam.28 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24)+s(P25)+s(P26)+s(P27)+s(P28),data=train.1)
gam.29 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24)+s(P25)+s(P26)+s(P27)+s(P28)+s(P29),data=train.1)
gam.30 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24)+s(P25)+s(P26)+s(P27)+s(P28)+s(P29)+s(P30),data=train.1)
gam.31 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24)+s(P25)+s(P26)+s(P27)+s(P28)+s(P29)+s(P30)+s(P31),data=train.1)
gam.32 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24)+s(P25)+s(P26)+s(P27)+s(P28)+s(P29)+s(P30)+s(P31)+s(P32),data=train.1)
gam.33 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24)+s(P25)+s(P26)+s(P27)+s(P28)+s(P29)+s(P30)+s(P31)+s(P32)
             +s(P33),data=train.1)
gam.34 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24)+s(P25)+s(P26)+s(P27)+s(P28)+s(P29)+s(P30)+s(P31)+s(P32)
             +s(P33)+s(P34),data=train.1)
gam.35 = gam(revenue~s(P1)+s(P2)+s(P3)+s(P4)+s(P5)+s(P6)+s(P7)+s(P8)+s(P9)+s(P10)+s(P11)+s(P12)+s(P13)+s(P14)+s(P15)+s(P16)+s(P17)
             +s(P18)+s(P19)+s(P20)+s(P21)+s(P22)+s(P23)+s(P24)+s(P25)+s(P26)+s(P27)+s(P28)+s(P29)+s(P30)+s(P31)+s(P32)
             +s(P33)+s(P34)+s(P35),data=train.1)

gam.36 = gam(revenue~s(P2,4)+s(P5,4)+s(P11,4)+s(P21,4)+s(P22,4)+s(P27,4)+s(P29,4)+s(P30,4)+s(P31,4)+s(P32,4)
             +year+City.Group1+Type1,data=train.1)
gam.37 = gam(revenue~s(P2,5)+s(P5,5)+s(P11,5)+s(P21,7)+s(P22,5)+s(P27,4)+s(P29,4)+s(P30,6)+s(P31,5)+s(P32,6)
             +year+factor(City.Group1)+factor(Type1),data=train.1)
plot.gam(gam.37,se=T,col="red")
summary(gam.37)

gam.38 = gam(revenue~year+factor(City.Group1)+factor(Type1)+s(P2,5)+s(P5,5)+s(P11,5)+s(P21,7)+s(P22,5)+s(P27,4)+s(P29,4)+s(P30,6)+s(P31,5)+s(P32,6)
             ,data=train.1)

gam.39 = gam(revenue~year+factor(City.Group1)+factor(Type1)+s(P2,5)+s(P3,5)+s(P5,5)+s(P11,5)+s(P21,7)+s(P22,5)+s(P27,4)+s(P29,4)+s(P30,6)+s(P31,5)+s(P32,6)
             ,data=train.1)

gam.40 = gam(revenue~s(P2,5)+s(P3,5)+s(P4,5)+s(P5,5)+s(P11,5)+s(P21,7)+s(P22,5)+s(P27,4)+s(P29,4)+s(P30,6)+s(P31,5)+s(P32,6)
             +year+factor(City.Group1)+factor(Type1),data=train,family="inverse.gaussian")

gam.41 = gam(revenue~s(P2,5)+s(P3,5)+s(P4,5)+s(P5,5)+s(P11,5)+s(P21,7)+s(P22,5)+s(P27,4)+s(P29,4)+s(P30,6)+s(P31,5)+s(P32,6)
             +factor(City.Group1),data=train,family="inverse.gaussian")
gam.42 = gam(revenue~s(P2,5)+s(P3,5)+s(P4,5)+s(P5,5)+s(P11,5)+s(P21,7)+s(P22,5)+s(P27,4)+s(P29,4)+s(P30,6)+s(P31,5)+s(P32,6)
             +factor(City.Group1),data=train)

gam.43 = gam(revenue~s(P2,5)+s(P3,5)+s(P4,5)+s(P5,5)+s(P11,5)+s(P21,7)+s(P22,5)+s(P27,4)+s(P29,4)+s(P30,6)+s(P31,5)+s(P32,6)
             +year+factor(City.Group1)+factor(Type1),data=train)

#Predict using the GAM.37 model
preds=predict(gam.37,newdata=test.1)
preds=data.frame(preds)
preds$Id=test.1$Id
preds=preds[,c(2,1)]
MSE = mean((test.1$revenue-preds$preds)^2)
MSE
# MSE 2.491997e+12
plot(test.1$revenue~test.1$Id,col="2")
points(preds$preds~preds$Id,col='3',pch=16)

#Predict using the GAM.38 model
preds=predict(gam.38,newdata=test.1)
preds=data.frame(preds)
preds$Id=test.1$Id
preds=preds[,c(2,1)]
MSE = mean((test.1$revenue-preds$preds)^2)
MSE
# MSE 2.491997e+12

#Predict using the GAM.39 model
preds=predict(gam.39,newdata=test.1)
preds=data.frame(preds)
preds$Id=test.1$Id
preds=preds[,c(2,1)]
MSE = mean((test.1$revenue-preds$preds)^2)
MSE
# MSE 2.456429e+12

#Predict using the GAM.40 model
preds.train=predict(gam.40,newdata=train.1)
preds.train=data.frame(preds.train)
(preds$Id=test.1$Id
preds=preds[,c(2,1)]
MSE = mean((test.1$revenue-preds$preds)^2)
MSE
# MSE 2.445439e+12

#Predict using the GAM.40 model on test data
preds=predict(gam.40,newdata=test.1,type="response")
preds=data.frame(preds)
preds$Id=test.1$Id

#Subset all Na from Non Nas from preds
preds$Check=ifelse(preds$preds>0,1,NA)
preds1.2=preds[is.na(preds$Check),]
preds1.2$preds <- NULL
preds1.2$Check <- NULL
NA.test <- test.1[(test.1$Id %in% preds1.2$Id),]
preds1.1 <- subset(preds,!(Id%in%preds1.2$Id))
preds1.1$Check <- NULL
colnames(preds1.1) <- c("Prediction","Id")

#Predict using the GAM 43 model on the NA.test data
preds1.2=predict(gam.43,newdata=NA.test,type="response")
preds1.2=data.frame(preds1.2)
preds1.2$Id=NA.test$Id
colnames(preds1.2) <- c("Prediction","Id")
datamerge.2=merge(preds1.1,preds1.2,by=c("Prediction","Id"),all=T)


#Predict using the GAM.41 model on test data
preds.1=predict(gam.41,newdata=test.2,type="response")
preds.1=data.frame(preds.1)
preds.1$Id=test.2$Id
colnames(preds.1.1) <- c("Prediction","Id","Check")
preds.1.1$Check <- NULL
plot(preds.1$preds~preds.1$Id)

# MSE 2.445439e+12

#subset all the NA and non NAs from preds.1 data
preds.1$check=ifelse(preds.1$preds>0,1,NA)
NA.1=subset(preds.1,is.na(preds.1$check))
NA.1$preds <- NULL
NA.1$check <- NULL
NA.test <- test.2[(test.2$Id %in% NA.1$Id),]
preds.1.1=subset(preds.1,preds.1$check==1)

#Predict using the GAM.42 model on NA.test
preds1.2=predict(gam.42,newdata=NA.test,type="response")
preds1.2=data.frame(preds1.2)
preds1.2$Id=NA.test$Id
colnames(preds1.2) <- c("Prediction","Id")
datamerge.1=merge(preds.1.1,preds1.2,by=c("Prediction","Id"),all=T)


#data submittion
submission=merge(datamerge.1,datamerge.2, by=c("Prediction","Id"),all=T)
submission=submission[,c(2:1)]
submission$Prediction=round(submission$Prediction,digits=0)
write.csv(submission,"submission.csv",row.names=FALSE,quote=FALSE)
