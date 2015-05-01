Project1 = read.csv(file="~/Documents/R Projects/train.csv",head=TRUE)
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

# Change the date data into day/ month/ year
library(lubridate)
train$day<-as.factor(day(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$month<-as.factor(month(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))
train$year<-as.factor(year(as.POSIXlt(train$Open.Date, format="%m/%d/%Y")))

# change the city, city group and type as numeric
train$City1 <- as.numeric(train$City)
train$City.Group1 <- as.numeric(train$City.Group)
train$Type1 <- as.numeric(train$Type)

# subset data by year
train.1 = subset(train, year=='1999'|year=='2000'|year=='2013'|year=='2012')
train.1 = train.1[,c(1,6:42,44:49,43)]

# fit a GAM model
library(gam)
gam1=lm(revenue~P1+P2+P3+P4+P5+P6+P7+P8+P9+P10+P11+P12+P13+P14+P15
        +P16+P17+P18+P19+P20+P21+P22+P23+P24+P25+P26+P27+P28+P29
        +P30+P31+P32+P33+P34+P35+P36+P37+year+City1+City.Group1
        +Type1,data=train.1)

gam1=lm(revenue~ns(P1)+ns(P2)+ns(P3)+ns(P4)+ns(P5)+ns(P6)+ns(P7)+ns(P8)
        +ns(P9)+ns(P10)+ns(P11)+ns(P12)+ns(P13)+ns(P14)+ns(P15)
        +ns(P16)+ns(P17)+ns(P18)+ns(P19)+ns(P20)+ns(P21)+ns(P22)
        +ns(P23)+ns(P24)+ns(P25)+ns(P26)+ns(P27)+ns(P28)+ns(P29)
        +ns(P30)+ns(P31)+ns(P32)+ns(P33)+ns(P34)+ns(P35)+ns(P36)
        +ns(P37)+year+City1+City.Group1
        +Type1,data=train.1)

gam.m3 = gam(revenue~s(P1,4)+s(P2,4)+s(P3,4)+s(P4,4)+s(P5,4)+s(P6,4)+s(P7,4)+s(P8,4)
          +s(P9,4)+s(P10,4)+s(P11,4)+s(P12,4)+s(P13,4)+s(P14,4)+s(P15,4)
          +s(P16,4)+s(P17,4)+s(P18,4)+s(P19,4)+s(P20,4)+s(P21,4)+s(P22,4)
          +s(P23,4)+s(P24,4)+s(P25,4)+s(P26,4)+s(P27,4)+s(P28,4)+s(P29,4)
          +s(P30,4)+s(P31,4)+s(P32,4)+s(P33,4)+s(P34,4)+s(P35,4)+s(P36,4)
          +s(P37,4)+year+City1+City.Group1
          +Type1,data=train.1)


coef(summary(gam1))