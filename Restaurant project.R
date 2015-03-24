Project1 = read.csv(file="~/Documents/R Projects/train.csv",head=TRUE)
attach(Project1)
plot(revenue,P1,col='1',pch=16)
points(revenue,P2,col='2',pch=16)
points(revenue,P3,col='3',pch=15)
points(revenue,P4,col='4',pch=15)
points(revenue,P5,col='5',pch=15)
points(revenue,P6,col='6',pch=15)
points(revenue,P7,col='7',pch=15)
points(revenue,P8,col='8',pch=15)
points(revenue,P9,col='2',pch=1)
points(revenue,P10,col='3',pch=1)

P=c(1:42)

for (i in 7:42){
points(revenue,Project1[,i],col=P[i],pch=16)}

