library(ff)
library(ffbase)
df <- read.csv.ffdf(file=gzfile("~/Documents/Kaggle competition/Ad analysis/train.gz"),VERBOSE=TRUE,first.rows = 2000000,next.rows = 4000000,header=T)
save.ffdf(df,dir='~/Documents/Kaggle competition/Ad analysis/train.data')
load.ffdf(dir='~/Documents/Kaggle competition/Ad analysis/train.data')


s <- sample(nrow(df), nrow(df)*0.01)
df.in.mem <- df[s, ]
save.ffdf(df.in.mem,dir=='~/Documents/Kaggle competition/Ad analysis/sample.data')
write.csv(df.in.mem,file="~/Documents/Kaggle competition/Ad analysis/sample.csv",row.names=FALSE)
train <- read.csv(file="~/Documents/Kaggle competition/Ad analysis/sample.csv", head=T)

save.image()
load(".RData")
rm(list=ls())

#exploring sample data
train$year <- as.Date((train$hour,format="%y/%m/%d/%h"))
library(ggplot2)
ggplot(train, aes(x=site_category,fill=click)) + geom_histogram(binwidth=1) 
ggplot(train, aes(x=site_category,y=click)) + geom_point() + geom_rug(col="darkred",alpha=.1)
