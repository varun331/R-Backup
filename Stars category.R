Stars = read.csv(file="~/Documents/hygfull.csv",head=TRUE)
library(stringi)
Stars$spectrum1 <- substring(Stars$Spectrum,1,1)
Stars$spectrum1 <- tolower(Stars$spectrum1)
Category <- cbind(c("Low","Low","Int","Int","Int","High","V.High"),c("m","k","g","f","a","b","o"))
Category <- data.frame(Category)
Stars$Category <- Category$X1[match(Stars$spectrum1,Category$X2)] 
write.csv(Stars,file="~/Documents/Stars.csv",row.names=FALSE)

