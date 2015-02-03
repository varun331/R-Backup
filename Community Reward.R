#open file in R
Invoice= read.csv(file="~/Documents/Communtityreward/InvoiceChargedResults.csv",head=TRUE)
library(splitstackshape)
Invoice1 <- concat.split.multiple(data =Invoice, split.cols = c("Item"), seps = ":")
Invoice1 <- Invoice1[,c(1,2,5,6,3,4)]
Invoice1$Check <- rep(NA,nrow(Invoice1))
Payment= read.csv(file="~/Documents/Communtityreward/InvoicePaidv2Results.csv",head=TRUE)
Payment$Check <- rep("YES",nrow(Payment))
Invoice1$Check <- Payment$Check[match(Invoice1$Internal.ID,Payment$Internal.ID)] 
Invoice2 <- subset(Invoice1,Invoice1$Check=="YES")
write.csv(Invoice2,file="~/Documents/Communtityreward/Invoice-SKU.csv",row.names=FALSE)
