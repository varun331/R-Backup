
#Open file in R
PO= read.csv(file="~/Documents/StandardcostingSKUs/KPKRecentPurchaseOrdersbySKUv1Results984.csv",head=TRUE)
library(splitstackshape)

#Split PO Item by : and rearrange the column
PO1 <- concat.split.multiple(data =PO, split.cols = c("Item"), seps = ":")
PO2 <- PO1[,c(1,3,4,5,6,7,15,9,10,11,12,13)]

#Subset date by Active
PO3 <- subset(PO2,PO2$Item.Phase.NEW=="Active")


#Subset data by specific SKU
Active= read.csv(file="~/Documents/StandardcostingSKUs/Active DomesticSKUs.csv",head=TRUE)
Active1 <- data.frame(Active$SKU)
PO4 <- subset(PO3,PO3$Item_2 %in% Active1$Active.SKU)
write.csv(PO4,file="~/Documents/StandardcostingSKUs/PO4.csv",row.names=FALSE)
