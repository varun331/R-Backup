CTemployee = read.csv(file="~/Documents/Concur item level detail.csv",head=TRUE)
CTreport = read.csv(file="~/Documents/CTreportv1.csv",head=TRUE)
CTemployee$Email <- rep(NA,19802)
CTemployee$Email <- CTreport$Email[match(CTemployee$RPT_KEY,CTreport$RPT_KEY)]  
CTemployee$FirstName <- rep(NA,19802)
CTemployee$FirstName <- CTreport$FirstName[match(CTemployee$RPT_KEY,CTreport$RPT_KEY)]  
CTemployee$LastName <- rep(NA,19802)
CTemployee$LastName <- CTreport$LastName[match(CTemployee$RPT_KEY,CTreport$RPT_KEY)]  
CTemployee$Date <- CTemployee$TRANSACTION_DATE
library(data.table)
CTemployee$D <- as.character(lapply(strsplit(as.character(CTemployee$Date),split=" "),"[",3))
CTemployee$D1 <- as.character(lapply(strsplit(as.character(CTemployee$Date),split=" "),"[",4))
CTreport1 <- subset(CTemployee,CTemployee$D=="2014"|CTemployee$D1=="2014")

write.csv(CTreport1,file="~/Documents/CTreport4.csv",row.names=FALSE)



CTemployee$Date <- split(CTemployee$Date," ")
CTemployee$Date<-as.Date(as.character(CTemployee$Date,format="%d/%m/%y")) 



CTreport1 <- subset(CTemployee,CTemployee$TRANSACTION_DATE=="2014")
