
Audit$Date.1 = as.Date(Audit$Date,"%m/%d/%y")
Audit$Month <- as.numeric(format(Audit$Date.1,format="%m"))
Audit$Count <- rep(1,length(200388))
Audit.CSR <- subset(Audit,Audit$Department=="Community : Customer Service")
Plot <- data.frame(aggregate(Count~Name,data=Audit.CSR,sum))
Plot$LogCount <- log(Plot$Count)
Count <- subset(Plot,Plot$Count<30 & Plot$Count>20) 
Count$Email <- rep(NA,19)
Count$Email <- Audit$Email.Address[match(Count$Internal.ID,Audit$Internal.ID)]  
scores$score1[match(students$team, scores$team)]
ggplot(Plot,aes(Internal.ID,LogCount,colour=Internal.ID))+geom_point()+scale_y_continuous(breaks=round(seq(min(Audit$Count),max(200),by=1),10))
write.csv(Plot,file="~/Documents/Audit_CSR.csv",row.names=FALSE)
