UCincome = read.csv(file="~/Documents/UncategorizedIncome.csv",head=TRUE)
income = read.csv(file="~/Documents/Income.csv",head=TRUE)
UCincome$Match <- rep(NA,138582)
UCincome$Match <- income$Reconcile[match(UCincome$Number,income$Transaction.Number)]
write.csv(UCincome,file="~/Documents/UCincomev1.csv",row.names=FALSE)


#Transfer order
IR = read.csv(file="~/Documents/ItemReceipt.csv",head=TRUE)
TO = read.csv(file="~/Documents/Transferorder.csv",head=TRUE)
IR$Destination <- rep(NA,280)
IR$Destination <- TO$Destination[match(IR$Number1,TO$Number)]
write.csv(IR,file="~/Documents/IR.csv",row.names=FALSE)

#Tranfer order with Bills
TOWB = read.csv(file="~/Documents/TOWB.csv",head=TRUE)
Bills = read.csv(file="~/Documents/BillsLinkedtoTO.csv",head=TRUE)
TOWB$Billnumber = rep(NA,192)
TOWB$Billnumber <- Bills$Name[match(TOWB$HBL.HAWB,Bills$Number)]
write.csv(TOWB,file="~/Documents/TOWB1.csv",row.names=FALSE)


#Item receipt with Vendor Bill
IR = read.csv(file="~/Documents/IR.csv",head=TRUE)
TOWB = read.csv(file="~/Documents/TOWB4.csv",head=TRUE)
IR$NSBILL =rep(NA,280)
IR$NSBILL = TOWB$NS.BILL.NOS[match(IR$Number1,TOWB$Number)]
IR$AMOUNT = rep(NA,280)
IR$AMOUNT = TOWB$NS.BILL.AMOUNT[match(IR$Number1,TOWB$Number)] 
IR$VENOR = rep(NA,280)
IR$VENOR = TOWB$NS.BILL.VENDOR[match(IR$Number1,TOWB$Number)]
write.csv(IR,file="~/Documents/TOWB5.csv",row.names=FALSE)


#Item receipt with Duty
TOWB5 = read.csv(file="~/Documents/TOWB5.csv",head=TRUE)
DUTY = read.csv(file="~/Documents/duty.csv",head=TRUE)
TOWB5$DutyAmt = rep(NA,280)
TOWB5$DutyAmt = DUTY$Duty.1 [match(TOWB5$Number1,DUTY$Transfer.Order..)]
write.csv(TOWB5,file="~/Documents/TOWB6.csv",row.names=FALSE)


