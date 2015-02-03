CTemployee1 = read.csv(file="~/Documents/Bulk_Export_id_REPORT_USER_AND_EXPENSE_CATEGORY.csv",head=TRUE)
Expense = read.csv(file="~/Documents/ExpenseCategories608.csv",head=TRUE)
CTemployee1$Account <- rep(NA,9126)
CTemployee1$Account <- Expense$Expense.Account[match(CTemployee1$Category,Expense$Name)] 
write.csv(CTemployee1,file="~/Documents/CTreport5.csv",row.names=FALSE)


#Concur Data
CTemployee2 = read.csv(file="~/Documents/CTreport4.csv",head=TRUE)
Expense.D <- read.csv(file="~/Documents/ConcurExpenseDetails.csv",head=TRUE)
CTemployee2$Expense <- rep(NA,13158)
CTemployee2$Expense <- Expense.D$Name[match(CTemployee2$EXP_KEY,Expense.D$Code)] 
CTemployee2$Account <- rep(NA,13158)
CTemployee2$Account <- Expense$Expense.Account[match(CTemployee2$Expense,Expense$Name)] 
write.csv(CTemployee2,file="~/Documents/CTreport6.csv",row.names=FALSE)


#Doing R Analsying on Bullshit stuff
Concur = read.csv(file="~/Documents/CT_report.csv",head=TRUE)
NS = read.csv(file="~/Documents/TransactionSearchResults138.csv",head=TRUE)
Concur$InNS <- rep(NA,2032)
Concur$Amount <- rep (0,2032)
Concur$InNS <- NS$TRUE.[match(Concur$REPORT_ID,NS$External.Ref..Number)] 
Concur$Amount <- NS$Amount[match(Concur$REPORT_ID,NS$External.Ref..Number)] 
write.csv(Concur,file="~/Documents/CONCUR-NS1.csv",row.names=FALSE)
