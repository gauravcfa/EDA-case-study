#setting working directory

#Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(scales)
library(stringr)
library(dplyr)
library(lubridate)
library(gridExtra)

#Loading loan.csv into dataframe
base_data<- read.csv("loan.csv",stringsAsFactors = F)
#View(base_data)

################ DATA CLEANING Code###############

#Removing the col' which contains NAs only using apply function 
data_clean1 <- base_data[,apply(base_data , 2 , function(x)  !all(is.na(x)))]
#REmoving all the col's that containns O's only
data_clean2 <- data_clean1[,apply(data_clean1 , 2 , function(x)  !all(x==0))]
#REmoving all the col's that containnsO's  + NAs 
data_clean2<-data_clean2[,colSums(data_clean2 != 0,na.rm = T)>0]
#REmoving all the col's that contains unique values
data_clean3 <- data_clean2[, apply(data_clean2 , 2 , function(x) !length(unique(x)) == 1)]
#Removing % sign from int_rate
data_clean3$int_rate<- gsub("%","",data_clean3$int_rate)
data_clean3$int_rate<- as.numeric(data_clean3$int_rate)

data_clean3$revol_util<- gsub("%","",data_clean3$revol_util)
data_clean3$revol_util<- as.numeric(data_clean3$revol_util)

#Formatting all dates col's
data_clean3$issue_d<-paste0("01-",data_clean3$issue_d)
data_clean3$issue_d <- as.POSIXlt(data_clean3$issue_d, format = "%d-%b-%y")

data_clean3$earliest_cr_line<-paste0("01-",data_clean3$earliest_cr_line)
data_clean3$earliest_cr_line <- as.POSIXlt(data_clean3$earliest_cr_line, format = "%d-%b-%y")

data_clean3$last_credit_pull_d<-paste0("01-",data_clean3$last_credit_pull_d)
data_clean3$last_credit_pull_d <- as.POSIXlt(data_clean3$last_credit_pull_d, format = "%d-%b-%y")

data_clean3$last_pymnt_d<-paste0("01-",data_clean3$last_pymnt_d)
data_clean3$last_pymnt_d <- as.POSIXlt(data_clean3$last_pymnt_d, format = "%d-%b-%y")


#Std. precision
data_clean3$funded_amnt_inv <- round(data_clean3$funded_amnt_inv)
data_clean3$annual_inc <- round(data_clean3$annual_inc)
data_clean3$out_prncp <- round(data_clean3$out_prncp)
data_clean3$out_prncp_inv <- round(data_clean3$out_prncp_inv)
data_clean3$total_pymnt <- round(data_clean3$total_pymnt)
data_clean3$installment <- round(data_clean3$installment)
data_clean3$total_pymnt_inv <- round(data_clean3$total_pymnt_inv)
data_clean3$total_rec_prncp <- round(data_clean3$total_rec_prncp)
data_clean3$total_rec_int <- round(data_clean3$total_rec_int)
data_clean3$total_rec_late_fee <- round(data_clean3$total_rec_late_fee)
data_clean3$recoveries <- round(data_clean3$recoveries)
data_clean3$collection_recovery_fee <- round(data_clean3$collection_recovery_fee)
data_clean3$last_pymnt_amnt <- round(data_clean3$last_pymnt_amnt)

##############################End of data cleaning #

#Derived Fields

data_clean3$defaulter<-ifelse(data_clean3$loan_status=="Charged Off","Defaulter","Non-Defaulter") 
# since number of current are very low have combined them with Fully Paid
data_clean3$loan_burden_per <- round(data_clean3$installment/(data_clean3$annual_inc/12)*100)
#monthly burden of EMI


#write.csv(data_clean3,file="cleanedfile.csv")


########################univariate analysis###################################


#unique ids
sum(is.na(data_clean3$id))
length(data_clean3$id)
length(unique(data_clean3$id))


sum(is.na(data_clean3$member_id))
length(data_clean3$member_id)
length(unique(data_clean3$member_id))
# total records of 39717 applicant no duplicate records found


sum(is.na(data_clean3$loan_status))
summary(factor(data_clean3$loan_status))
round(summary(factor(data_clean3$loan_status))/nrow(data_clean3)*100)
#5627/39717 are defaulers which account for 14% , 5627 are in process of paying installment      

plot<-ggplot(data_clean3, aes(x = loan_status)) + geom_histogram(binwidth = 1 , stat = "count") +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1)+theme_classic()
plot
ggsave(filename = "plot1.jpeg",plot)


#Loan amount

round(mean(data_clean3$loan_amnt))
temp<-aggregate(data_clean3$loan_amnt,by=list(data_clean3$loan_status),FUN=mean)
temp$x<- round(temp$x)
temp$per<-temp$x/mean(data_clean3$loan_amnt)*100
temp
plot<-ggplot(data_clean3, aes(x=factor(loan_status), y=loan_amnt)) + stat_summary(fun.y="mean", geom="bar")
plot
ggsave(filename = "plot2.jpeg",plot)
# average loan amt applied = 11219 , Avg amt funded to  defaulers = 12104  which + 7%/average  


#funded amt inv
round(mean(data_clean3$funded_amnt_inv))
temp<-aggregate(data_clean3$funded_amnt_inv,by=list(data_clean3$loan_status),FUN=mean)
temp$x<- round(temp$x)
temp$per<-temp$x/mean(data_clean3$funded_amnt_inv)*100
temp
plot<-ggplot(data_clean3, aes(x=factor(loan_status), y=funded_amnt_inv)) + stat_summary(fun.y="mean", geom="bar")
plot
ggsave(filename = "plot3.jpeg",plot)
# average amount funded = 10397 , Avg amt funded to  defaulers = 10865  which + 4%/average  


#term 36 or 60 months
sum(is.na(data_clean3$term))
summary(factor(data_clean3$term))
term<-table(data_clean3$term,data_clean3$loan_status)
term
round(term/nrow(data_clean3)*100)
plot<-ggplot(data_clean3, aes(x = term)) + geom_bar(stat = "count") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -1)+theme_classic()
plot
ggsave(filename = "plot4.jpeg",plot)

plot<-ggplot(data_clean3, aes(x = term)) +
  geom_bar(stat = "count", aes(fill =loan_status)) +
  labs( x= "Term",y="count of applicant")+theme_classic()
plot
ggsave(filename = "plot5.jpeg",plot)
# maxmium applicant appiled for 36 monmths=29096
# max defaulters [3227]are those who have applied for 36 momths & their contibution in overall is 8% 
#with in charged off group ; 36 months - 57% , 60 monmths =43%


#intrest rate
round(mean(data_clean3$int_rate), 2)
temp<-aggregate(data_clean3$int_rate,by=list(data_clean3$loan_status),FUN=mean)
temp$x<- round(temp$x,2)
temp

temp$per<-temp$x/mean(data_clean3$int_rate)*100
temp
plot<-ggplot(data_clean3, aes(x=factor(loan_status), y=int_rate)) + stat_summary(fun.y="mean", geom="bar")
plot
ggsave(filename = "plot6.jpeg",plot)

ggplot(data_clean3,aes(factor(defaulter),int_rate))+geom_boxplot(fill="blue",notch = "TRUE",na.rm = TRUE)+
  ggtitle("Loan status vs.Interest Rate")+
  labs(x="Loan Status",y="Interest Rate")

#average int rate is 12.02 , charged off - 13.82 +14% /mean , high int. have been charged for defaulter cases

#Grade

sum(is.na(data_clean3$grade))
summary(factor(data_clean3$grade))
table(data_clean3$grade,data_clean3$loan_status)

grade_prop<-prop.table(table(data_clean3$grade,data_clean3$loan_status),1)*100
grade_prop
grade_prop<-as.data.frame.matrix(grade_prop) 
barplot(grade_prop$`Charged Off`,names.arg = row.names(grade_prop))
text(grade_prop$`Charged Off`,labels=sprintf(round(grade_prop$`Charged Off`,digits = 0),fmt = "%1.0f%%"),las=1,pos = 4)
# higher grades have lower default rates


#Emp length
plot<-ggplot(data_clean3, aes(x = emp_length)) +
  geom_bar(stat = "count", aes(fill =loan_status)) +
  labs( x= "Term",y="count of applicant")+theme_classic()
plot

ggplot(data_clean3, aes(x = emp_length)) +
  geom_bar(stat = "count", aes(fill =loan_status)) + facet_wrap( ~term)+
  labs( y= "Cnt",x="Emp Length")+theme_classic()+coord_flip()

sum(is.na(data_clean3$emp_length))
summary(factor(data_clean3$emp_length))
table(data_clean3$emp_length,data_clean3$loan_status)
emp_length_prop<-prop.table(table(data_clean3$emp_length,data_clean3$loan_status),1)*100
emp_length_prop
emp_length_prop<-as.data.frame.matrix(emp_length_prop) 
barplot(emp_length_prop$`Charged Off`,names.arg = row.names(emp_length_prop))

#max no of charged of cases are with emp_length = 10yrs+
#howvever no major % contribution has been noticed across emp length with in charged off category



#home ownership

plot<-ggplot(data_clean3, aes(x = home_ownership)) +
  geom_bar(stat = "count", aes(fill =loan_status)) +
  labs( x= "Term",y="count of applicant")+theme_classic()
plot

sum(is.na(data_clean3$home_ownership))
summary(factor(data_clean3$home_ownership))
table(data_clean3$home_ownership,data_clean3$loan_status)
home_ownership_prop<-prop.table(table(data_clean3$home_ownership,data_clean3$loan_status),1)
home_ownership_prop
home_ownership_prop<-as.data.frame.matrix(home_ownership_prop) 
barplot(home_ownership_prop$`Charged Off`,names.arg = row.names(home_ownership_prop))
# maxmium cases of defaulters are from Rent & mortage and their % cont. is 13% and 15%
# average of all default cases is 13%
#so home_ownership has not much impact on default rates

#verification status
plot<-ggplot(data_clean3, aes(x = verification_status)) +
  geom_bar(stat = "count", aes(fill =loan_status)) +
  labs( x= "Term",y="count of applicant")+theme_classic()
plot

sum(is.na(data_clean3$verification_status))
summary(factor(data_clean3$verification_status))
table(data_clean3$verification_status,data_clean3$loan_status)
verified_prop<-prop.table(table(data_clean3$verification_status,data_clean3$loan_status),1)*100
verified_prop
verified_prop<-as.data.frame.matrix(verified_prop) 
barplot(verified_prop$`Charged Off`,names.arg = row.names(verified_prop))
text(verified_prop$`Charged Off`,labels=sprintf(round(verified_prop$`Charged Off`,digits = 1),fmt = "%1.0f%%"),las=2)
#nO OF default cases are from 'not verified category'
#however verified records have higher default rates 16% which is not drawing any business sense!


#Purpose
plot<-ggplot(data_clean3, aes(x = purpose)) +
  geom_bar(stat = "count", aes(fill =loan_status)) +
  labs( x= "Term",y="count of applicant")+theme_classic()+coord_flip()
plot

sum(is.na(data_clean3$purpose))
summary(factor(data_clean3$purpose))
table(data_clean3$purpose,data_clean3$loan_status)
verified_prop<-prop.table(table(data_clean3$purpose,data_clean3$loan_status),1)*100
verified_prop
verified_prop<-as.data.frame.matrix(verified_prop)
barplot(verified_prop$`Charged Off`,names.arg = row.names(verified_prop))
#maxmium no of cases are from 'debt_consolidation'
#within purpose category 26% default cases are from 'small business'

#addr_state
plot<-ggplot(data_clean3, aes(x = addr_state)) +
  geom_bar(stat = "count", aes(fill =loan_status)) +
  labs( x= "Term",y="count of applicant")+theme_classic()
plot
sum(is.na(data_clean3$addr_state))
summary(factor(data_clean3$addr_state))
table(data_clean3$addr_state,data_clean3$loan_status)
verified_prop<-prop.table(table(data_clean3$addr_state,data_clean3$loan_status),1)*100
verified_prop
verified_prop<-as.data.frame.matrix(verified_prop) 
barplot(verified_prop$`Charged Off`,names.arg = row.names(verified_prop))
#maxmmium cases are from California state follwed by NY
#% cont. of Californaia's cases are 16% against avg of 13% from other states [ excluding one outliers] 

#dti

round(mean(data_clean3$dti), 2)
temp<-aggregate(data_clean3$dti,by=list(data_clean3$loan_status),FUN=mean)
temp$x<- round(temp$x,2)
temp
temp$per<-temp$x/mean(data_clean3$dti)*100
temp
plot<-ggplot(data_clean3, aes(x=factor(loan_status), y=dti)) + stat_summary(fun.y="mean", geom="bar")
plot
#higher dti ration for default cases 14% as compare to overall average of 13.3% , +5% obove average


#annual income
round(mean(data_clean3$annual_inc), 2)
temp<-aggregate(data_clean3$annual_inc,by=list(data_clean3$loan_status),FUN=mean)
temp$x<- round(temp$x,2)
temp
temp$per<-temp$x/mean(data_clean3$annual_inc)*100
temp
# average low annual income observed for default cases as compre to other category of status

# pub_rec_bankruptcies
# Unique Counts of bankruptcies
B_count<- unique(na.omit(data_clean3$pub_rec_bankruptcies))
# Percentage of customers who defaulted and had 0 bankruptcies
a <- length(which(data_clean3$pub_rec_bankruptcies==0 & data_clean3$loan_status=="Charged Off"))/length(which(data_clean3$pub_rec_bankruptcies==0))*100
# Percentage of customers who defaulted and had 1 bankruptcies
b <- length(which(data_clean3$pub_rec_bankruptcies==1 & data_clean3$loan_status=="Charged Off"))/length(which(data_clean3$pub_rec_bankruptcies==1))*100
# Percentage of customers who defaulted and had 2 bankruptcies
c <- length(which(data_clean3$pub_rec_bankruptcies==2 & data_clean3$loan_status=="Charged Off"))/length(which(data_clean3$pub_rec_bankruptcies==2))*100

# Percentage of customers who fully paid and had 0 bankruptcies
x <- length(which(data_clean3$pub_rec_bankruptcies==0 & data_clean3$loan_status=="Fully Paid"))/length(which(data_clean3$pub_rec_bankruptcies==0))*100
# Percentage of customers who fully paid and had 1 bankruptcies
y <- length(which(data_clean3$pub_rec_bankruptcies==1 & data_clean3$loan_status=="Fully Paid"))/length(which(data_clean3$pub_rec_bankruptcies==1))*100
# Percentage of customers who fully paid and had 2 bankruptcies
z <- length(which(data_clean3$pub_rec_bankruptcies==2 & data_clean3$loan_status=="Fully Paid"))/length(which(data_clean3$pub_rec_bankruptcies==2))*100

Default_Percent<-c(a,b,c)
FullyPaid_Percent<- c(x,y,z)
bankruptcy<- data.frame(B_count, Default_Percent, FullyPaid_Percent)
bankruptcy

# Plot camparing count of bankruptcies against Default percentage.
ggplot(bankruptcy, aes(x=B_count, y=Default_Percent, fill=Default_Percent)) + geom_col()+ 
  geom_text(aes(label = format(round(Default_Percent, 1), nsmall = 1)), vjust = -0.5)+ 
  ggtitle("Default percentage against count of Bankruptcies")

# Plot camparingcount of bankruptcies against Fully Paid percentage.
ggplot(bankruptcy, aes(x=B_count, y=FullyPaid_Percent, fill=FullyPaid_Percent)) + geom_col()+
  geom_text(aes(label = format(round(FullyPaid_Percent, 1), nsmall = 1)), vjust = -0.5)+ 
  ggtitle("Fully Paid percentage against count of Bankruptcies")
# This indicates that customers with higher bankruptcy count are more likely to default
# and the ones with lower or no bankruptcy count are more likely to fully pay off the loan.



#loan burden
sum(is.na(data_clean3$loan_burden_per))
summary(data_clean3$loan_burden_per)
ggplot(data_clean3,aes(factor(defaulter),loan_burden_per))+geom_boxplot(fill="blue",notch = "TRUE",na.rm = TRUE)+
  ggtitle("Loan status vs.Loan Burden")+
  labs(x="Loan Status",y="Loan Burden")
#Conclusion:Defaulters have higher loan burden

#others
data_clean3$Month_Yr <- format(as.Date(data_clean3$issue_d), "%Y-%m")
table(factor(data_clean3$Month_Yr))
table(data_clean3$Month_Yr,data_clean3$defaulter,data_clean3$grade)
month_prop<-prop.table(table(data_clean3$Month_Yr,data_clean3$defaulter),1)
month_prop
month_prop<-as.data.frame.matrix(month_prop) 
month_prop$date_issue<-row.names(month_prop)
barplot(month_prop$Defaulter,names.arg = row.names(month_prop))
month_prop$mon_yr<-dym(month_prop$date_issue)
summary(month_prop$mon_yr)
p1<-ggplot(month_prop,aes(mon_yr,Defaulter))+geom_line()+
  ggtitle("Movement of Default rates over time")
p1

p2<-ggplot(data_clean3,aes(issue_d,int_rate, col=grade))+geom_line()+
  geom_smooth()+
  ggtitle("Movement of Interest rates over time")
p2

term_new_prop<-prop.table(table(data_clean3$Month_Yr,data_clean3$term),1)
term_new_prop<-as.data.frame.matrix(term_new_prop) 
term_new_prop$date_issue<-row.names(term_new_prop)

term_new_prop$mon_yr<-dym(term_new_prop$date_issue)
class(term_new_prop$mon_yr)
summary(term_new_prop$mon_yr)
p3<-ggplot(term_new_prop,aes(mon_yr,term_new_prop[,1]))+geom_line()+
  ggtitle("% of loans where term is 36 months")
p3

p4<-ggplot(term_new_prop,aes(mon_yr,term_new_prop[,2]))+geom_line()+
  ggtitle("% of loans where term is 60 months")
p4

grid.arrange(p1,p2,p3,p4,nrow=2,ncol=2)

####end of code######
