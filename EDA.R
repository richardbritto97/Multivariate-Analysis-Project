#installing necessary packages
install.packages("tidyverse")
install.packages("magrittr")

#importing necessary libraries
library(data.table)
library(magrittr)
library(tidyverse)
library(stringr)
library(ggplot2)

#Data Cleaning

dt = setDT(Lending_Club)
class(dt)
str(dt)
View(dt)
?glimpse
glimpse(dt)
?select

#dropping the first column which contains NA
?subset
dt = subset(dt, select = -(id) )
view(dt)

dt(,col=NA)
?dt 
?grep

str_detect(dt,'NA')
dt['NA',]
dt[is.na(url),NROW(url)]
NROW(dt)
dt = subset(dt, select = -(url) )

str(dtc)

dtc = copy(Lengin_Club_Clean)
dtc = setDT(dtc)
view(dtc)

str(dtc)

dtc[,grade:=factor(grade,levels= c('A','B','C','D','E','F','G'),ordered=F)]

#int_rate to numeric

dtc[,int_rate:=gsub('[%]','',int_rate)]
dtc[,int_rate:=trimws(int_rate)]

dtc[,int_rate:=suppressWarnings(as.numeric(int_rate))]
str(dtc)
dtc[,grade:=(grade)]

dtc[,factor(sub_grade)]

dtc[,factor(emp_length)]

dtc[,emp_length:=factor(emp_length)]

dtc[grep('',revol_util),revol_util]

dtc[,factor(term)]
dtc[,term:=factor(term)]
pairs(dtc)

a = dtc[grep('[0-9]',addr_state)]
attach(dtc)
plot(loan_amnt, int_rate, pch=c(1,100)[Default_Flag],xlab="loan amount",ylab="interest rate")

ggplot(dtc, aes(x=loan_amnt, y=int_rate, Default_Flag=1 )) +
  geom_point()
?ggplot
?geom_point()
?geom_smooth()
pairs(test[,c(2,6,16,21,25)])
?pairs

## 75% of the sample size
smp_size <- floor(0.75 * nrow(dtc))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(dtc)), size = smp_size)

train <- dtc[train_ind, ]
test <- dtc[-train_ind, ]
cor(dtc[,c(2,6,16,21,25)])
test

install.packages('corrplot')
library(corrplot)
mat <- round(cor(dtc[,c(2,16,21,25)], use="pair"),2)
corrplot(mat)

    dtc[,loan_amnt:=as.numeric(loan_amnt)]
    dtc[,dti:=as.numeric(dti)]
    dtc[,open_acc:=as.numeric(open_acc)]
    dtc[,Default_Flag:=as.numeric(Default_Flag)]
    
    str(dtc)
   
    defaultdata =  filter(dtc, Default_Flag == 1)
    
    view(defaultdata)
#Univariate 
    
ggplot(data = defaultdata, aes(x=int_rate,y=Default_Flag)) + geom_bar(stat = 'identity')
       
ggplot(data = defaultdata, aes(x=int_rate)) + geom_histogram(color="black", fill="blue")


ggplot(data = dtc, aes(x=int_rate)) + geom_histogram(color="black", fill="blue")

ggplot(dtc, aes(x=int_rate, color=loan_status)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")

ggplot(dtc, aes(x=int_rate,color=loan_status,fill=loan_status)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2)


ggplot(dtc, aes(x=dti,color=loan_status,fill=loan_status)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2)


ggplot(dtc, aes(x=loan_amnt,color=loan_status,fill=loan_status)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2)

ggplot(dtc, aes(x=annual_inc,color=loan_status,fill=loan_status)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2)


ggplot(dtc[which(annual_inc!=6000000),], aes(x= annual_inc,color=loan_status,fill=loan_status)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2)


ggplot(data = dtc, aes(x=annual_inc)) + geom_histogram(color="black", fill="blue",binwidth=1000,bins=5)

ggplot(dtc, aes(x=annual_inc,y=annual_inc)) + 
  geom_boxplot(outlier.colour="red",
               outlier.size=4)
summary(dtc$annual_inc)

ggplot(dtc[which(annual_inc<100000),], aes(x= annual_inc,color=loan_status,fill=loan_status)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2)

max(dtc$annual_inc)

#Bivariate Analysis
defaultdata=setDT(defaultdata)
ggplot(data = defaultdata, aes(x=purpose,y=Default_Flag)) + geom_bar(stat = 'count')

ggplot(defaultdata, aes(x = purpose)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))

ggplot(dtc, aes(x = purpose)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)))



view(defaultdata)    
count(Default_Flag[purpose])
2407/3987

#purpose f
install.packages('dplyr')
library(dplyr)

dtf=dtf %>% group_by(purpose) %>%  mutate(cnt = NROW(member_id)) %>% as.data.frame(dtf)

dtf=dtf %>% group_by(purpose) %>%  mutate(defcnt = sum(Default_Flag)) %>% as.data.frame(dtf)

purpose = unique(dtf[,c('purpose','cnt','defcnt')])
purpose = purpose %>% group_by(purpose) %>% mutate(percent = (defcnt*100)/cnt) %>% as.data.frame(purpose)

purpose

data.table(dtf)
ggplot(purpose, aes(x = percent,y = purpose)) +  geom_bar(stat = 'identity')


#no of accounts a
dta=c(1:4)
data.table(dta)

dtc[ open_acc %in% c(1,2,3,4,5), 'x' := 'LT5']
dtc[ open_acc %in% c(6,7,8,9,10), 'x' := '6-10']
dtc[ open_acc %in% c(11,12,13,14,15), 'x' := '11-15']
dtc[ open_acc >15, 'x' := '15+']
dtc = dtc %>% rename(no_of_acct=x)

dta=dtc %>% group_by(no_of_acct) %>%  mutate(cnt = NROW(member_id)) %>% as.data.frame(dtc)

dta=dta %>% group_by(no_of_acct) %>%  mutate(defcnt = sum(Default_Flag)) %>% as.data.frame(dta)
dta

acctvsdefault = unique(dta[,c('no_of_acct','cnt','defcnt')])

acctvsdefault = acctvsdefault %>% group_by(no_of_acct) %>% mutate(percent = (defcnt*100)/cnt) %>% as.data.frame(acctvsdefault)

str(acctvsdefault)

data.table(acctvsdefault)
ggplot(acctvsdefault, aes(x = percent,y = no_of_acct)) +  geom_bar(stat = 'identity') + coord_cartesian(xlim=c(12.5,15.5))

#inquiry

dtinq=dtc %>% group_by(inq_last_6mths) %>%  mutate(cnt = NROW(member_id)) %>% as.data.frame(dtc)

dtinq=dtinq %>% group_by(inq_last_6mths) %>%  mutate(defcnt = sum(Default_Flag)) %>% as.data.frame(dtinq)
dtinq
inq = unique(dtinq[,c('inq_last_6mths','cnt','defcnt')])

inq = inq %>% group_by(inq_last_6mths) %>% mutate(percent = (defcnt*100)/cnt) %>% as.data.frame(inq)


class(inq)
setDT(inq)
inq[,inq_last_6mths := factor(inq_last_6mths)]
str(inq)
ggplot(inq, aes(x = percent,y = inq_last_6mths)) +  geom_bar(stat = 'identity')



