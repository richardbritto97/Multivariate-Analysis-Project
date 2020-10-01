#Loading required libraries
library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(knitr)
library(corrplot)
library(tidyverse)

### Data Loading

Lending_Data <- read_csv('Lending_Data.csv')
Lend = copy(Lending_Data)
Lend = setDT(Lend)
view(Lend)
str(Lend)

### Data Cleaning

Lend[, member_id := factor(member_id)]
Lend[, loan_status := factor(loan_status)]
Lend[, home_ownership:= factor(home_ownership)]
Lend[, purpose := factor(purpose)]
Lend[, verification_status := factor(verification_status)]

Lend[, int_rate := gsub('[%]', '', int_rate)]
Lend[, int_rate := trimws(int_rate)]
Lend[, int_rate := suppressWarnings(as.numeric(int_rate))]

Lend[open_acc %in% c(1,2,3,4,5), 'x' := 'LT5']
Lend[open_acc %in% c(6,7,8,9,10), 'x' := '6-10']
Lend[open_acc %in% c(11,12,13,14,15), 'x' := '11-15']
Lend[open_acc >15, 'x' := '15+']
Lend = Lend %>% rename(no_of_acct = x)
str(Lend)


### Data Splitting

#Training Testing

## 75% of the sample size
smp_size = floor(0.75 * nrow(Lend))

## set the seed to make our partition reproducible
set.seed(123)
train_ind = sample(seq_len(nrow(Lend)), size = smp_size)

train = Lend[train_ind, ]
test = Lend[-train_ind, ]

#default & nondefault data

defaultdata =  filter(Lend, Default_flag == 1)
nondefault = filter(Lend, Default_flag == 0)
view(defaultdata)
view(nondefault)
defaultdata = setDT(defaultdata)
nondefault = setDT(nondefault)

### Data Exploration

#Let us start by exploring the density of defaulters and nondefaulters
# accros the total density for each variables, i.e int_rate, dti, loan_amnt, annual_inc
  
#Let us first see the histogram count of defaulters on interest rates:
ggplot(data = defaultdata, aes(x = int_rate)) + 
  geom_histogram(color = "black", fill = "red", alpha = 0.3)


#Let us see the histogram count of non defaulters on interest rates:
ggplot(data = nondefault, aes(x = int_rate)) + 
  geom_histogram(color = "black", fill = "#00FFFF", alpha = 0.3)


#Now let us compare the above histograms on one plot and a 
# common scale against interest rates:
ggplot(Lend, aes(x = int_rate, color = loan_status, fill = loan_status)) +
  geom_histogram(alpha = 0.3, position = "identity")

#Let us observe the density of Defaulters and Non Defaulters 
# on their interest rates, we might expect to get some insights:
ggplot(Lend, aes(x = int_rate, color = loan_status, fill = loan_status)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2)

#As we can see in the above plot that when the interest rates 
# were low the default rates were proportionally low as compared 
# to the non default rate. As we go on increasing the interest rate 
# we can see that the density of defaulters increase while the density of
# non defaulters decrease. This can give us some good information about 
# how the interest rate affects the default rates.

#Now Let us perform the save plotting for the debt to income ratio i.e dti:
ggplot(Lend, aes(x = dti, color = loan_status, fill = loan_status)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  geom_density(alpha = .2)

#For the dti against loan_status we can observe a simmilar effect 
# but for the dti greater than 25 we can see that the density of defaulters 
# and non defaulters are almost equal.

#Let us check the density against loan amounts:
ggplot(Lend, aes(x = loan_amnt, color = loan_status, fill = loan_status)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2)

#We dont have much information to get from this graph except that 
# there are more borrowers borrowing loan near the 10,000 mark

#Let us see for the annual income and the loan status:
ggplot(Lend, aes(x = annual_inc, color = loan_status, fill = loan_status)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2)

#This graph looks very weird. This might be due to outliers in this column. 

#Let us check for the summary and outliers:
summary(Lend$annual_inc)

ggplot(Lend, aes(x = annual_inc, y = annual_inc, group = 1)) + 
  geom_boxplot(outlier.colour = "red",outlier.size = 2, alpha = 0.3)

#We can see that the max value for the annual income is 6,000,000 and the 
# 3rd Quartile ends at 83000 which is a clear mark that this is a big outlier problem. 
# We can see in the plot as well that there are many outliers. 

#Let us use the Income_bins for controlling the outlier problem:
ggplot(Lend, aes(x = Income_bins, color = loan_status, fill = loan_status)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") +
  geom_density(alpha = 0.2)

#Now we can see that there is a slight difference in the density of defauters. 
# There are more defaulters when the annual income is gets low


##Let us explore the percentages of defaulters accros every unique value of 
# purpose, open account bins, No of enquiry, delinquent im last 2 years
  
#Let us start with plotting perecntage of defaulters for each loan purpose:
purpose = Lend %>%
  group_by(purpose) %>% 
  summarize(cnt = n(), defcnt = sum(Default_flag)) %>%  
  summarize(purpose, percent = (defcnt*100)/cnt)

ggplot(purpose, aes(x = percent, y = purpose)) + 
  geom_bar(stat = 'identity')

#We can point out that loans taken for vacations have higher  defaulter proportions
# and educational, house, and wedding have lessdefaulter proportions

#Let us plot another graph where we can check the percentage of defaulters 
# over each distinct number of open credit lines:
acctvsdefault = Lend %>% group_by(no_of_acct) %>%  
  summarize(cnt = n(),defcnt = sum(Default_flag)) %>% 
  summarize(no_of_acct, percent = (defcnt*100)/cnt)
ggplot(acctvsdefault, aes(x = percent,y = no_of_acct)) + 
  geom_bar(stat = 'identity') + coord_cartesian(xlim = c(12.5,14.5))

#We can obsserve that borrowers with less than 5 credit lines 
# have less default percentage whereas the borrowers having credit lines 
# between 6-10 have the highest and the ones having more than 15 is yet less.

#Let us plot a graph for percentage of defaults for every number of
# enquiry the borrower made in the last 6 months:
inq = Lend %>% group_by(No_of_Enquiry) %>%  
  summarize(cnt = n(),defcnt = sum(Default_flag)) %>%  
  summarize(No_of_Enquiry,percent = (defcnt*100)/cnt)

ggplot(inq) + 
  geom_bar( mapping = aes(y = percent, x = No_of_Enquiry), stat = 'identity') +
  coord_flip()

#We observe that as the number of enquiries increase the percentage 
# of default rates increase but thats not the case for 8 enquiries 
# in the last 6 months. We need to further explore to understand why is it so

#Let us also explore how are the percentages of defaults related to past 2 year delinqueny:
delinq = Lend %>% 
  group_by(delinq_2yrs) %>% 
  summarize(cnt = n(),defcnt = sum(Default_flag)) %>% 
  summarize(delinq_2yrs, percent = (defcnt*100)/cnt)

ggplot(delinq) + 
  geom_bar( mapping = aes(y = percent, x = delinq_2yrs), stat='identity') +
  coord_flip()

#We can observe that as delinquency increase from 0 to 3 
# there is'nt any significant increase in default percentage but at 
# delinq_2  = 8 the default percent jumps to 50%.

#Let us check the correlation of each variable by pairs

mat <- round(cor(Lend[, c(3,5,8,10,14,17,18)], use = "pair"), 2)
corrplot(mat)

#We can see the correlation of each variable to each other. 
# Annual income looks neggatively correlated to the debt to income ratio. 
# Loan amount also looks negatively correlated to the anual income 
# and have a slightly hiher positive correlation with the number of open credit lines.

#We can use this information we gathered in our EDA for further analysis 
# where we do our hypothesis testing