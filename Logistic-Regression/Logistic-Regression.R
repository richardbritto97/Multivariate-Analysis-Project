#Loading required libraries
library(cluster)
library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(knitr)
library(corrplot)
library(tidyverse)
library(factoextra)
library(psych)
library(FactoMineR)
library(nFactors)
library(GGally)
library(MASS)
library(gvlma)
library(leaps)
library(relaimpo)
library(cowplot)
library(regclass)
library(e1071)
library(caret)
library(pROC)
#library(FFally)


### Data Loading

Lending_Data <- read_csv('Lending_Data.csv')
Lend = copy(Lending_Data)
Lend = setDT(Lend)
Logistic_training_final <- read_csv('Logistic_training_final.csv')
Lending_log = copy(Logistic_training_final)
Lending_log = setDT(Lending_log)

#view(Lend)
str(Lending_log)
### Data Cleaning

Lend[, member_id := factor(member_id)]
Lend[, loan_status := factor(loan_status)]
Lend[, home_ownership := factor(home_ownership)]
Lend[, purpose := factor(purpose)]
Lend[, verification_status := factor(verification_status)]

Lend[, int_rate := gsub('[%]', '', int_rate)]
Lend[, int_rate := trimws(int_rate)]
Lend[, int_rate := suppressWarnings(as.numeric(int_rate))]

Lend[open_acc %in% c(1, 2, 3, 4, 5), 'x' := 'LT5']
Lend[open_acc %in% c(6, 7, 8, 9, 10), 'x' := '6-10']
Lend[open_acc %in% c(11, 12, 13, 14, 15), 'x' := '11-15']
Lend[open_acc > 15, 'x' := '15+']
Lend = Lend %>% rename(no_of_acct = x)
str(Lend)

### Data Splitting

#Training Testing

## 10% of the sample size
smp_size = floor(0.10 * nrow(Lend))

## set the seed to make our partition reproducible
set.seed(123)
train_ind = sample(seq_len(nrow(Lend)), size = smp_size)

train = Lend[train_ind, ]
test = Lend[-train_ind, ]


############### Logistic Regression ###############


head(Lending_log) # you see data, but no column names

xtabs(~ loan_status + roi, data = Lending_log)
#xtabs(~ loan_status + loan_amnt, data = Lending_log)
xtabs(~ loan_status + inq_last_6mths, data = Lending_log)
xtabs(~ loan_status + purpose, data = Lending_log)
xtabs(~ loan_status + revol_util, data = Lending_log)
xtabs(~ loan_status + Late_fee_bin, data = Lending_log)
xtabs(~ loan_status + term, data = Lending_log)
#xtabs(~ loan_status + total_pymnt, data = Lending_log)


logistic_lend <- glm(loan_status ~ purpose, data = Lending_log)
summary(logistic_lend)

ll_null <- logistic_lend$null.deviance / -2
ll_proposed <- logistic_lend$deviance / -2

ll_null
ll_proposed

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll_null - ll_proposed) / ll_null

## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2 * (ll_proposed - ll_null), df = 1)
1 - pchisq((logistic_lend$null.deviance - logistic_lend$deviance), df = 1)

## Lastly, let's  see what this logistic regression predicts, given
## the interest rate (and no other data about them).
predicted_data <- data.frame(probability_of_default = logistic_lend$fitted.values,
                             purpose = Lending_log$purpose)
predicted_data

## We can plot the data...

ggplot(data = predicted_data, 
       aes(x = purpose, y = probability_of_default)) + 
  geom_point( size = 5) + 
  xlab("Purpose") +
  ylab("Predicted probability of being a Defaulter")


#####################################


## Now we will use all of the data available to predict loan defaulters.


#####################################

str(Lending_log)
logistic_complex <- glm(loan_status ~ roi + loan_amnt + inq_last_6mths + purpose +
                          revol_util + Late_fee_bin + term + total_pymnt,
                        data = Lending_log)
summary(logistic_complex)

## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic_complex$null.deviance / -2
ll.proposed <- logistic_complex$deviance / -2

## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null

## The p-value for the R^2
1 - pchisq(2 * (ll.proposed - ll.null), df = (length(logistic_complex$coefficients) - 1))

## now we can plot the data
predicted.data <- data.frame(probability.of.lend = logistic_complex$fitted.values,
                             Default = Lending_log$loan_status)
summary(predicted.data)
str(Lend)
NROW(predicted.data)
NROW(Lend_log)

predicted.data <- predicted.data[order(predicted.data$probability.of.lend, 
                                       decreasing = FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)


## Lastly, we can plot the predicted probabilities for each sample having

## loan default and color by whether or not they actually had defaulted
ggplot(data = predicted.data,
       aes(x = rank, y = probability.of.lend)) +
  geom_point(aes(color = Default), alpha = 1, shape = 4, stroke = 2) +
  xlab("Index") +
  ylab("Predicted probability of getting heart disease")

# Few packages for confusion matrix. Lets look at them one by one


NROW(logistic_complex$fitted.values)
confusion_matrix(logistic_complex)


pdata <- predict(logistic_complex, newdata = Lending_log, type = "response" )
head(pdata)
summary(pdata)                    
pdataF <- as.factor(ifelse(test = as.numeric(pdata > 0.5) == 0,
                           yes = 0, no = 1))
NROW(pdataF)                    
NROW(Lending_log$loan_status)
head(pdataF)
head(Lending_log$loan_status)
Lending_log[, loan_status := factor(loan_status)]
confusionMatrix(pdataF, Lending_log$loan_status)

par(pty = "s")
roc(Lending_log$loan_status, logistic_complex$fitted.values, plot = TRUE)

## NOTE: By default, roc() uses specificity on the x-axis and the values range

## from 1 to 0. This makes the graph look like what we would expect, 
## but the x-axis itself might induce a headache. To use 1-specificity 
## (i.e. the False Positive Rate) on the x-axis, set "legacy.axes" to TRUE.


logistic_complex$fitted.values

roc(Lending_log$loan_status,
    logistic_complex$fitted.values,
    plot = TRUE,
    legacy.axes = TRUE)

roc(Lending_log$loan_status,
    logistic_complex$fitted.values, 
    plot = TRUE,
    legacy.axes = TRUE,
    xlab = "False Positive Percentage",
    ylab = "True Postive Percentage")

roc(Lending_log$loan_status, 
    logistic_complex$fitted.values, 
    plot = TRUE,
    legacy.axes = TRUE, 
    xlab = "False Positive Percentage",
    ylab = "True Postive Percentage",
    col = "#377eb8",
    lwd = 4)

roc(Lending_log$loan_status, 
    logistic_complex$fitted.values,
    plot = TRUE,
    legacy.axes = TRUE,
    xlab = "False Positive Percentage", 
    ylab = "True Postive Percentage", 
    col = "#377eb8", 
    lwd = 4)

## If we want to find out the optimal threshold we can store the

## data used to make the ROC graph in a variable...

roc.info <- roc(Lending_log$loan_status,
                logistic_complex$fitted.values,
                legacy.axes = TRUE)

str(roc.info)

## tpp = true positive percentage               
## fpp = false positive precentage
roc.df <- data.frame(tpp = roc.info$sensitivities * 100, 
                     fpp = (1 - roc.info$specificities) * 100,
                     thresholds = roc.info$thresholds)

roc.df

head(roc.df) 
## head() will show us the values for the upper right-hand corner of the ROC graph, 
##when the threshold is so low
## (negative infinity) that every single sample is called "obese".
## Thus TPP = 100% and FPP = 100%

tail(roc.df) 
## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity)
## that every single sample is called "not obese".
## Thus, TPP = 0% and FPP = 0%
## now let's look at the thresholds between TPP 60% and 80%

roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]

roc(Lending_log$loan_status, 
    logistic_complex$fitted.values, 
    plot = TRUE,
    legacy.axes = TRUE, 
    xlab = "False Positive Percentage",
    ylab = "True Postive Percentage",
    col = "#377eb8", 
    lwd = 4, 
    percent = TRUE)

roc(Lending_log$loan_status, 
    logistic_complex$fitted.values, 
    plot = TRUE,
    legacy.axes = TRUE,
    xlab = "False Positive Percentage",
    ylab = "True Postive Percentage",
    col = "#377eb8",
    lwd = 4, 
    percent = TRUE, 
    print.auc = TRUE)

roc(Lending_log$loan_status,
    logistic_complex$fitted.values,
    plot = TRUE,
    legacy.axes = TRUE,
    xlab = "False Positive Percentage",
    ylab = "True Postive Percentage",
    col = "#377eb8",
    lwd = 4,
    percent = TRUE,
    print.auc = TRUE, 
    partial.auc = c(100, 90),
    auc.polygon = TRUE,
    auc.polygon.col = "#377eb822",
    print.auc.x = 45)

# Lets do two roc plots to understand which model is better

roc(Lending_log$loan_status, 
    logistic_lend$fitted.values, 
    plot = TRUE,
    legacy.axes = TRUE,
    percent = TRUE,
    xlab = "False Positive Percentage",
    ylab = "True Postive Percentage",
    col = "#377eb8",
    lwd = 4, 
    print.auc = TRUE)

# Lets add the other graph

plot.roc(Lending_log$loan_status,
         logistic_complex$fitted.values,
         percent = TRUE,
         col = "#4daf4a",
         lwd = 4,
         print.auc = TRUE,
         add = TRUE,
         print.auc.y = 40)

legend("bottomright",
       legend = c("Simple", "Non Simple"),
       col = c("#377eb8", "#4daf4a"),
       lwd = 4) # Make it user friendly

# reset the par area back to the default setting

par(pty = "m")

