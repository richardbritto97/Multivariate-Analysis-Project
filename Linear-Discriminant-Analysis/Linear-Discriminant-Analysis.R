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
library(ROCR)
library(klaR)
#library(FFally)


### Data Loading

Lending_Data <- read_csv('Lending_Data.csv')
Lend = copy(Lending_Data)
Lend = setDT(Lend)
#view(Lend)
str(Lend)

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
Lend_lda <- read_csv('Logistic_training_final.csv')
Lend_lda = copy(Logistic_training_final)
Lend_lda = setDT(Lending_log)

### Data Splitting

#Training Testing

## 10% of the sample size
smp_size = floor(0.50 * nrow(Lend_lda))

## set the seed to make our partition reproducible
set.seed(123)
train_ind = sample(seq_len(nrow(Lend_lda)), size = smp_size)
train_ind
train = Lend_lda[train_ind, ]
test = Lend_lda[-train_ind, ]
train
test

###### Linear Discriminant Analysis ######
str(train)

lend_lda <- lda(formula = loan_status ~ ., data = Lend_lda)

summary(lend_lda)
print(lend_lda)
lend_lda$counts
lend_lda$means
lend_lda$scaling
lend_lda$prior
lend_lda$lev
lend_lda$svd

#singular values (svd) that gives the ratio of the between- and within-group standard deviations on the linear discriminant variables.
class(lend_lda)
?lda
lend_lda$N
lend_lda$call
(prop = lend_lda$svd^2/sum(lend_lda$svd^2))
#we can use the singular values to compute the amount of the between-group variance that is explained by each linear discriminant. In our example we see that the first linear discriminant explains more than 99% of the between-group variance in the iris dataset.
lend_lda_2 <- lda(formula = loan_status ~ ., data = Lend_lda, CV = TRUE)
lend_lda_2
head(lend_lda_2$class)

#the Maximum a Posteriori Probability (MAP) classification (a factor)
#posterior: posterior probabilities for the classes.
head(lend_lda_2$posterior, 3)

train <- sample(1:150, 75)
lend_lda_3 <- lda(loan_status ~ ., # training model
          Lend_lda)
lend_plda = predict(object = lend_lda_3, # predictions
               newdata = test)
head(lend_plda$class)
head(lend_plda$posterior, 6) # posterior prob.
head(lend_plda$x, 3)
plot(lend_lda)
plot(lend_lda_3)
lend_lda <- lda(loan_status ~ .,
         Lend_lda)
prop_lda = lend_lda$svd^2/sum(lend_lda$svd^2)
lend_plda <- predict(object = lend_lda,
                newdata = Lend_lda)
dataset = data.frame(Defaulters = Lend_lda[,"loan_status"],lda = lend_plda$x)
dataset

#ggplot(dataset) + 
#  geom_point(aes(LD1, colour = loan_status, shape = loan_status), size = 2.5) 

# lets play with accuracy
# lets look at another way to divide a dataset

set.seed(101) # Nothing is random!!
sample_n(Lend_lda,10)

# Lets take a sample of 75/25 like before. Dplyr preserves class. 
training_sample <- sample(c(TRUE, FALSE), nrow(Lend_lda), replace = T, prob = c(0.75,0.25))
train <- Lend_lda[training_sample, ]
test <- Lend_lda[!training_sample, ]

#lets run LDA like before
lda_lend <- lda(loan_status ~ ., train)

# do a quick plot to understand how good the model is
plot(lda_lend, col = as.integer(train$loan_status))

# Sometime bell curves are better
plot(lda_lend, dimen = 1, type = "b")

# This plot shows the essense of LDA. It puts everything on a line and finds cutoffs. 

# Partition plots
#partimat(loan_status ~ ., data=train, method="lda")

# Lets focus on accuracy. Table function
lda_train <- predict(lda_lend)
train$lda <- lda_train$class
table(train$lda,train$loan_status)

# running accuracy on the training set shows how good the model is. It is not an indication of "true" accuracy. We will use the test set to approximate accuracy
lda_test <- predict(lda_lend,test)
test$lda <- lda_test$class
table(test$lda,test$loan_status)



