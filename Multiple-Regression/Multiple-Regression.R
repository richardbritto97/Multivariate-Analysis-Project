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

### Data Splitting

#Training Testing

## 10% of the sample size
smp_size = floor(0.10 * nrow(Lend))

## set the seed to make our partition reproducible
set.seed(123)
train_ind = sample(seq_len(nrow(Lend)), size = smp_size)

train = Lend[train_ind, ]
test = Lend[-train_ind, ]



#Multiple Regression
Lend_reg = train[, c(7, 3, 5, 8, 10, 14, 18)]
Lend_reg

# Performing multiple regression on Lending dataset
fit <- lm(Default_flag ~ int_rate + dti + annual_inc + loan_amnt, data = Lend_reg)

#show the results
summary(fit)

#Summary has three sections.
#Section1: How well does the model fit the data (before Coefficients). 
#Section2: Is the hypothesis supported? (until sifnif codes). 
#Section3: How well does data fit the model (again).
#Useful Helper Functions

coefficients(fit)

ggpairs(data = Lend_reg, title = "Lending Data")
confint(fit, level = 0.95)

# Predicted Values
fitted(fit)
residuals(fit)

# Anova Table
anova(fit)
vcov(fit)
cov2cor(vcov(fit))

temp <- influence.measures(fit)
temp
View(temp)

# diagnostic plots
plot(fit)

# Assessing Outliers

outlierTest(fit)
qqPlot(fit, main = "QQ Plot")
leveragePlots(fit) # leverage plots

# Influential Observations
# added variable plots
avPlots(fit)

# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4 / ((nrow(Lend_reg) - length(fit$coefficients) - 2))
plot(fit, which = 4, cook.levels = cutoff)

# Influence Plot
influencePlot(fit, 
              id.method = "identify",
              main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance" )

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main = "QQ Plot")

# distribution of studentized residuals
sresid <- studres(fit)
hist(sresid,
     freq = FALSE,
     main = "Distribution of Studentized Residuals")

xfit<-seq(min(sresid), max(sresid), length = 40)
yfit<-dnorm(xfit)
lines(xfit, yfit)

# Non-constant Error Variance
# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)

# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)

# Multi-collinearity
# Evaluate Collinearity
vif(fit) # variance inflation factors
sqrt(vif(fit)) > 2 # problem?

# Nonlinearity
# component + residual plot
crPlots(fit)

# Ceres plots
ceresPlots(fit)

# Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit)

# Global test of model assumptions
gvmodel <- gvlma(fit)
summary(gvmodel)
fit
summary(fit)
fit1 <- fit
fit2 <- lm(Default_flag ~ int_rate + annual_inc + loan_amnt, data = Lend_reg)

# compare models
anova(fit1, fit2)
step <- stepAIC(fit, direction = "both")
step$anova # display results


leaps<-regsubsets(Default_flag ~ int_rate + dti + No_of_Enquiry + 
                    annual_inc + open_acc + loan_amnt,
                    data = Lend_reg, nbest = 10)

# view results
summary(leaps)

# plot a table of models showing variables in each model.
# models are ordered by the selection statistic.
plot(leaps)
plot(leaps, scale = "r2")

# All Subsets Regression
plot(leaps, scale = "bic")
summary(leaps)
View(leaps)
leaps
coef(leaps, 1:6)

# Calculate Relative Importance for Each Predictor
calc.relimp(fit, type = c("lmg", "last", "first", "pratt"),
            rela = TRUE)

# Bootstrap Measures of Relative Importance (1000 samples)
boot <- boot.relimp(fit, 
                    b = 1000,
                    type = c("lmg", "last", "first", "pratt"),
                    rank = TRUE,
                    diff = TRUE,
                    rela = TRUE)

booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result

summary(fit)

(predict.lm(fit, data.frame(int_rate = 12 ,dti=23,annual_inc=60000,loan_amnt=150000) ))
