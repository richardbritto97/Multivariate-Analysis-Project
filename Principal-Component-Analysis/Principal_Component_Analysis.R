#Loading required libraries
library(data.table)
library(magrittr)
library(stringr)
library(ggplot2)
library(knitr)
library(corrplot)
library(tidyverse)
library(car)
library(factoextra)

### Data Loading

Lending_Data <- read_csv('Lending_Data.csv')
Lend = copy(Lending_Data)
Lend = setDT(Lend)
view(Lend)
str(Lend)
### Data Cleaning

Lend[, member_id := factor(member_id)]
Lend[, loan_status := factor(loan_status)]
Lend[, Default_flag := factor(Default_flag)]

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


#Let us check the correlation of each variable by pairs

mat <- round(cor(Lend[, c(3,5,8,10,14,17,18)], use = "pair"), 2)
corrplot(mat)
mat

# PCA

Lend_PCA = test[, c(7,3,5,8,10,14,18)]
#pairs(PCA_Data)
Lend_PCA
PCA_Data <- prcomp(Lend_PCA[, c(2,3,4,5,6,7)],scale=TRUE)
attach(Lend_PCA)
PCA_Data
summary(PCA_Data)

(eigen_lend <- PCA_Data$sdev^2)
names(eigen_lend) <- paste("PC",1:6,sep="")
eigen_lend
sumlambdas_lend <- sum(eigen_lend)
sumlambdas_lend
propvar_lend <- eigen_lend/sumlambdas_lend
propvar_lend

cumvar_lend <- cumsum(propvar_lend)
cumvar_lend

matlambdas_lend <- rbind(eigen_lend,propvar_lend,cumvar_lend)
matlambdas_lend
rownames(matlambdas_lend) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas_lend,4)
summary(PCA_Data)
print(PCA_Data)

PCA_Data$rotation
PCA_Data$x

# Identifying the scores by their Default flag status

lendtyp_pca <- cbind(data.frame(Default_flag),PCA_Data$x)
lendtyp_pca

# Means of scores for all the Principal Components classified by Default flag status

tabmeansPC_lend <- aggregate(lendtyp_pca[,2:7],by=list(Default_flag=Lend_PCA$Default_flag),mean)
tabmeansPC_lend
tabmeansPC_lend <- tabmeansPC_lend[rev(order(tabmeansPC_lend$Default_flag)),]
tabmeansPC_lend
tabfmeans_lend <- t(tabmeansPC_lend[,-1])
tabfmeans_lend

colnames(tabfmeans_lend) <- t(as.vector(tabmeansPC_lend[1]))
tabfmeans_lend

tabsdsPC_lend <- aggregate(lendtyp_pca[,2:7],by=list(Default_flag=Lend_PCA$Default_flag),sd)
tabfsds_lend <- t(tabsdsPC_lend[,-1])
colnames(tabfsds_lend) <- t(as.vector(tabsdsPC_lend[1]))
tabfsds_lend

t.test(PC1~Lend_PCA$Default_flag,data=lendtyp_pca)
t.test(PC2~Lend_PCA$Default_flag,data=lendtyp_pca)
t.test(PC3~Lend_PCA$Default_flag,data=lendtyp_pca)
t.test(PC4~Lend_PCA$Default_flag,data=lendtyp_pca)
t.test(PC5~Lend_PCA$Default_flag,data=lendtyp_pca)
t.test(PC6~Lend_PCA$Default_flag,data=lendtyp_pca)

var.test(PC1~Lend_PCA$Default_flag,data=lendtyp_pca)
var.test(PC2~Lend_PCA$Default_flag,data=lendtyp_pca)
var.test(PC3~Lend_PCA$Default_flag,data=lendtyp_pca)
var.test(PC4~Lend_PCA$Default_flag,data=lendtyp_pca)
var.test(PC5~Lend_PCA$Default_flag,data=lendtyp_pca)
var.test(PC6~Lend_PCA$Default_flag,data=lendtyp_pca)

(LTPC1 <- leveneTest(PC1~Lend_PCA$Default_flag,data=lendtyp_pca))
(LTPC1 <- leveneTest(PC1~Lend_PCA$Default_flag,data=lendtyp_pca))
(p_PC1_1sided <- LTPC1[[3]][1]/2)
(LTPC2 <- leveneTest(PC2~Lend_PCA$Default_flag,data=lendtyp_pca))
(p_PC2_1sided=LTPC2[[3]][1]/2)
(LTPC3 <- leveneTest(PC3~Lend_PCA$Default_flag,data=lendtyp_pca))
(p_PC3_1sided <- LTPC3[[3]][1]/2)
(LTPC4 <- leveneTest(PC4~Lend_PCA$Default_flag,data=lendtyp_pca))
(p_PC4_1sided <- LTPC4[[3]][1]/2)
(LTPC5 <- leveneTest(PC5~Lend_PCA$Default_flag,data=lendtyp_pca))
(p_PC5_1sided <- LTPC5[[3]][1]/2)
(LTPC6 <- leveneTest(PC6~Lend_PCA$Default_flag,data=lendtyp_pca))
(p_PC6_1sided <- LTPC6[[3]][1]/2)

plot(lendtyp_pca$PC1, lendtyp_pca$PC2,pch=ifelse(lendtyp_pca$Default_flag == 1,1,16),xlab="PC1", ylab="PC2", main="Borrowers against PC1 & PC2")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Defaulter","Nondefaulter"), pch=c(1,16))

plot(eigen_lend, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_lend), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
print(summary(PCA_Data))
View(PCA_Data)
diag(cov(PCA_Data$x))
xlim <- range(PCA_Data$x[,1])
PCA_Data$x[,1]
PCA_Data$x
plot(PCA_Data$x,xlim=xlim,ylim=xlim)
PCA_Data$rotation[,1]
PCA_Data$rotation
plot(Lend_PCA[,-1])
PCA_Data$x
plot(PCA_Data)

fviz_eig(PCA_Data)

# get the original value of the data based on PCA
center <- PCA_Data$center
scale <- PCA_Data$scale
new_lend <- as.matrix(Lend_PCA[,-1])
new_lend
drop(scale(new_lend,center=center, scale=scale)%*%PCA_Data$rotation[,1])
predict(PCA_Data)[,1]

# The aboved two gives us the same thing. predict is a good function to know.
out <- sapply(1:6, function(i){plot(Lend_PCA$Default_flag,PCA_Data$x[,i],xlab=paste("PC",i,sep=""),ylab="Default_flag")})
pairs(PCA_Data$x[,1:6], ylim = c(-6,4),xlim = c(-6,4),panel=function(x,y,...){text(x,y,Lend_PCA$Default_flag)})

# Color by contributions to the Principal Components and avoid text overlapping
fviz_pca_var(PCA_Data, col.var = "contrib", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)             

