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

### Data Loading
Lending_Data <- read_csv('Lending_Data.csv')
Lend = copy(Lending_Data)
Lend = setDT(Lend)
view(Lend)
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
Lend[open_acc >15, 'x' := '15+']
Lend = Lend %>% rename(no_of_acct = x)
str(Lend)

view(Lend)

Lend_fact = Lend[, c(7, 3, 5, 8, 10, 14, 18)]
Lend_fact[, -1]

# Computing Correlation Matrix
corrm_lend <- cor(Lend_fact[, -1])
corrm_lend
plot(corrm_lend)

lend_pca <- prcomp(Lend_fact[, -1], scale = TRUE)
lend_pca
summary(lend_pca)
plot(lend_pca)

# A table containing eigenvalues and %'s accounted, follows. Eigenvalues are the sdev^2
(eigen_lend <- round(lend_pca$sdev^2, 2))
names(eigen_lend) <- paste("PC", 1:6, sep = "")
eigen_lend

sumlambdas_lend <- sum(eigen_lend)
sumlambdas_lend

propvar_lend <- round(eigen_lend/sumlambdas_lend, 2)
propvar_lend

cumvar_lend <- cumsum(propvar_lend)
cumvar_lend

matlambdas_lend <- rbind(eigen_lend, propvar_lend, cumvar_lend)
matlambdas_lend

rownames(matlambdas_lend) <- c("Eigenvalues", "Prop. variance", "Cum. prop. variance")
rownames(matlambdas_lend)
eigvec_lend <- lend_pca$rotation

print(lend_pca)

# Taking the first four PCs to generate linear combinations for all the variables with
# four factors
pcafactors_lend <- eigvec_lend[, 1:4]
pcafactors_lend

# Multiplying each column of the eigenvector’s matrix by the square-root of the 
# corresponding eigenvalue in order to get the factor loadings
unrot_fact_lend <- sweep(pcafactors_lend, MARGIN = 2, lend_pca$sdev[1:4], `*`)
unrot_fact_lend

# Computing communalities
communalities_lend <- rowSums(unrot_fact_lend^2)
communalities_lend

# Performing the varimax rotation. The default in the varimax function is norm=TRUE thus,
# Kaiser normalization is carried out
rot_fact_lend <- varimax(unrot_fact_lend)
View(unrot_fact_lend)
rot_fact_lend

# The print method of varimax omits loadings less than abs(0.1). 
# In order to display all the loadings, it is necessary to ask explicitly the
# contents of the object $loadings
fact_load_lend <- rot_fact_lend$loadings[1:6, 1:4]
fact_load_lend

# Computing the rotated factor scores for the borrowers.
scale_lend <- scale(Lend_fact[, -1])
scale_lend
as.matrix(scale_lend)%*%fact_load_lend%*%solve(t(fact_load_lend)%*%fact_load_lend)

fit_pc <- principal(Lend_fact[,-1], nfactors=4, rotate="varimax")
fit_pc
round(fit_pc$values, 3)
fit_pc$loadings

# Loadings with more digits
for (i in c(1, 3, 2, 4)) { print(fit_pc$loadings[[1, i]])}

# Communalities
fit_pc$communality

# Rotated factor scores, Notice the columns ordering: RC1, RC3, RC2 and RC4
fit_pc$scores

result <- PCA(Lend_fact)

# Play with FA utilities
fa.parallel(Lend_fact[, -1], show.legend = FALSE) # See factor recommendation
fa.plot(fit_pc) # See Correlations within Factors
fa.diagram(fit_pc) # Visualize the relationship
vss(Lend_fact[, -1]) # See Factor recommendations for a simple structure



eigenvals <- eigen(cor(Lend_fact)) # get eigenvalues
par <- parallel(subject = NROW(Lend_fact), var = NCOL(Lend_fact), rep = 100, cent = .05)
Scree <- nScree(x = eigenvals$values, aparallel = par$eigen$qevpea)
plotnScree(Scree)

rot_varimax <- factanal(covmat = corrm_lend, factors = 2,
                        n.obs = NROW(Lend_fact), rotation = "varimax") 
rot_varimax 
