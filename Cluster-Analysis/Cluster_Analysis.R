
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

Lend[open_acc %in% c(1,2,3,4,5), 'x' := 'LT5']
Lend[open_acc %in% c(6,7,8,9,10), 'x' := '6-10']
Lend[open_acc %in% c(11,12,13,14,15), 'x' := '11-15']
Lend[open_acc >15, 'x' := '15+']
Lend = Lend %>% rename(no_of_acct = x)
str(Lend)

view(Lend)



# Clustering for purpose at which the loan was taken

vals = c('int_rate', 'dti', 'No_of_Enquiry', 'annual_inc',
         'open_acc', 'delinq_2yrs', 'loan_amnt')

Lend_purpose = Lend[, c(13, 3, 5, 8, 10, 14, 17, 18)]

Lend_purpose = Lend_purpose %>%
  group_by(purpose)  %>% 
  summarise_at(vals, mean, na.rm = TRUE)

setDT(Lend_purpose)

Lend_purpose = Lend_purpose %>% 
  remove_rownames %>% 
  column_to_rownames(var = "purpose")

matstd_Lend_purpose = scale(Lend_purpose)


# Creating a (Euclidean) distance matrix of the standardized data 
dist_Lend_purpose <- dist(matstd_Lend_purpose, method = "euclidean")

# Invoking hclust command (cluster analysis by single linkage method)      
cluspurpose_nn <- hclust(dist_Lend_purpose, method = "single") 

# Plotting vertical dendrogram      
# create extra margin room in the dendrogram, on the bottom 

#For Nearest Neighbor - Single Linkage:
par(mar = c(8, 4, 2, 1) + 0.1)
plot(as.dendrogram(cluspurpose_nn), 
     ylab = "Distance between Loan Purpose",
     ylim = c(0,5.5), 
     main = "Dendrogram - Nearest Neighbor")


#For Farthest Neighbor - Complete Linkage:
cluspurpose_fn <- hclust(dist_Lend_purpose)
plot(as.dendrogram(cluspurpose_fn),
     ylab = "Distance between Loan Purpose",
     main = "Dendrogram - Farthest Neighbor")

#For Average Linkage:
cluspurpose_avl <- hclust(dist_Lend_purpose,method="average")
plot(as.dendrogram(cluspurpose_avl),
     ylab = "Distance between Loan Purpose",
     main = "Dendrogram - Average Linkage")

#Horizontal Nearest Neightbore for better interpretation:
par(mar = c(4, 1, 2, 8) + 0.1)
plot(as.dendrogram(cluspurpose_nn), 
     xlab = "Distance between Loan Purpose",
     horiz = TRUE, 
     main = "Dendrogram Horizontal")


# We will use agnes function as it allows us to select option for data standardization, the distance measure and clustering algorithm in one single function

(agn_purpose <- agnes(Lend_purpose, metric="euclidean", stand=TRUE, method = "single"))
View(agn_purpose)


#  Description of cluster merging
agn_purpose$merge

#Dendrogram
par(mar = c(8, 1, 2, 2) + 0.1)
#plot(as.dendrogram(agn_purpose,
 #                  xlab = "Distance between Loan Purpose",
  #                 horiz = TRUE, main = "Dendrogram"))

#Interactive Plots
par(mar = c(1, 1, 2, 2) + 0.1)

plot(agn_purpose,ask=TRUE) 
plot(agn_purpose, which.plots = 1)
plot(agn_purpose, which.plots = 2)


# K-Means Clustering
# K-means, k=2, 3, 4, 5, 6
# Centers (k's) are numbers thus, 10 random sets are chosen
(kmeans2_purpose <- kmeans(matstd_Lend_purpose, 1, nstart = 10))


# Computing the percentage of variation accounted for. Two clusters
perc_var_2 <- round(100*(1 - kmeans2_purpose$betweenss/kmeans2_purpose$totss),1)
names(perc_var_2) <- "Perc. 2 clus"
perc_var_2

# Computing the percentage of variation accounted for. Three clusters
(kmeans3_purpose <- kmeans(matstd_Lend_purpose, 3, nstart = 10))
perc_var_3 <- round(100*(1 - kmeans3_purpose$betweenss/kmeans3_purpose$totss), 1)
names(perc_var_3) <- "Perc. 3 clus"
perc_var_3

# Computing the percentage of variation accounted for. Four clusters
kmeans4_purpose <- kmeans(matstd_Lend_purpose, 4, nstart = 10)
perc_var_4 <- round(100*(1 - kmeans4_purpose$betweenss/kmeans4_purpose$totss), 1)
names(perc_var_4) <- "Perc. 4 clus"
perc_var_4

# Computing the percentage of variation accounted for. Five clusters
(kmeans5_purpose <- kmeans(matstd_Lend_purpose, 5, nstart = 10))
perc_var_5 <- round(100*(1 - kmeans5_purpose$betweenss/kmeans5_purpose$totss), 1)
names(perc_var_5) <- "Perc. 5 clus"
perc_var_5

# Computing the percentage of variation accounted for. Six clusters
(kmeans6_purpose <- kmeans(matstd_Lend_purpose, 6, nstart = 10))
perc_var_6 <- round(100*(1 - kmeans6_purpose$betweenss/kmeans6_purpose$totss), 1)
names(perc_var_6) <- "Perc. 6 clus"
perc_var_6
#
# Saving four k-means clusters in a list
clus_1 <- matrix(names(kmeans4_purpose$cluster[kmeans4_purpose$cluster == 1]), 
            ncol = 1,
            nrow = length(kmeans4_purpose$cluster[kmeans4_purpose$cluster == 1]))
colnames(clus_1) <- "Cluster 1"

clus_2 <- matrix(names(kmeans4_purpose$cluster[kmeans4_purpose$cluster == 2]), 
            ncol = 1,
            nrow = length(kmeans4_purpose$cluster[kmeans4_purpose$cluster == 2]))
colnames(clus_2) <- "Cluster 2"

clus_3 <- matrix(names(kmeans4_purpose$cluster[kmeans4_purpose$cluster == 3]), 
            ncol = 1,
            nrow = length(kmeans4_purpose$cluster[kmeans4_purpose$cluster == 3]))
colnames(clus_3) <- "Cluster 3"

clus_4 <- matrix(names(kmeans4_purpose$cluster[kmeans4_purpose$cluster == 4]), 
            ncol = 1,
            nrow = length(kmeans4_purpose$cluster[kmeans4_purpose$cluster == 4]))
colnames(clus_4) <- "Cluster 4"

list(clus_1, clus_2, clus_3, clus_4)

fviz_cluster(kmeans4_purpose, data = Lend_purpose)

fviz_nbclust(Lend_purpose, kmeans, method = "wss")

fviz_nbclust(Lend_purpose, kmeans, method = "silhouette")

gap_stat <- clusGap(Lend_purpose, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


####################

vals = c('int_rate', 'dti', 'No_of_Enquiry', 'annual_inc',
         'open_acc', 'delinq_2yrs', 'loan_amnt')

Lend_kmeans = Lend[, c(1, 3, 5, 8, 10, 14, 17, 18)]

setDT(Lend_kmeans)
Lend_kmeans

Lend_kmeans = Lend_kmeans %>% 
  remove_rownames %>% 
  column_to_rownames(var = "member_id")

matstd_Lend_kmeans = scale(Lend_kmeans)
matstd_Lend_kmeans

# Computing the percentage of variation accounted for. Two clusters
(kmeans2_lend_kmeans <- kmeans(matstd_Lend_kmeans, 2, nstart = 10))
perc_var_2 <- round(100*(1 - kmeans2_lend_kmeans$betweenss/kmeans2_lend_kmeans$totss), 1)
names(perc_var_2) <- "Perc. 2 clus"
perc_var_2

# Computing the percentage of variation accounted for. Three clusters
(kmeans3_lend_kmeans <- kmeans(matstd_Lend_kmeans, 3, nstart = 10))
perc_var_3 <- round(100*(1 - kmeans3_lend_kmeans$betweenss/kmeans3_lend_kmeans$totss), 1)
names(perc_var_3) <- "Perc. 3 clus"
perc_var_3

# Computing the percentage of variation accounted for. Four clusters
(kmeans4_lend_kmeans <- kmeans(matstd_Lend_kmeans, 4, nstart = 10))
perc_var_4 <- round(100*(1 - kmeans4_lend_kmeans$betweenss/kmeans4_lend_kmeans$totss), 1)
names(perc_var_4) <- "Perc. 4 clus"
perc_var_4

# Computing the percentage of variation accounted for. Five clusters
(kmeans5_lend_kmeans <- kmeans(matstd_Lend_kmeans, 5, nstart = 10))
perc_var_5 <- round(100*(1 - kmeans5_lend_kmeans$betweenss/kmeans5_lend_kmeans$totss), 1)
names(perc_var_5) <- "Perc. 5 clus"
perc_var_5

# Computing the percentage of variation accounted for. Six clusters
(kmeans6_lend_kmeans <- kmeans(matstd_Lend_kmeans, 6, nstart = 10))
perc_var_6 <- round(100*(1 - kmeans6_lend_kmeans$betweenss/kmeans6_lend_kmeans$totss), 1)
names(perc_var_6) <- "Perc. 6 clus"
perc_var_6
#
# Saving four k-means clusters in a list
clus_1 <- matrix(names(kmeans4_lend_kmeans$cluster[kmeans4_lend_kmeans$cluster == 1]), 
              ncol = 1,
              nrow = length(kmeans4_lend_kmeans$cluster[kmeans4_lend_kmeans$cluster == 1]))
colnames(clus_1) <- "Cluster 1"

clus_2 <- matrix(names(kmeans4_lend_kmeans$cluster[kmeans4_lend_kmeans$cluster == 2]), 
              ncol = 1,
              nrow = length(kmeans4_lend_kmeans$cluster[kmeans4_lend_kmeans$cluster == 2]))
colnames(clus_2) <- "Cluster 2"

clus_3 <- matrix(names(kmeans4_lend_kmeans$cluster[kmeans4_lend_kmeans$cluster == 3]), 
              ncol = 1,
              nrow = length(kmeans4_lend_kmeans$cluster[kmeans4_lend_kmeans$cluster == 3]))
colnames(clus_3) <- "Cluster 3"

clus_4 <- matrix(names(kmeans4_lend_kmeans$cluster[kmeans4_lend_kmeans$cluster == 4]), 
              ncol = 1,
              nrow = length(kmeans4_lend_kmeans$cluster[kmeans4_lend_kmeans$cluster == 4]))
colnames(clus_4) <- "Cluster 4"

list(clus_1, clus_2, clus_3, clus_4)

fviz_cluster(kmeans2_lend_kmeans, data = Lend_kmeans)

## set the seed to make our partition reproducible
set.seed(123)
smp_size = floor(0.10 * NROW(Lend_kmeans))
train = Lend_kmeans[sample((NROW(Lend_kmeans)), size = smp_size),]
train

#Interpreting optimal number of clusters using wss method:
fviz_nbclust(train, kmeans, method = "wss")

#Interpreting optimal number of clusters using silhouette method:
fviz_nbclust(train, kmeans, method = "silhouette")

#Interpreting optimal number of clusters using gap stat method:
suppressWarnings(gap_stat <- clusGap(train, FUN = kmeans, nstart = 25, K.max = 10, B = 50))
fviz_gap_stat(gap_stat)




