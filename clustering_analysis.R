# libraries for use
#install.packages('corrplot')
library(corrplot)

library(cluster)
#install.packages('factoextra')
library(factoextra)

# ___________ setup _____________
setwd("C:\\Users\\jorda\\Data-Mining-")
rates_data <- read.csv(file = 'cleaned_data02.csv')

# quick check of data
head(rates_data)
summary(rates_data)

# creating quick data frame to keep selected columns
data_frame = subset(rates_data, select = -c(iso3c, iso2c, adminregion, incomeLevel))
head(data_frame)

# removing response variable 'year' and store in new variable
rates.new <- data_frame[,-c(1,2)]
rates.class <- data_frame[,1,2]
head(rates.new)

# setting seed + standardise data in explanatory vars
set.seed(1337)
rates.new.1 <- sapply(rates.new, FUN=function(x) {scale(x, scale = T, center=T)})

# check levels (seems to be only Afghan?)
head(rates.class)
head(rates.new.1)

# ________find best k value (optimal # of clusters)______________
# creating correlation matrix between variables for standardized variables
corrmatrix <- cor(rates.new.1)
corrplot(corrmatrix, method = 'number')
head(rates.new.1)

# removing attributes of different scale (year, gdp_us)
df <- rates.new.1[-c(1,4)]

# using silhouette method to find best k value (# of clusters)
silhouette_score <- function(k){
  km <- kmeans(df, centers=k, nstart=25)
  ss <- silhouette(km$cluster, dist(df))
  mean(ss[,3])
}
k <- 5:11
avg_sil <- sapply(k, silhouette_score)
plot(k, type='b', avg_sil, xlab='Number of clusters',
     ylab='Average Silhouette Scores', frame = FALSE)

# the resulting graph shows 7 as the optimal num of clusters ?? 

# ________ clustering __________
km.final <- kmeans(df,7)
# total within cluster Sum of Square (ss)
km.final$tot.withinss

# cluster sizes
km.final$size

rates.new.1$cluster <- km.final$cluster
head(rates.new.1,7)

