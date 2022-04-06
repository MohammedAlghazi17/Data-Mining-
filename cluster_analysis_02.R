#install.packages(c("rattle","LICORS","dbscan","ppclust","clusterSim","cluster","factoextra","ggplot2","ggpubr"))

library(rattle) # for the data set
library(LICORS) # for the k-Means++ algorithm
library(clusterSim) # for the Davies-Bouldin Index
library(cluster) # for the Silhouette Coefficient
library(factoextra) # for cluster visualisation
library(ggplot2) # for visualisation of cluster metrics
library(ggpubr) # for visualisation of cluster metrics

# ___________ setup _____________
setwd("C:\\Users\\jorda\\Data-Mining-")
rates_data <- read.csv(file = 'cleaned_data02.csv')

# quick check of data
head(rates_data)
summary(rates_data)

# creating quick data frame to keep selected columns
data_frame = subset(rates_data, select = -c(iso3c, iso2c, adminregion, incomeLevel))
head(data_frame)

rates.new <- data_frame[,-c(1,2)]
rates.class <- data_frame[,5]

head(rates.new)
head(rates.class)

# before clustering, standardize the data in explanatory variables
set.seed(1337) # set seed for reproducibility
rates.new.1 <- sapply(rates.new, FUN=function(x) { scale(x, scale = T, center=T)})
head(rates.new.1)

# apply k means w/ k = 111 (unique values of countries counted in Excel)
res.1 <- kmeans(rates.new.1,50) #111

# check clustering output
str(res.1)

df <- data.frame(
  cluster = factor(res.1$cluster),
  gdp_per_capita = rates_data$GDP_per_capita
)
table(df$cluster, df$gdp_per_capita)

# standardizing this table
rates.new.2 <- sapply(rates.new, FUN=function(x) {scale(x, scale = T, center=F)})
head(rates.new.2)

res.2 <- kmeans(rates.new.2,10) #111
df <- data.frame(
  cluster = factor(res.2$cluster),
  homicides = rates_data$Intentional_homicides_per100k
)
table(df$cluster, df$homicides)

res.2$centers

# plot the clusters and original labels to visually check results
par(mfrow=c(2,2), mar=c(5,4,2,2))
plot(rates.new[c(1,2)], col=res.2$cluster)
plot(rates.new[c(1,2)], col=rates.class)
plot(rates.new[c(3,4)], col=res.2$cluster)
plot(rates.new[c(3,4)], col=rates.class)

library(clusterSim) # for david bouldin index
library(cluster) # for the silhouette

results <- list()
tot.withinss <- c()
betweenss <- c()
dbindex <- c()
silhouettes <- c()
for (k in 3:11) {
  results[[k]] <- kmeans(rates.new, k)
  tot.withinss[k] <- results[[k]]$tot.withinss
  betweenss[k] <- results[[k]]$betweenss
  dbindex[k] <- index.DB(rates.new, results[[k]]$cluster, centrotypes="centroids")$DB
  s <- silhouette(results[[k]]$cluster, daisy(rates.new))
  silhouettes[k] <- mean(s[,3])
}

# having run k-means for values of k between 2-10 we can build a scree plot
par(mfrow=c(2,2))
plot(tot.withinss, xlab="k", type="o")
plot(betweenss, xlab="k", type="o")
plot(dbindex, xlab="k", type = "o")
plot(silhouettes, xlab="k", type="o")

# plot silhouette for visual inspection
plot(silhouette(results[[3]]$cluster, daisy(rates.new)))

# a silhouette width of 0.71-1.0 indicates that a strong structure has been found
# the observations using clusters of <=10 show a strong structure
# indicating a good k value being anything <= 10. 
# re-running with a k value of 10

d <- dist(rates.new, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D") 
plot(fit) # display dendogram
groups <- cutree(fit, k=10) # cut tree into 10 clusters
# draw dendogram with red borders around the 10 clusters 
rect.hclust(fit, k=3, border="red")

# An average silhouette width of 0.9 means
