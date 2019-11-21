library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(datasets)
library(cluster)
library(lubridate)
library(hms)
library(dummies)
library(data.table)
library(Hmisc)

# I went to this tutorial for a guide in hierarchical clustering
# https://uc-r.github.io/hc_clustering

# Load in Data
myData <- state.x77
# Remove missing values
myData <- na.omit(myData)

# Trial Run at Clustering 
# Dissimilarity matrix
d <- dist(myData, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)

# Ward's method
ward <- function (myData) {
  d <- dist(myData, method = "euclidean")
  hc5 <- hclust(d, method = "ward.D2" )
  # Cut tree into 4 groups
  sub_grp <- cutree(hc5, k = 4)
  # Number of members in each cluster
  table(sub_grp)
  
  as.data.frame(myData) %>%
    mutate(cluster = sub_grp) %>% 
    head()
  
  plot(hc5, cex = 0.6)
  rect.hclust(hc5, k = 4, border = 2:5)
  
  # Visulaize the clusters in 2d
  fviz_cluster(list(data = myData, cluster = sub_grp))
} 

# Test with Ward's method, unscaled
ward(myData)

# Repeated process with now normailized data 
myData_scaled <- scale(na.omit(myData))
ward(myData_scaled)

# Remove the Area Attribute 
myData_noArea <- select(as.data.frame(myData_scaled), -Area)
ward(myData_noArea)

# Cluster only the Frost Attribute and observe
myData_frost <- select(as.data.frame(myData_scaled), Frost)

d <- dist(myData_frost, method = "euclidean")
hc5 <- hclust(d, method = "ward.D2" )

sub_grp <- cutree(hc5, k = 4)

table(sub_grp)

as.data.frame(myData_frost) %>%
  mutate(cluster = sub_grp) %>% 
  head()

plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 4, border = 2:5)
# Since it's only one dimension, I'm unable to plot it in 2d. 
# The dendrogram makes the data look like it's clustered fairly evenly. 

# Clustering normalized data using kmeans() into 3 clusters
myClusters <- kmeans(myData_scaled, 3)

# Summary of the clusters
summary(myClusters)
# Centers (mean values) of the clusters
myClusters$centers
# Cluster assignments
myClusters$cluster
# Within-cluster sum of squares and total sum of squares across clusters
myClusters$withinss
myClusters$tot.withinss
# Plotting a visual representation of k-means clusters
clusplot(myData_scaled, myClusters$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Using a for loop, repeat the clustering process for k = 1 to 25, and plot the total
# within-cluster sum of squares error for each k-value.
cusPlot <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("k", "Clus_SS"))

for (k in 1:25) {
  myClusters <- kmeans(myData_scaled, k)
  cusPlot[nrow(cusPlot) + 1,] <- c(k,myClusters$tot.withinss)
}

# Visulaize the cluster count  
cusPlot %>% 
  ggplot(aes(x = k, 
             y = Clus_SS)) + 
  geom_point() + 
  geom_path()

# Ideal elbow is between 6-8 
clus_6 <- kmeans(myData_scaled, 6)
clusplot(myData_scaled, clus_6$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# List the states in each cluster.
clus_6$cluster 

# It seems like cluster 5 is made up of the most populated states; California, New York and Florida. 
# That being said, Texas was placed in cluster 6. Cluster 1 is made up of many southern states from 
# the same region of the US. The smaller less populated states appear to be clustered in cluster 3. 
# For some reason, Alaska was consistently placed in it's own cluster by itself.

# Above and beyond is using clustering to explore Earthquake data 

# Read in the data
eq <- read_csv("../data/earthquake.csv")

# Data Wrangling 
eq_clean <- eq %>% 
  mutate(hour = hour(time), 
         min = minute(time),
         wday = wday(time), 
         nst = impute(nst, median), 
         gap = impute(gap, median), 
         dmin = impute(dmin, median), 
         horizontalError = impute(horizontalError, median), 
         magError = impute(magError, median), 
         magNst = impute(magNst, median)) 

eq_clean <- cbind(eq_clean, dummy(eq_clean$magType, sep = "_")) 
eq_clean <- cbind(eq_clean, dummy(eq_clean$net, sep = "_"))
eq_clean <- cbind(eq_clean, dummy(eq_clean$type, sep = "_")) 

eq_clean <- eq_clean %>% 
  select(-time, -magType, -net, -id, -updated, -place, -type, -status, -locationSource, -magSource, -eq_clean_earthquake)

# Scale the data 
eq_cleanscale <- scale(eq_clean) 

eqCusPlot <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("k", "Clus_SS"))

for (k in 1:25) {
  myClusters <- kmeans(eq_cleanscale, k)
  eqCusPlot[nrow(eqCusPlot) + 1,] <- c(k,myClusters$tot.withinss)
}

# Visulaize the cluster count  
eqCusPlot %>% 
  ggplot(aes(x = k, 
             y = Clus_SS)) + 
  geom_point() + 
  geom_path()

# Perhaps 5 clusters is the right way to go. The plot isn't as much of an elbow as it is a lightning bolt. 
myCluster_5 <- kmeans(eq_cleanscale, 5)
# Summary of the clusters
summary(myCluster_5)
# Centers (mean values) of the clusters
myCluster_5$centers
# Cluster assignments
myCluster_5$cluster
clusplot(eq_cleanscale, myCluster_5$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# Cluster 1: longitude, mag, rms, eq_clean_mb
# Cluster 2: latitude, nst, magError
# Cluster 3: depth, mag, rms, horizontalError, magNst, eq_clean_mb
# Cluster 4: latitude, gap, eq_clean
# Cluster 5: mag, dmin, rms, horizontalError, eq_clean_mb


