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
set.seed(52296)
myCluster_5 <- kmeans(eq_cleanscale, 5)
# Summary of the clusters
summary(myCluster_5)
# Centers (mean values) of the clusters
myCluster_5$centers
# Cluster assignments
myCluster_5$cluster
clusplot(eq_cleanscale, myCluster_5$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

View(myCluster_5)

Clus <- as.data.frame(cbind(eq,myCluster_5$cluster))

Clus1 <- as.data.frame(cbind(eq,myCluster_5$cluster)) %>% 
  filter(myCluster_5$cluster == 1)

Clus2 <- as.data.frame(cbind(eq,myCluster_5$cluster)) %>% 
  filter(myCluster_5$cluster == 2)

Clus3 <- as.data.frame(cbind(eq,myCluster_5$cluster)) %>% 
  filter(myCluster_5$cluster == 3)

Clus4 <- as.data.frame(cbind(eq,myCluster_5$cluster)) %>% 
  filter(myCluster_5$cluster == 4)

Clus5 <- as.data.frame(cbind(eq,myCluster_5$cluster)) %>% 
  filter(myCluster_5$cluster == 5)
