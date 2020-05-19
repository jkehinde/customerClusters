# K-Means Clustering

# This a k-means clustering problem puts customers into groups based on 
# annual income and in store points. As the customer buy more things the they will get 
# more points. By putting these customers into their respective clusters, the company is 
# able to target specfic groups when they want to release a new product 

# Importing the dataset
dataset = read.csv('Mall_Customers.csv')
dataset = dataset[4:5] 


# Using the elbow method to find the optimal number of clusters
set.seed(6)
wcss = vector()
# initialized empty vector and with the for loop we'll populate it with the different 
# within sum of squares 
for (i in 1:10) wcss[i] <- sum(kmeans(dataset, i)$withinss) # i is No. of cluster with wcss
plot(1:10,
     wcss,
     type = 'b', # b give us point and lines 
     main = paste('The Elbow Method'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')
# Now we are able to see where the optimal amount of clusters is and  you will see that
# is exactly at 5

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = dataset, centers = 6, iter.max =300, nstart = 10)
y_kmeans = kmeans$cluster

# Visualising the clusters
library(cluster)
clusplot(dataset,
         y_kmeans,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')

# a plot without scaling
plot(x=dataset[,1], y=dataset[,2], col=y_kmeans, pch=19, 
     xlim=c(from=min(dataset[,1]), to=max(dataset[,1]+30)),
     xlab="Annual Income", ylab="Spending Score")
clusters=c("Careless", "Standard", "Sensible", "Target", "Careful")
legend('bottomright', legend=clusters, col=1:5, pch=19, horiz=F)