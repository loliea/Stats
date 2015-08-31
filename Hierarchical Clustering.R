# Hierarchical Clustering
seed(12345)
# create two datasets with 100 elements each - will be 100 points with two coordinates
x <- rnorm(n = 50,mean = rep(25, each = 4), sd = 35)
y <- rnorm(n = 50, mean = rep(10, each = 10), sd = 27)
the.label <- as.character(1:50)
plot(x, y, pch = 19) # randomly distributed points along x and y
text(x + 3, y + 3, labels = the.label)

dataset <- data.frame(x = x, y = y, row.names = the.label)
the.dist <- dist(dataset) # calculate the distance
the.clust <- hclust(the.dist, method = "ward.D") # create the clusters
plot(the.clust)

the.matrix <- as.matrix(dataset)
heatmap(the.matrix)

# with two more dimention (4 x 50)
z <- rnorm(n = 50, mean = rep(50, each = 2), sd = 40)
w <- rnorm(n = 50, mean = rep(5, each = 20), sd = 10)
dataset.4x50 <- data.frame(x = x, y = y, z = z, w = w)
the.clust.4x50 <- hclust(dist(dataset.4x50))
plot(the.clust.4x50)
the.matrix.4x50 <- as.matrix(dataset.4x50)
heatmap(the.matrix.4x50)
