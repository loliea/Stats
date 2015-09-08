set.seed(12345)
##rep(0.2,4)
# Generate a matrix of 10 cols and 40 rows
dataMatrix <- matrix(rnorm(400), nrow = 40)
# Reorganize the matrix so it is displayed as in a matrix (starting top left) - for that need to transpose first and reogranize the columns
# Also rename the axis label to match rows and col of matrix
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)
## We don't see real order to this, not very interesting

## Now we are going to introduce a pattern to it
dataMatrix1 <- dataMatrix
## repurpose the previous random matrix by adding to each row where rbinom is 1 the sequence 0 0 0 0 0 3 3 3 3 3
set.seed(678910)
for (i in 1:40) {
  # flip a coin
  coinFlip <- rbinom(1, size = 1, prob = 0.5)
  # if coin is heads add a common pattern to that row
  if (coinFlip) {
    dataMatrix1[i, ] <- dataMatrix1[i, ] + rep(c(0, 3), each = 5)
  }
}
# Then print the image and heatmap of the matrix
# We can see in these a pattern as rows that are coinFlip have an incrate of 3 in the last 5 column (lighter color = higher value)
image(1:10, 1:40, t(dataMatrix1)[,nrow(dataMatrix1):1])
heatmap(dataMatrix1)

# Now with this dataset we create a cluster to group the columns that are similar
theClust <- hclust(dist(dataMatrix1))
# We reorder the matrix by group of cluster
dataMatrix1.ordered <- dataMatrix1[theClust$order,]
image(1:10, 1:40, t(dataMatrix1.ordered)[,nrow(dataMatrix1.ordered):1]) # we now clearly see a pattern
# We create the SVD scaling the ordered matrix to normalize the data first
svd.mat1 <- svd(scale(dataMatrix1.ordered))
plot(svd.mat1$d)
# we see that the first element/singular value of d account for a big proportion of explanation of the variance of the matrix
plot(svd.mat1$d^2/sum(svd.mat1$d^2)) # using variance we see that the first value account for 40% of total variance
# the corresponding column of the first principal component of u and v are
plot(svd.mat1$u[,1])  # this is the first left singular vector
plot(svd.mat1$v[,1])  # this is the first rigth singular vector
# We see that there is a true pattern for these first columns where the singular value explain a lot of the variacne

#We can also use PCA to directly get the principal component of the matrice
pca.mat1 <- prcomp(dataMatrix1, scale = TRUE)
summary(pca.mat1) # we see in the second row the proportion of variance of the components. The first one is


#==================
sample(1:150,150)
#transform iris data set in matrix shuffing the rows (otherwith they are already grouped by flower)
iris.Matrix <- as.matrix(iris[sample(1:150,150),1:4])
pca.iris <- prcomp(iris.Matrix, scale = TRUE)
summary(pca.iris) ## 73% of varaince explained by first principal component
iris.clust <- hclust(dist(iris.Matrix))
image(scale(iris.Matrix[iris.clust$order,])) # there is a pattern
#when not ordering by cluster we don't see anything
image(scale(iris.Matrix))
#Extracting the left and right singular vectors
iris.svd <- svd(scale(iris.Matrix))
plot(iris.svd$u[,1])
plot(iris.svd$v[,1])
## when ploting the first left and righ singular vector we clearly see a pattern
