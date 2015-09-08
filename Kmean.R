library(datasets)
#Load the iris dataset
irisDS <- iris
# "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"
#create factors with numbers for coloring
fact.Species <- factor(irisDS$Species, levels =c("setosa", "versicolor", "virginica"), labels = c(4,5,6)) 

# === Following the class on Coursera: exploratory data analysis ======================
# sepal length and width
k.one <- kmeans(irisDS[,1:2], centers = 3, iter.max = 10, nstart = 10)
plot(irisDS$Sepal.Length, irisDS$Sepal.Width, col = k.one$cluster)   # Plot coloring by cluster
points(k.one$centers, col = 1:3, pch = 3, lwd = 3)   # Plot the centroids
points(irisDS$Sepal.Length, irisDS$Sepal.Width, col = fact.Species, pch = 8)

## For each of the specy box plot the distribution of each variable using boxplot
# length - cluster | length - species
# width - cluster | width - species
opar <- par()
par(mfrow = c(2,2))
boxplot(irisDS$Sepal.Length~irisDS$Species, main = "Sepal Length by Species", ylab = "Length")
boxplot(irisDS$Sepal.Length~k.one$cluster, main = "Sepal Length by Cluster", ylab = "Length")
boxplot(irisDS$Sepal.Width~irisDS$Species, main = "Sepal Width by Species", ylab = "Width")
boxplot(irisDS$Sepal.Width~k.one$cluster, main = "Sepal Width by Cluster", ylab = "Width")
par(opar)

## 2D plot with the cluster with the actual flowers on the left and clusters on the right
par(mfrow = c(1, 2))
plot(irisDS[,1:2], col = fact.Species, main = "By flower")
plot(irisDS[,1:2], col = k.one$cluster, main = "By cluster")
par(opar)

## 1D plot for length only with the cluster with the actual flowers on the left and clusters on the right
par(mfrow = c(1, 2))
stripchart(irisDS$Sepal.Length ~ irisDS$Species, vertical = TRUE, col = unique(fact.Species), main = "By flower")
stripchart(irisDS$Sepal.Length ~ k.one$cluster, vertical = TRUE, col = unique(k.one$cluster), main = "By cluster")
par(opar)

# length - cluster | length - species
# width - cluster | width - species

boxplot(irisDS$Sepal.Length[,1] | irisDS$Species)

# for multiple dimension can use headmap
k.two <- kmeans(irisDS[,1:4], centers = 3, iter.max = 10, nstart = 10)
irisDS.matrix <- as.matrix(irisDS[,1:4])
heatmap(irisDS.matrix)
# Do a matrix plot of the 3 clusters on the 4 dimensions
pairs(irisDS[,1:4], col = (k.two$cluster))

# === Try on my own ======================
#Compute k mean with 3 centers for the sepal length and width
k <- kmeans(irisDS[,1:2], centers = 3, iter.max = 10, nstart = 10)
# Ad the centroid labels to the dataset
irisDS2 <- cbind(irisDS, k[1])


colors <- c("red", "blue", "green")
colors2 <- c("violet", "gray0", "yellow")
centroids <- sort(unique(irisDS2$cluster))

#First needs to jungle with order of the centroid so they get assigned the right color in the plot
cent <- data.frame(0,0)
for (i in 1:3) {
  for (j in 1:2) {
    cent[i,j] = k[[2]][centroids[i],j]
  }
}
#Plot the centroid and setup the canvas
plot(cent, pch = 4, xlab = names(k[[2]][1,][1]), ylab = names(k[[2]][1,][2]), xlim = c(min(irisDS2[,1]), max(irisDS2[,1])), ylim = c(min(irisDS2[,2]), max(irisDS[,2])), col = colors) 
#Plot the clusters
for (i in 1:3) {
  with(subset(irisDS2, cluster == centroids[i]), points(Sepal.Length, Sepal.Width, col = colors[i]))
#  with(subset(irisDS2, cluster == i), points(Sepal.Length, Sepal.Width, col = colors[i]))
}
#Plot the actual group of flowers
flowers <- unique(irisDS2[,5])
for (i in 1:3) {
  with(subset(irisDS2, Species == flowers[i]), points(Sepal.Length, Sepal.Width, pch="*", col = colors2[i]))
}

with(subset(irisDS2, Species == flowers[1]),
     plot(Sepal.Length, Sepal.Width, pch="*", main = "Only the flowers", xlim = c(min(irisDS2[,1]), max(irisDS2[,1])), ylim = c(min(irisDS2[,2]), max(irisDS[,2])), col = colors[1]))
for (i in 2:3) {
  with(subset(irisDS2, Species == flowers[i]), points(Sepal.Length, Sepal.Width, pch="*", col = colors2[i]))
}

#=========================
#Determine teh optimum k based on improvement of sum of squares
n = 10
opar = par()
par(mfrow = c(2,1))

ka <- data.frame(kn=0, k.value=0)
for (i in 2:n) {
  the.k <- kmeans(irisDS[,1:4], centers = i)
  ka[i,1] = i
  ka[i,2] = the.k[5]
}
plot(ka, main = "Total Sum of Squares")

x <- as.numeric(0)
for (i in 3:nrow(ka)) {
  x[i] = ka[i,2] - ka[i-1,2]
}
plot(x[3:nrow(ka)], type="l", main = "Improvement between two iterrations (larger the better)")
suppressWarnings(par(opar))

## looks like optimimum is 2 as this is where there is hte largest improvement 
