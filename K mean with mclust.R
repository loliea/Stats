library(mclust)
mod1 = Mclust(mtcars[,c(1,3,4,6,7)])
summary(mod1)
plot(mod1)


#---------------------------------
#Correlation analysis for mtcars
#---------------------------------
variables <- c(1,3,4,6,7)

car.cor.mat <- matrix(data = 0, nrow = 5, ncol = 5)

car.cor.mat <- data.frame(mpg = 1:5, disp = 0, hp = 0, wt = 0, qsec = 0, row.names = names(mtcars[,variables]))

for (i in 1:length(variables)) {
  for (j in 1:length(variables)) {
    car.cor.mat[i,j] <- cor(mtcars[,i], mtcars[,j])
  }
}
