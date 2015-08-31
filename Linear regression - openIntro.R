## Open into
library(openintro)
data(marioKart)

# Linear regretion with one predicor cond
lin.reg.1 <- lm((totalPr - shipPr) ~ cond, marioKar, )
summary(lin.reg.1)
plot(lin.reg.1)
# We have p-value translate that we can regject the null hypothesis with 92.5% confidence thus the one dimention model doesn't looks good
stripchart((marioKart$totalPr - marioKart$shipPr) ~ marioKart$cond, vertical = TRUE, ylim = c(min(marioKart$totalPr - marioKart$shipPr)-1, 80))
points(lin.reg.1$fitted.values ~ marioKart$cond, pch = 3, col = "red")
abline(lin.reg.1, col = "blue")
mean(marioKart$totalPr[marioKart$cond == "new"] - marioKart$shipPr[marioKart$cond == "new"])
mean(marioKart$totalPr[marioKart$cond == "used"] - marioKart$shipPr[marioKart$cond == "used"])
points(x=2, y=mean(marioKart$totalPr[marioKart$cond == "new"] - marioKart$shipPr[marioKart$cond == "new"]), pch = 1, col = "yellow")

# Doing a linear regression with 4 predictors: cond, stockPhoto, duration and wheels
lin.reg.4 <- lm((totalPr - shipPr) ~ cond + stockPhoto + duration + wheels, marioKart)
summary(lin.reg.4)
plot(lin.reg.4)

marioKart$cond.v <- as.numeric(factor(marioKart$cond, levels = c("new", "used"), labels = c(1, 0)))
marioKart$stockPhoto.v <- as.numeric(factor(marioKart$stockPhoto, levels = c("no", "yes"), labels = c(0, 1)))

cor.mat <- cor(marioKart[,c(2, 3, 5, 6, 7, 9, 11, 13, 14)])
symnum(cor.mat)
cor.test(marioKart$totalPr, marioKart[,c(2, 3, 5, 9, 11, 13, 14)])


names(marioKart)
test <- lm(totalPr-shipPr ~ nBids, marioKart)
plot(test)

#least square approach

