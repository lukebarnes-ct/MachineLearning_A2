
### Libraries (if necessary)
library(ggplot2)

set.seed(2023)

##### Question 1.A

## True Underlying Model
N = 30
x = runif(N, -1, 1)
e = rnorm(N, 0, 1)
y = 0.8 * x + e

## Fitted Models

g1.Mod = lm(y ~ 0 + x, offset = rep(0.5, N))

g2.Mod = lm(y ~ 0 + x, offset = rep(-0.5, N))

## Plot true model and overlay fitted models
colVec = c("blue", "red", "black")
lmPlotData = data.frame("X" = x,
                        "Y" = y,
                        "G1" = g1.Mod$fitted.values,
                        "G2" = g2.Mod$fitted.values,
                        "Color" = colVec)

pdf("lmPlot.pdf")
ggplot(lmPlotData, aes(x = X)) +
  geom_line(aes(y = Y), color = "black", size = 1, linetype = 1) +
  geom_line(aes(y = G1), color = "red", size = 2, linetype = 2) +
  geom_line(aes(y = G2), color = "blue", size = 2, linetype = 2) +
  theme_bw()
dev.off()

## Expected Performance using R^2

MSE = function(N, yhat, y){
  (1/N)*sum((yhat - y)^2)
}

summary(g1.Mod)
summary(g2.Mod)

G1.MSE = MSE(N, lmPlotData$G1, y)
G2.MSE = MSE(N, lmPlotData$G2, y)

##### Question 1.B

dataSize = 10000
dataMatX = matrix(0, nrow = 30, ncol = dataSize)
dataMatY = matrix(0, nrow = 30, ncol = dataSize)

for(i in 1:dataSize){
  xi = runif(N, -1, 1)
  dataMatX[, i] = xi
  ei = rnorm(N, 0, 1)
  yi = 0.8 * xi + ei
  dataMatY[, i] = yi
}

setSize = 5:25

for (j in setSize){
  
}


