
### Libraries (if necessary)
library(ggplot2)
library(dplyr)

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
  theme_bw(base_size = 16)
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
setSize = 5:25

matG.star.out = matrix(0, nrow = 21, ncol = dataSize)
matG.star.val = matrix(0, nrow = 21, ncol = dataSize)

for(i in 1:dataSize){
  
  xi = runif(N, -1, 1)
  ei = rnorm(N, 0, 1)
  yi = 0.8 * xi + ei
  
  for (j in 1:length(setSize)){
    
    sizeS = setSize[j]
    sizeTrain = N - sizeS
    
    samp = sort(sample(1:N, sizeS, replace = FALSE))
    
    valData = xi[samp]
    trainData = xi[-samp]
    
    yVal = yi[samp]
    yTrain = yi[-samp]
    
    g1.Mod.i = lm(yTrain ~ 0 + trainData, offset = rep(0.5, sizeTrain))
    b1 = as.numeric(g1.Mod.i$coefficients)
    
    g2.Mod.i = lm(yTrain ~ 0 + trainData, offset = rep(-0.5, sizeTrain))
    b2 = as.numeric(g2.Mod.i$coefficients)
    
    g1.valPred = 0.5 + (b1 * valData)
    g2.valPred = -0.5 + (b2 * valData)
    
    G1.MSE.i = MSE(sizeS, g1.valPred, yVal)
    G2.MSE.i = MSE(sizeS, g2.valPred, yVal)
    
    if(G1.MSE.i < G2.MSE.i){
      
      matG.star.out[j, i] = MSE(sizeTrain, g1.Mod.i$fitted.values, yTrain)
      matG.star.val[j, i] = G1.MSE.i
    }
    
    else {
      matG.star.out[j, i] = MSE(sizeTrain, g2.Mod.i$fitted.values, yTrain)
      matG.star.val[j, i] = G2.MSE.i
    }
  }
}

err.Gstar.out = rowMeans(matG.star.out)
err.Gstar.val = rowMeans(matG.star.val)

errorPlotData = data.frame("SetSize" = setSize,
                           "ValExp" = err.Gstar.val,
                           "OutExp" = err.Gstar.out)

pdf("errorPlot.pdf")
ggplot(errorPlotData, aes(x = SetSize)) +
  geom_line(aes(y = ValExp), color = "black", size = 2, linetype = 1) +
  geom_line(aes(y = rev(OutExp)), color = "red", size = 2, linetype = 1) +
  xlab("Size of Set") + ylab("Expected Error") +
  theme_bw(base_size = 16)
dev.off()

