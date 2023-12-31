rm(list = ls())
### Libraries (if necessary)
library(tidyverse)
library(tictoc)
library(pracma)

set.seed(2023)

##### Question 1.A

## True Underlying Model
N = 30
x = runif(N, -1, 1)
e = rnorm(N, 0, 1)
y = 0.8 * x + e

xVal = seq(-1, 1, length = N)
yVal = 0.8 * xVal

## Fitted Models

g1.Mod = lm(y ~ 0 + x, offset = rep(0.5, N))

g2.Mod = lm(y ~ 0 + x, offset = rep(-0.5, N))

## Plot true model and overlay fitted models
colVec = c("blue", "red", "black")
lmPlotData = data.frame("X" = x,
                        "Y" = y,
                        "undX" = xVal,
                        "undY" = yVal,
                        "G1" = g1.Mod$fitted.values,
                        "G2" = g2.Mod$fitted.values,
                        "Color" = colVec)

pdf("Figures/lmPlot.pdf")
ggplot(lmPlotData, aes(x = undX)) +
  geom_point(aes(x = X, y = Y), color = "black", size = 3) +
  geom_line(aes(y = undY), color = "black", linewidth = 2, linetype =1) +
  geom_line(aes(x = X, y = G1), color = "red", linewidth = 2, linetype = 2) +
  geom_line(aes(x = X, y = G2), color = "blue", linewidth = 2, linetype = 2) +
  theme_bw(base_size = 16) +
  labs(x = "X", y = "Y")
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

f = function(x){
  eps = rnorm(length(x), 0, 1)
  y = 0.8 * x + eps
  return(y)
}

outIntegral = function(x, g1, beta){
  
  if (g1 == TRUE){
    g = 0.5 + (beta * x)
  }
  
  else {
    g = -0.5 + (beta * x)
  }
  
  f = f(x)
  
  int = (g - f)^2 * 0.5
  return(int)
}

a = -1
b = 1
n = 100000

for(i in 1:dataSize){
  
  xi = runif(N, -1, 1)
  yi = f(xi)
  
  tic()
  
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
      
      matG.star.val[j, i] = G1.MSE.i
      
      xsam = runif(n, a, b)
      int = (b-a) * mean(outIntegral(xsam, TRUE, b1))
      matG.star.out[j, i] = int

      # matG.star.out[j, i] = integrate(outIntegral, lower = -1, upper = 1, TRUE, b1)$value
      # matG.star.out[j, i] = quadl(outIntegral, xa = -1, xb = 1, g1 = TRUE, beta = b1)
    }
    
    else {
      
      matG.star.val[j, i] = G2.MSE.i
      
      xsam = runif(n, a, b) 
      int = (b-a) * mean(outIntegral(xsam, FALSE, b2))
      matG.star.out[j, i] = int

      # matG.star.out[j, i] = integrate(outIntegral, lower = -1, upper = 1, FALSE, b2)$value
      # matG.star.out[j, i] = quadl(outIntegral, xa = -1, xb = 1, g1 = FALSE, beta = b2)
      
    }
  }
  
  toc()
}

err.Gstar.out = rowMeans(matG.star.out)
err.Gstar.val = rowMeans(matG.star.val)

errorPlotData = data.frame("SetSize" = setSize,
                           "ValExp" = err.Gstar.val,
                           "OutExp" = err.Gstar.out)

pdf("Figures/errorPlot.pdf")
ggplot(errorPlotData, aes(x = SetSize)) +
  geom_smooth(aes(y = ValExp), color = "black", linewidth = 2, linetype = 1, se = F) +
  # geom_line(aes(y = ValExp), color = "black", linewidth = 2, linetype = 1) +
  geom_smooth(aes(y = OutExp), color = "red", linewidth = 2, linetype = 1, se = F) +
  # geom_line(aes(y = OutExp), color = "red", linewidth = 2, linetype = 1) +
  xlab("Size of Validation Set") + ylab("Expected Error") +
  theme_bw(base_size = 16) 
dev.off()
