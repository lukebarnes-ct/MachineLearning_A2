rm(list = ls())
### Libraries (if necessary)
library(tidyverse)
library(pracma)
library(glmnet)

set.seed(2023)

##### Question 2.A

## True Underlying Model
N = 50
x = runif(N, -1, 1)
e = rnorm(N, 0, 1)
y = sin(pi * x) + e

xVal = seq(-1, 1, length = N)
yVal = sin(pi * xVal)

lmPlotData = data.frame("X" = x,
                        "Y" = y,
                        "undX" = xVal,
                        "undY" = yVal)

pdf("modPlot_Q2.pdf")
ggplot(lmPlotData, aes(x = undX)) +
  geom_line(aes(y = undY), color = "black", linewidth = 3, linetype = 1) +
  geom_point(aes(x = X, y = Y), color = "red", size = 4) +
  labs(x = "X", y = "Y") +
  ylim(-3, 3) +
  theme_bw(base_size = 16)
dev.off()


##### Question 2.B

legMat = t(legendre(10, x))
lambda = c(0, 5)

legPred = matrix(0, nrow = N, ncol = 2)

legMod1 = glmnet(legMat, y, family = "gaussian", 
                  alpha = 0, lambda = lambda[1])
legPred[, 1] = predict(legMod1, legMat)

legMod2 = glmnet(legMat, y, family = "gaussian", 
                  alpha = 0, lambda = lambda[2])
legPred[, 2] = predict(legMod2, legMat)

legPlotData = data.frame("X" = x,
                         "Y" = y,
                         "undX" = xVal,
                         "undY" = yVal,
                         "leg1" = legPred[, 1],
                         "leg2" = legPred[, 2])

pdf("legPlot_Q2.pdf")
ggplot(legPlotData) +
  geom_line(aes(x = undX, y = undY), color = "black", linewidth = 3, linetype = 1) +
  geom_point(aes(x = X, y = Y), color = "black", size = 4) +
  geom_line(aes(x = X, y = leg1), color = "red", linewidth = 2, linetype = 1) +
  geom_line(aes(x = X, y = leg2), color = "blue", linewidth = 2, linetype = 1) +
  labs(x = "X", y = "Y") +
  ylim(-3, 3) +
  theme_bw(base_size = 16)
dev.off()

##### Question 2.C

kFoldSeq = seq(5, 50, by = 5)

#kFoldMat = matrix(0, nrow = 5, ncol = 10)
kFoldMat = list()
valPredMat = matrix(0, nrow = 5, ncol = 10)
#trainSet = matrix(0, nrow = 45, ncol = 10)
trainSet = list()
trainPredMat = matrix(0, nrow = 45, ncol = 10)

for (k in 1:10){
  ind = (kFoldSeq[k]-4):kFoldSeq[k]
  kFoldMat[[k]] = legMat[ind,]
  valPredMat[, k] = y[ind]
  trainSet[[k]] = legMat[-ind,]
  trainPredMat[, k] = y[-ind]
}

lambdas = seq(0.1, 10, length = 200)
cvError = c()

for (c in 1:length(lambdas)){
  
  avgErr = c()
  
  for(d in 1:10){
    
    legMod = glmnet(trainSet[[d]], trainPredMat[, d], family = "gaussian", 
                     alpha = 0, lambda = lambdas[c])
    
    valPred = predict(legMod, kFoldMat[[d]])
    avgErr[d] = mean((valPred - valPredMat[, d])^2)
  }
  
  cvError[c] = mean(avgErr)
}

cvPlotData = data.frame("Lambdas" = lambdas,
                        "CVError" = cvError)

pdf("cvPlot_Q2.pdf")
ggplot(cvPlotData) +
  geom_line(aes(x = Lambdas, y = CVError), color = "black", 
            linewidth = 3, linetype = 1) +
  labs(x = expression(lambda), y = "CV Error") +
  theme_bw(base_size = 16)
dev.off()

lambdaFit = lambdas[which.min(cvError)]

legFitMod = glmnet(legMat, y, family = "gaussian", 
                   alpha = 0, lambda = lambdaFit)
legFitPred = predict(legFitMod, legMat)

legFitPlotData = data.frame("s0" = legFitPred,
                            "X" = x,
                            "Y" = y,
                            "undX" = xVal,
                            "undY" = yVal)

pdf("legFitPlot_Q2.pdf")
ggplot(legFitPlotData) +
  geom_line(aes(x = undX, y = undY), color = "black", linewidth = 3, linetype = 1) +
  geom_point(aes(x = X, y = Y), color = "black", size = 4) +
  geom_line(aes(x = X, y = s0), color = "red", linewidth = 3, linetype = 1) +
  labs(x = "X", y = "Y") +
  ylim(-3, 3) +
  theme_bw(base_size = 16)
dev.off()
