rm(list = ls())
### Libraries (if necessary)
library(tidyverse)
library(pracma)
library(glmnet)

set.seed(2023)
load("Q2_Data.RData")

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
  geom_point(aes(x = X, y = Y), color = "red", size = 3) +
  labs(x = "X", y = "Y") +
  ylim(-3, 3) +
  theme_bw(base_size = 16)
dev.off()


##### Question 2.B

legendreFunctions = matrix(0, nrow = N, ncol = 1)
legendreFunctions[, 1] = t(legendre(0, x))
for (q in 1:10){
  legendreFunctions = cbind(legendreFunctions, t(legendre(q, x))) 
}

lambda = c(0, 5)

legPred = matrix(0, nrow = N, ncol = 2)

legMod1 = glmnet(legendreFunctions, y, family = "gaussian", 
                  alpha = 0, lambda = lambda[1])
legPred[, 1] = predict(legMod1, legendreFunctions)

legMod2 = glmnet(legendreFunctions, y, family = "gaussian", 
                  alpha = 0, lambda = lambda[2])
legPred[, 2] = predict(legMod2, legendreFunctions)

legPlotData = data.frame("X" = x,
                         "Y" = y,
                         "undX" = xVal,
                         "undY" = yVal,
                         "leg1" = legPred[, 1],
                         "leg2" = legPred[, 2])

pdf("legPlot_Q2.pdf")
ggplot(legPlotData) +
  geom_line(aes(x = undX, y = undY), color = "black", linewidth = 3, linetype = 1) +
  geom_point(aes(x = X, y = Y), color = "black", size = 3) +
  geom_line(aes(x = X, y = leg1), color = "red", linewidth = 2, linetype = 1) +
  geom_line(aes(x = X, y = leg2), color = "blue", linewidth = 2, linetype = 1) +
  labs(x = "X", y = "Y") +
  ylim(-3, 3) +
  theme_bw(base_size = 16)
dev.off()


save.image("Q2_Data.RData")
