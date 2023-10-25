rm(list = ls())
### Libraries (if necessary)
library(tidyverse)
library(pracma)
library(colorspace)
library(paletteer)

### Data

set.seed(2023)

dat = read.table("Collider_Data_2022 2.txt", 
                 h = TRUE, stringsAsFactors = TRUE)
dim(dat)

### creating continuous colour palette

colours = c("magenta", "darkblue", "darkorange")
color.gradient = function(x, colors = colours, colsteps = 50){
  
  colpal = colorRampPalette(colors)
  return( colpal(colsteps)[ findInterval(x, seq(min(x), max(x), length = colsteps)) ] )
}

##### Question 4.A
### Plot the coordinates in the feature space and 
### colour-code according to the response.

var = dat %>% 
  gather(Response, flag, Yi1:Yi3) %>%
  filter(flag == 1) %>%
  select(-flag) %>%
  mutate(Response = as.factor(Response))

clPlotData = data.frame("X1" = dat$X1,
                        "X2" = dat$X2)

pdf("Figures/dataPlot_Q4.pdf")
ggplot(clPlotData, aes(x = X1)) +
  geom_point(aes(x = X1, y = X2), color = colours[var$Response], size = 4) +
  labs(x = "X1", y = "X2") +
  ylim(-4.5, 4.5) +
  theme_bw(base_size = 16)
dev.off()


##### Question 4.B

### Write an R-function which evaluates the soft-max activation function in
### matrix form.

### Z is 3 x N

softMaxMat = function(z){
  
  expZ = exp(z)
  sumZ = colSums(expZ)
  matSumZ = rbind(sumZ, sumZ, sumZ)
  
  sM = expZ / matSumZ
  
  rownames(sM) = c("J1", "J2", "J3")
  
  return(sM)
  #return(list(expZ = expZ, sumZ = sumZ, sM = sM))
}


##### Question 4.C

## Hidden Layer

sig1 = function(z){
  
  tanh(z)
}

## Feed Forward Neural Network

neuralNet = function(X, Y, theta, m, nu){
  
  N = dim(X)[1]
  p = dim(X)[2]
  q = dim(Y)[2]
  
  dims = c(p, m, q)
  
  index = 1 : (dims[1]*dims[2])
  W1    = matrix(theta[index], dims[1], dims[2])
  
  index = max(index) + 1 : (dims[2]*dims[3])
  W2    = matrix(theta[index], dims[2], dims[3])
  
  index = max(index) + 1 : (dims[2])
  b1    = matrix(theta[index], dims[2], 1)
  
  index = max(index) + 1 : (dims[3])
  b2    = matrix(theta[index], dims[3], 1)
  
  ones = matrix(1, 1, N)

  A0 = t(X)
  A1 = sig1(t(W1)%*%A0 + b1%*%ones)
  A2 = softMaxMat(t(W2)%*%A1+b2%*%ones)
  
  yHat = t(A2)
  
  error = Y * 0
  ind1 = which(Y == 1, arr.ind = TRUE)
  error[ind1] = log(yHat[ind1])

  E1 = - (1/N) * sum(error)
  E2 = E1 + ((nu/2) * (sum(W1^2) + sum(W2^2))/N)
  
  return(list(A2 = A2, A1 = A1, E1 = E1, E2 = E2))
}

X = as.matrix(dat[, 1:2])
Y = as.matrix(dat[, 3:5])

nu  = 0
# m   = 360/90
m = 9

p = dim(X)[2]
q = dim(Y)[2]
npars = p*m+m*q+m+q
thetaRand = runif(npars, -1, 1)

obj = function(pars){
  
  resModel = neuralNet(X, Y, pars, m, nu)
  return(resModel$E1)
}

obj(thetaRand)

# Fit the neural network using a standard optimizer in R:

resOpt = nlm(obj, thetaRand, iterlim = 1000)
resOpt

### Validation Analysis

set.seed(2022)

N = dim(X)[1]
set = sample(1:N, 0.5*N, replace = FALSE)

XTrain = as.matrix(X[set, ])
YTrain = as.matrix(Y[set, ])
XVal = as.matrix(X[-set, ])
YVal = as.matrix(Y[-set, ])

nu = 0.5

objPen = function(pars){
  
  resMod = neuralNet(X, Y, pars, m, nu)
  return(resMod$E2)
}

objPen(thetaRand)

M = 20
#nus = exp(seq(-11.51293, -2.302585, length = M))
#nus = exp(seq(-8, -2.3, length = M))
nus = exp(seq(log(1e-2), log(5e-1), length = M))
ms = 3:8

valErr = matrix(0, length(ms), M)
inErr = valErr

for (j in 1:length(ms)){
  
  m = ms[j]

  for (i in 1:M) {
      
      nu = nus[i]
      resOpt = nlm(objPen, thetaRand, iterlim = 1000)
      
      resIn = neuralNet(XTrain, YTrain, resOpt$estimate, m, 0)
      inErr[j, i] = resIn$E1
      
      resVal = neuralNet(XVal, YVal, resOpt$estimate, m, 0)
      valErr[j, i] = resVal$E1
      
      print(paste0("Validation Run | m = ", m, " ,", i, "| nu =", round(nu, 4)))
      print(paste0("In Error: ", inErr[j, i]))
      print(paste0("Val Error: ", valErr[j, i]))
      
  }
  
}

valPlotData = data.frame("ValErr3" = valErr[1, ],
                         "ValErr4" = valErr[2, ],
                         "ValErr5" = valErr[3, ],
                         "ValErr6" = valErr[4, ],
                         "ValErr7" = valErr[5, ],
                         "ValErr8" = valErr[6, ],
                         "Nus" = nus)

valPlotData = valPlotData %>%
  pivot_longer(col = starts_with("ValErr"), names_to = "ValErr", names_prefix = "ValErr") %>%
  rename("Nodes" = "ValErr")

pdf("Figures/valPlot_Q4.pdf")
ggplot(valPlotData, aes(x = Nus, y = value, col = Nodes)) +
  geom_line(linewidth = 2) +
  geom_vline(xintercept = nus[which.min(valErr)], linewidth = 0.5, linetype = 2, col = "black") +
  xlab(expression(nu)) +
  ylab("Validation Error") +
  scale_color_paletteer_d("ggthemes::calc") +
  theme_bw(base_size = 16) +
  theme(legend.text = element_text(size = 12),
        legend.key.height = unit(0.5, 'cm'),
        legend.key.width= unit(1.25, 'cm'),
        legend.position = c(0.9, 0.8),
        legend.background = element_rect(fill = "white",
                                         linewidth = 0.6, 
                                         linetype = "solid", 
                                         colour = "black"))
dev.off()


##### Question 4.C

## Regularised Neural Network and corresponding response curve

m = 8
nu = nus[which.min(valErr)]
optRes = nlm(objPen, thetaRand, iterlim = 1000)

M  = 300
x1 = seq(-4, 4,length = M)
x2 = seq(-4, 4,length = M)
xx1 = rep(x1, M)
xx2 = rep(x2, each = M)

XX = cbind(xx1, xx2)
YY = matrix(1, M^2, 3)

regMod = neuralNet(XX, YY, optRes$estimate, m, nu)

predY = round(t(regMod$A2))

xxPlotData = data.frame("X1" = xx1,
                        "X2" = xx2,
                        "Y1" = predY[, 1],
                        "Y2" = predY[, 2],
                        "Y3" = predY[, 3])

yyVar = xxPlotData %>%
  mutate(Response1 = Y1) %>%
  mutate(Response2 = recode(Y2, "1" = 2)) %>%
  mutate(Response3 = recode(Y3, "1" = 3)) %>%
  pivot_longer(col = starts_with("Response"), names_to = "Response", names_prefix = "Response") %>% 
  filter(value == 1 | value == 2 | value == 3) %>%
  mutate(Response = value) %>%
  select(X1, X2, Response)

oldData = var %>%
  mutate(numResponse = recode(Response, "Yi1" = 1, "Yi2" = 2, "Yi3" = 3))

pdf("Figures/responsePlot_Q4.pdf")
ggplot(yyVar, aes(x = X1)) +
  geom_point(aes(x = X1, y = X2), color = color.gradient(yyVar$Response), size = 4) +
  geom_text(data = oldData, aes(x = X1, y = X2, label = numResponse), 
            size = 5, col = "white") +
  labs(x = "X1", y = "X2") +
  ylim(-4.1, 4) +
  theme_bw(base_size = 16)
dev.off()
