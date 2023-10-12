rm(list = ls())
### Libraries (if necessary)
library(tidyverse)
library(pracma)
library(colorspace)

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

pdf("dataPlot_Q4.pdf")
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
  
  return(sM)
  #return(list(expZ = expZ, sumZ = sumZ, sM = sM))
}


##### Question 4.C

## Hidden Layer

sig1 = function(z){
  
  tanh(z)
}

## Obj Function

objFunc = function(y, yhat, N){
  
  print(paste0("NAs at: ", which(is.na(yhat), arr.ind = TRUE)))

  error = y * 0
  ind0 = which(y == 0, arr.ind = TRUE)
  ind1 = which(y == 1, arr.ind = TRUE)
  
  error[ind0] = log(1 - yhat[ind0])
  error[ind1] = (y[ind1]) * log(yhat[ind1])
  
  # innerSum = rowSums(error)
  innerSum = rowSums(y*log(yhat) + (1-y)*log(1-yhat))
  
  outerSum = sum(innerSum)
  
  obj = -(1/N) * outerSum
  
  print(paste0("OBJ is: ", obj))
  
  return(obj)
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

  error = objFunc(Y, yHat, N)
  E2 = error + (nu * (sum(W1^2) + sum(W2^2))/N)
  
  return(list(A2 = A2, A1 = A1, E1 = error, E2 = E2,
              W2 = W2, b2 = b2, ones = ones))
}

X = as.matrix(dat[, 1:2])
Y = as.matrix(dat[, 3:5])

nu  = 0
m   = 360/20

p = dim(X)[2]
q = dim(Y)[2]
npars = p*m+m*q+m+q
thetaRand = runif(npars, -1, 1)

obj = function(pars){
  
  resModel = neuralNet(X, Y, pars, m, nu)
  return(resModel$E1)
}

obj(thetaRand)

fitMod = neuralNet(X, Y, thetaRand, m, nu)
sFMat = softMaxMat(t(fitMod$W2)%*%fitMod$A1+fitMod$b2%*%fitMod$ones)

# Fit the neural network using a standard optimizer in R:

resOpt = nlm(obj, thetaRand, iterlim = 1000)
resOpt

### Validation Analysis

set.seed(2022)

N = dim(X)[1]
set = sample(1:N, 0.5*N, replace = FALSE)

XTrain = as.matrix(X[set,])
YTrain = as.matrix(Y[set,])
XVal = as.matrix(X[-set,])
YVal = as.matrix(Y[-set,])

nu = 0.5

objPen = function(pars){
  
  resMod = neuralNet(X, Y, pars, m, nu)
  return(resMod$E2)
}

objPen(thetaRand)

splitter = function(N, K, shuffle = FALSE){
  wind = N %*% K 
  origin = 1:N
  if (shuffle){
    origin = sample(N, N, replace = TRUE)
  }
  
  indexes = matrix(origin, K, wind, byrow = TRUE)
  return(indexes)
}

M = 10
K = 5
indices = splitter(N, K)
valErr = matrix(0, K, M)
nus = exp(seq(-5, 0, length = M))

for (k in 1:K){
  
  setVal = c(indices[k,])
  setTrain = c(indices[-k,])
  XTrain = as.matrix(X[setTrain,])
  YTrain = as.matrix(Y[setTrain,])
  XVal = as.matrix(X[-setVal,])
  YVal = as.matrix(Y[-setVal,])
  
  for (i in 1:M) {
    
    nu = nus[i]
    resOpt = nlm(objPen, theta_rand, iterlim = 500)
    
    resVal = neuralNet(XVal,YVal, resOpt$estimate, m, 0)
    valErr[k, i] = resVal$E1
    
    print(paste0("Validation Run ", i, "| nu =", round(nu, 4)))
    
  }
}

eCV = apply(valErr, 2, mean) 
eCV.sd = apply(valErr, 2, sd) 

plot(eCV~nus, type = "b", main = "Validation Analysis")
segments(nus, eCV, nus, eCV + eCV.sd)
segments(nus, eCV, nus, eCV - eCV.sd)
