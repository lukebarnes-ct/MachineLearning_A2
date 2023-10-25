
library(orthopolynom)
##### Scrap Coding for Assignment Questions

dataMatX = matrix(0, nrow = 30, ncol = dataSize)
dataMatY = matrix(0, nrow = 30, ncol = dataSize)

xi = runif(N, -1, 1)
dataMatX[, i] = xi
ei = rnorm(N, 0, 1)
yi = 0.8 * xi + ei
dataMatY[, i] = yi

for (j in setSize){
  dat = data.frame(xi)
  
  dat$id = 1:nrow(dat)
  
  valData = dat %>% slice_sample(j, replace = FALSE)
  trainData = anti_join(dat, valData, by = "id")
  
  g1.Mod.i = lm(y ~ 0 + trai, offset = rep(0.5, N))
  
  g2.Mod.i = lm(y ~ 0 + x, offset = rep(-0.5, N))
}

randX = runif(N, -1, 1)
as.numeric(g1.Mod$coefficients) 

plot(err.Gstar.out ~ setSize, type = "l", ylim = c(0.75, 1.5))
lines(err.Gstar.val ~ setSize, col = "red")


## Find Beta coefficients using Least Squares
legMat = lgF
legMat = xxx
ones = diag(1, dim(legMat)[2])

betaMat = matrix(0, nrow = dim(legMat)[2], ncol = 2)

for (b in 1:2){
  betaMat[, b] = (solve(t(legMat) %*% legMat + 
                          (lambda[b] * ones)) %*% t(legMat) %*% y)
}

betas = (solve(t(legMat) %*% legMat + 
                 (lambda[1] * ones)) %*% t(legMat) %*% y)

yhat1 = as.matrix(x) %*% t(as.matrix(betaMat[, 1]))
yhat1 = rowSums(yhat1)

yhat2 = as.matrix(x) %*% t(as.matrix(betaMat[, 2]))
yhat2 = rowSums(yhat2)

t(legMat[, 2]) %*% legMat[, 3]

dot(legMat[, 1], legMat[, 2])

betas = t(legendreFunctions) %*% legendreFunctions + 
  (lambda[2] * ones)


trainLeg = legFunc(trainSet[, 1], 10, 45)
legMod = glmnet(trainLeg, trainPredMat, family = "gaussian", 
                alpha = 0, lambda = lambdas[1])

plot(lmPlotData$undX, lmPlotData$undY, type = "l", ylim = c(-3, 3))
points(lmPlotData$X, lmPlotData$Y, col = "red")
lines(lmPlotData$X, yhat1, col = "blue")
lines(lmPlotData$X, yhat2, col = "green")

### Legendre Function

legFunc = function(x, Q, N){
  
  legendreFunctions = matrix(0, nrow = N, ncol = 1)
  legendreFunctions[, 1] = t(legendre(0, x))
  
  for (q in 1:Q){
    legendreFunctions = cbind(legendreFunctions, t(legendre(q, x))) 
  }
  
  return(legendreFunctions)
}

legendreFunctions = legFunc(x, 10, N)

legFunc = function(x, q){
  
  legFuncMat = matrix(0, nrow = 50, ncol = q+1)
  
  for (k in 0:q){
    
    val = (q+k-1)/2
    
    legSum = (x^k) * (factorial(q)/(factorial(k)*factorial(q-k))) * (factorial(val)/(factorial(q)*factorial(val-q)))
    legFuncMat[, k+1] = legSum
  }
  
  return((2^q) * sum(legFuncMat))
}

xxx = legFunc(x, 10)

legPoly = legendre.polynomials(10, normalized = TRUE)
legPoly

o2 = -0.5 + 1.5*x^2
legPoly[[4]]

lgF = matrix(0, nrow = 50, ncol = 11)

for (lg in 1:11){
  func = legPoly[[lg]]
  lgF[, lg] = lapply(x, as.function(func))
}

lgF[, 1] = 0.7071068 * rep(1, 50)
lgF[, 2] = 1.224745*x
lgF[, 3] = -0.7905694 + 2.371708*x^2 
lgF[, 4] = -2.806243*x + 4.677072*x^3 
lgF[, 5] = 0.7954951 - 7.954951*x^2 + 9.280777*x^4
lgF[, 6] = 4.397265*x - 20.52057*x^3 + 18.46851*x^5 
lgF[, 7] = -0.7967218 + 16.73116*x^2 - 50.19347*x^4 + 36.80855*x^6 
lgF[, 8] = -5.990715*x + 53.91644*x^3 - 118.6162*x^5 + 73.42906*x^7
lgF[, 9] = 0.7972005 - 28.69922*x^2 + 157.8457*x^4 - 273.5992*x^6 + 146.571*x^8
lgF[, 10] = 7.585119*x - 111.2484*x^3 + 433.8688*x^5 - 619.8126*x^7 + 292.6893*x^9
lgF[, 11] = -0.7974349 + 43.85892*x^2 - 380.1106*x^4 + 1140.332*x^6 - 1384.689*x^8 + 584.6464*x^10



#############################

are_vectors_orthogonal <- function(vector1, vector2) {
  
  # Calculate the dot product of the subvectors
  dot_product <- sum(vector1 * vector2)
  
  # Check if the dot product is approximately zero (within a tolerance)
  tolerance <- 1e-10  # You can adjust this tolerance as needed
  is_orthogonal <- abs(dot_product) < tolerance
  
  return(is_orthogonal)
}

are_vectors_orthogonal(xxx[, 4], xxx[, 5])
sum(rep(1, 50) * x)

integrate((rep(1, 50) * x), lower = -1, upper = 1)


####################################3

legendre_polynomials <- function(x, max_degree) {
  
  # Initialize a list to store Legendre polynomials
  legendre_list <- matrix(0, nrow = 50, ncol = max_degree+1)
  
  # Calculate Legendre polynomials for degrees 0 to max_degree
  for (l in 0:max_degree) {
    if (l == 0) {
      legendre_list[, l+1] <- rep(1, 50)
    } else if (l == 1) {
      legendre_list[, l+1] <- x
    } else {
      P0 <- 1
      P1 <- x
      for (i in 2:l) {
        P <- ((2 * i - 1) * x * P1 - (i - 1) * P0) / i
        P0 <- P1
        P1 <- P
      }
      legendre_list[, l+1] <- P
    }
  }
  
  return(legendre_list)
}

xxx = legendre_polynomials(x, 10)

##### Q4.B

softMax = function(z){
  expZ = exp(z)
  sumZ = sum(expZ)
  
  return(expZ/sumZ)
}

y1 = as.matrix(dat[1:10, 3:5])
yhat1 = matrix(2, 10, 3)
y1 * yhat1


jjj = sFMat$expZ[2, ]/sFMat$sumZ
jjj[1:10]
sFMat$sM[2, 1:10]

sFMat$expZ[1, 3]/sFMat$sumZ[3]

M  = 100
x1 = seq(-4, 4,length = M)
x2 = seq(-4, 4,length = M)
xx1 = rep(x1, M)
xx2 = rep(x2, each = M)

abline(h = x2,v = x1, lty = 3)

XX = cbind(xx1, xx2)
YY = matrix(1, M^2, 3)
res_fitted = neuralNet(XX, YY, resOpt$estimate, m, nu)

plot(xx2~xx1,pch = 16, col = color.gradient(yyVar$Response))
points(xxPlotData$X2~xxPlotData$X1, col = colours[Y], pch = 16)

predY = round(t(res_fitted$A2))

xxPlotData = data.frame("X1" = xx1,
                        "X2" = xx2,
                        "Y1" = predY[, 1],
                        "Y2" = predY[, 2],
                        "Y3" = predY[, 3])

yyVar = xxPlotData %>%
  mutate(Response1 = Y1) %>%
  mutate(Response2 = recode(Y2, "1" = 2)) %>%
  mutate(Response3 = recode(Y3, "1" = 3)) %>%
  pivot_longer(col=starts_with("Response"), names_to="Response", names_prefix="Response") %>% 
  filter(value ==1 | value == 2 | value == 3) %>%
  mutate(Response = value) %>%
  select(X1, X2, Response)

ggplot(yyVar, aes(x = X1)) +
  geom_point(aes(x = X1, y = X2), color = color.gradient(yyVar$Response), size = 4) +
  geom_point(aes(x = X1, y = X2), col = "black", size = 5, shape = trueY) +
  labs(x = "X1", y = "X2") +
  ylim(-4.5, 4.5) +
  theme_bw(base_size = 16)

randomMat = Y * log(t(sFMat))


# %-Fold Cross Validation

crossVal = function(X, Y){
  ns = 1:N
  k1 = sample(ns, size = N/5, replace = FALSE)
  ns = ns[-k1]
  k2 = sample(ns, size = N/5, replace = FALSE)
  ns = ns[-k2]
  k3 = sample(ns, size = N/5, replace = FALSE)
  ns = ns[-k3]
  k4 = sample(ns, size = N/5, replace = FALSE)
  ns = ns[-k4]
  k5 = ns
  
  X1.T = as.matrix(X[k1, ])
  X2.T = as.matrix(X[k2, ])
  X3.T = as.matrix(X[k3, ])
  X4.T = as.matrix(X[k4, ])
  X5.T = as.matrix(X[k5, ])
  
  Y1.T = as.matrix(Y[k1, ])
  Y2.T = as.matrix(Y[k2, ])
  Y3.T = as.matrix(Y[k3, ])
  Y4.T = as.matrix(Y[k4, ])
  Y5.T = as.matrix(Y[k5, ])
  
  X1.V = as.matrix(X[-k1, ])
  X2.V = as.matrix(X[-k2, ])
  X3.V = as.matrix(X[-k3, ])
  X4.V = as.matrix(X[-k4, ])
  X5.V = as.matrix(X[-k5, ])
  
  Y1.V = as.matrix(Y[-k1, ])
  Y2.V = as.matrix(Y[-k2, ])
  Y3.V = as.matrix(Y[-k3, ])
  Y4.V = as.matrix(Y[-k4, ])
  Y5.V = as.matrix(Y[-k5, ])
  
  return(x)
  
}

IsoMap = function(res, d){
  
  ## Your Work Here...
  
  N    = dim(res$edges)[1]
  Dg   = matrix(Inf, N, N)
  
  for(i in 1:N){
    
    Dg[i, res$neighbours[i, ]] = res$edges[i, ]
  }
  
  diag(Dg) = 0
  asp = allShortestPaths(Dg)
  
  H = diag(N) - (1/N * matrix(1, N, N))
  
  tau = -0.5 * H %*% (asp$length^2) %*% H
  sv = svd(tau)
  
  # Return a list with the embedding in U:
  return(list(Dg = Dg, d = d, ASP = asp, SVD = sv))
}

dgMod = IsoMap(res, 2)

error = Y * 0
wh0 = which(Y == 0, arr.ind = TRUE)
error[wh0] = 1

rowSums(Y*log(t(sFMat)) + (1-Y)*log(1-t(sFMat)))
Y*log(t(sFMat)) + (1-Y)*log(1-t(sFMat))

(1-Y)*log(1-t(sFMat)

          objFunc = function(y, yhat, N){
            
            wh1 = which(y == )
            error[ind0] = log(1 - yhat[ind0])
            error[ind1] = (y[ind1]) * log(yhat[ind1])
            
            # innerSum = rowSums(error)
            innerSum = rowSums(y*log(yhat) + (1-y)*log(1-yhat))
            
            outerSum = sum(innerSum)
            
            obj = -(1/N) * outerSum
            
            print(paste0("OBJ is: ", obj))
            
            return(obj)
          }
          
sFMat = softMaxMat(t(fitMod$W2)%*%fitMod$A1+fitMod$b2%*%fitMod$ones)

########################################################################

legFunc = function(x, q){
  
  legFuncSum = 0
  
  for (k in 0:q){
    
    fact1 = factorial(q)/(factorial(k)*factorial(q-k))
    fact2 = factorial(q+k)/(factorial(k)*factorial(q))
    
    legSum = ((x-1)/2)^k * (fact1) * (fact2)
    
    legFuncSum = legFuncSum + legSum
  }
  
  return(legFuncSum)
}

legPolyMat = matrix(0, N, 11)

for (i in 0:10){
  
  legPolyMat[, i + 1] = legFunc(x, i)
}

lambda = c(0, 5)
ones = diag(dim(legPolyMat)[2])

legPred = matrix(0, nrow = N, ncol = 2)

betaMat = matrix(0, nrow = dim(legPolyMat)[2], ncol = 2)

for (b in 1:2){
  betaMat[, b] = (solve(t(legPolyMat) %*% legPolyMat + 
                          (lambda[b] * ones)) %*% t(legPolyMat) %*% y)
  
  legPred[, b] = legPolyMat %*% betaMat[, b]
}

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

#############################################################

## Old Q2 with GLMNET

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

## Obj Function

objFunc = function(y, yhat, N){
  
  error = y * 0
  ind1 = which(y == 1, arr.ind = TRUE)
  
  error[ind1] = log(yhat[ind1])
  
  if(sum(is.na(yhat[ind1])) > 0 | sum(yhat[yhat == 0]) > 0)
    
    which(yhat == 0, )
  outerSum = sum(error)
  
  obj = -(1/N) * outerSum
  
  # print(paste0("OBJ is: ", obj))
  
  return(obj)
}

kFoldSeq = seq(5, 50, by = 5)

#kFoldMat = matrix(0, nrow = 5, ncol = 10)
kFoldMat = list()
valPredMat = matrix(0, nrow = 5, ncol = 10)
#trainSet = matrix(0, nrow = 45, ncol = 10)
trainSet = list()
trainPredMat = matrix(0, nrow = 45, ncol = 10)

for (k in 1:10){
  ind = (kFoldSeq[k]-4):kFoldSeq[k]
  kFoldMat[[k]] = legPolyMat[ind,]
  valPredMat[, k] = y[ind]
  trainSet[[k]] = legPolyMat[-ind,]
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

### check orthogonality of polynomials first from first principles

N1 = 80
dataSize1 = 1000
setSize1 = 5:75

matG.star.out1 = matrix(0, nrow = length(setSize1), ncol = dataSize1)
matG.star.val1 = matrix(0, nrow = length(setSize1), ncol = dataSize1)

for(i in 1:dataSize1){
  
  xi = runif(N1, -1, 1)
  ei = rnorm(N1, 0, 1)
  yi = 0.8 * xi + ei
  
  for (j in 1:length(setSize1)){
    
    sizeS = setSize1[j]
    sizeTrain = N1 - sizeS
    
    samp = sort(sample(1:N1, sizeS, replace = FALSE))
    
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
      
      matG.star.out1[j, i] = MSE(sizeTrain, g1.Mod.i$fitted.values, yTrain)
      matG.star.val1[j, i] = G1.MSE.i
    }
    
    else {
      matG.star.out1[j, i] = MSE(sizeTrain, g2.Mod.i$fitted.values, yTrain)
      matG.star.val1[j, i] = G2.MSE.i
    }
  }
}

err.Gstar.out1 = rowMeans(matG.star.out1)
err.Gstar.val1 = rowMeans(matG.star.val1)

errorPlotData1 = data.frame("SetSize" = setSize1,
                           "ValExp" = err.Gstar.val1,
                           "OutExp" = err.Gstar.out1)

ggplot(errorPlotData1, aes(x = SetSize)) +
  geom_smooth(aes(y = ValExp), color = "black", linewidth = 2, linetype = 1, se = F) +
  geom_smooth(aes(y = OutExp), color = "red", linewidth = 2, linetype = 1, se = F) +
  xlab("Size of Validation Set") + ylab("Expected Error") +
  geom_vline(xintercept = 18, linewidth = 0.5, linetype = 2, col = "blue") +
  theme_bw(base_size = 16) + 
  scale_x_continuous(sec.axis = dup_axis(~rev(.), name = "Size of Training Set"))


nu = exp(-40)
resOpt = nlm(objPen, thetaRand, iterlim = 1000)

resIn = neuralNet(XTrain, YTrain, resOpt$estimate, m, 0)
inErr = resIn$E1

resVal = neuralNet(XVal, YVal, resOpt$estimate, m, 0)
valErr = resVal$E1

print(paste0("Validation Run ", K, " ,", i, "| nu =", round(nu, 4)))
print(paste0("In Error: ", inErr))
print(paste0("Val Error: ", valErr))

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

#pdf("Figures/errorPlot.pdf")
ggplot(errorPlotData, aes(x = SetSize)) +
  geom_smooth(aes(y = ValExp), color = "black", linewidth = 2, linetype = 1, se = F) +
  geom_smooth(aes(y = OutExp), color = "red", linewidth = 2, linetype = 1, se = F) +
  xlab("Size of Validation Set") + ylab("Expected Error") +
  geom_vline(xintercept = 18, linewidth = 0.5, linetype = 2, col = "blue") +
  theme_bw(base_size = 16) + 
  scale_x_continuous(sec.axis = dup_axis(~rev(.), name = "Size of Training Set"))
#dev.off()

dx = 1/10
phiX = 0.5
a = 1

for(i in 1:dataSize){
  
  xi = runif(N, -1, 1)
  yi = f(xi) + rnorm(N, 0, 1)
  
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
      
      g1.Pred = 0.5 + (b1 * xi)
      actY = f(xi)
      
      #matG.star.out[j, i] = sum((g1.Pred - actY)^2 * phiX * dx)
      
      # matG.star.out[j, i] = mean((g1.Pred - actY)^2)
      # matG.star.out[j, i] = mean((g1.valPred - f(valData))^2)
      
      matG.star.out[j, i] = integrate(outIntegral, lower = -1, upper = 1, TRUE, b1)$value
    }
    
    else {
      matG.star.val[j, i] = G2.MSE.i
      
      g2.Pred = -0.5 + (b2 * xi)
      actY = f(xi)
      
      #matG.star.out[j, i] = sum((g2.Pred - actY)^2 * phiX * dx)
      # matG.star.out[j, i] = mean((g2.Pred - actY)^2)
      # matG.star.out[j, i] = mean((g2.valPred - f(valData))^2)
      
      matG.star.out[j, i] = integrate(outIntegral, lower = -1, upper = 1, FALSE, b2)$value 
    }
  }
}

xxx = rnorm(N, 0, 1)
dnorm(xxx, 0, 1)


###############################################################################

a = -1
b = 1
nnn = 10000

xsam = runif(nnn, a, b)
int = (b-a) * mean(fx(xsam))

dataSize = 100
setSize = 5:25

matG.star.out = matrix(0, nrow = 21, ncol = dataSize)
matG.star.out1 = matrix(0, nrow = 21, ncol = dataSize)
matG.star.val = matrix(0, nrow = 21, ncol = dataSize)

f = function(x){
  y = 0.8 * x
  return(y)
}

fx = function(x){
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
  
  f = fx(x)
  
  int = (g - f)^2 * 0.5
  return(int)
}

dx = 1/12.5
phiX = 0.5

for(i in 1:dataSize){
  
  xi = runif(N, -1, 1)
  yi = f(xi) + rnorm(N, 0, 1)
  
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
      
      g1.Pred = 0.5 + (b1 * xi)
      actY = yi
      
      a = -1
      b = 1
      nnn = 100000
      
      xsam = runif(nnn, a, b)
      int = (b-a) * mean(outIntegral(xsam, TRUE, b1))
      matG.star.out[j, i] = int
      
      xxx = seq(a, b, length.out = nnn + 1)
      h = (b - a) / nnn
      int1 = h * (sum(outIntegral(xxx, TRUE, b1)) - 0.5 * (outIntegral(a, TRUE, b1) + outIntegral(b, TRUE, b1)))
      
      matG.star.out1[j, i] = int1
      #matG.star.out[j, i] = sum((g1.Pred - actY)^2 * phiX * dx)
      
      # matG.star.out[j, i] = integrate(outIntegral, lower = -1, upper = 1, TRUE, b1, rel.tol = .Machine$double.eps^.05)$value
      # matG.star.out[j, i] = quadl(outIntegral, xa = -1, xb = 1, g1 = TRUE, beta = b1)
    }
    
    else {
      
      matG.star.val[j, i] = G2.MSE.i
      
      g2.Pred = -0.5 + (b2 * xi)
      actY = yi
      
      #matG.star.out[j, i] = sum((g2.Pred - actY)^2 * phiX * dx)
      
      # matG.star.out[j, i] = integrate(outIntegral, lower = -1, upper = 1, FALSE, b2, rel.tol = .Machine$double.eps^.05)$value
      # matG.star.out[j, i] = quadl(outIntegral, xa = -1, xb = 1, g1 = FALSE, beta = b2)
      
      a = -1
      b = 1
      nnn = 100000
      
      xsam = runif(nnn, a, b) 
      int = (b-a) * mean(outIntegral(xsam, FALSE, b2))
      matG.star.out[j, i] = int
      
      xxx = seq(a, b, length.out = nnn + 1)
      h = (b - a) / nnn
      int1 = h * (sum(outIntegral(xxx, FALSE, b2)) - 0.5 * (outIntegral(a, FALSE, b2) + outIntegral(b, FALSE, b2)))
      
      matG.star.out1[j, i] = int1
      
    }
  }
  
  toc()
}

err.Gstar.out = rowMeans(matG.star.out)
err.Gstar.out1 = rowMeans(matG.star.out1)
err.Gstar.val = rowMeans(matG.star.val)

valPlotData = data.frame("ValErr" = valErr[1, ],
                         "TrainErr" = inErr[1, ],
                         "Nus" = nus)

#pdf("Figures/valPlot_Q4.pdf")
ggplot(valPlotData, aes(x = Nus)) +
  #geom_line(aes(y = TrainErr), col = "black", linewidth = 1) +
  #geom_point(aes(y = TrainErr), col = "black", size = 4) +
  geom_line(aes(y = ValErr), col = "red", linewidth = 1) +
  #geom_point(aes(y = ValErr), col = "red", size = 4) +
  geom_vline(xintercept = nus[which.min(valErr[j, ])], linewidth = 0.5, linetype = 2, col = "blue") +
  xlab(expression(nu)) +
  ylab("Cross Entropy Error") +
  theme_bw(base_size = 16)
#dev.off()

ggplot(valPlotData, aes(x = Nus)) +
  geom_line(aes(y = ValErr3), col = "black", linewidth = 1) +
  geom_line(aes(y = ValErr4), col = "red", linewidth = 1) +
  geom_line(aes(y = ValErr5), col = "darkgreen", linewidth = 1) +
  geom_line(aes(y = ValErr6), col = "royalblue", linewidth = 1) +
  geom_line(aes(y = ValErr7), col = "violet", linewidth = 1) +
  geom_line(aes(y = ValErr8), col = "darkorange", linewidth = 1) +
  geom_vline(xintercept = nus[which.min(valErr)], linewidth = 0.5, linetype = 2, col = "blue") +
  xlab(expression(nu)) +
  ylab("Validation Error") +
  theme_bw(base_size = 16)


##### Question 1.B

dataSize = 100
setSize = 5:25

matG.star.out = matrix(0, nrow = 21, ncol = dataSize)
matG.star.val = matrix(0, nrow = 21, ncol = dataSize)

f = function(x){
  y = 0.8 * x
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
  
  int = (g - f + 1)^2 * 0.5
  return(int)
}

for(i in 1:dataSize){
  
  xi = runif(N, -1, 1)
  yi = f(xi) + rnorm(N, 0, 1)
  
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
      
      matG.star.out[j, i] = quadl(outIntegral, xa = -1, xb = 1, g1 = TRUE, beta = b1)
    }
    
    else {
      
      matG.star.val[j, i] = G2.MSE.i
      
      matG.star.out[j, i] = quadl(outIntegral, xa = -1, xb = 1, g1 = FALSE, beta = b2)
      
    }
  }
  
  toc()
}

err.Gstar.out = rowMeans(matG.star.out)
err.Gstar.val = rowMeans(matG.star.val)

errorPlotData = data.frame("SetSize" = setSize,
                           "ValExp" = err.Gstar.val,
                           "OutExp" = err.Gstar.out)

ggplot(errorPlotData, aes(x = SetSize)) +
  geom_smooth(aes(y = ValExp), color = "black", linewidth = 2, linetype = 1, se = F) +
  # geom_line(aes(y = ValExp), color = "black", linewidth = 2, linetype = 1) +
  geom_smooth(aes(y = OutExp), color = "red", linewidth = 2, linetype = 1, se = F) +
  # geom_line(aes(y = OutExp), color = "red", linewidth = 2, linetype = 1) +
  xlab("Size of Validation Set") + ylab("Expected Error") +
  theme_bw(base_size = 16) 

