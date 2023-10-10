
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



