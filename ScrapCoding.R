
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

ones = diag(1, dim(legendreFunctions)[2])

betaMat = matrix(0, nrow = dim(legendreFunctions)[2], ncol = 2)

for (b in 1:2){
  betaMat[, b] = solve(t(legendreFunctions) %*% legendreFunctions + 
                         (lambda[b] * ones)) %*% t(legendreFunctions) %*% y
}

betas = (solve(t(legendreFunctions) %*% legendreFunctions + 
                 (lambda[2] * ones)) %*% t(legendreFunctions) %*% y)

betas = t(legendreFunctions) %*% legendreFunctions + 
  (lambda[2] * ones)
