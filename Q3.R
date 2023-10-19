rm(list = ls())
### Libraries (if necessary)
library(tidyverse)
library(pracma)
library(e1071)

set.seed(2023)

##### Question 3.A

#======================================================

dat    = read.csv('Digits2020.csv')

Y  =  dat[, 1]
X  =  as.matrix(dat[, -1])

image(matrix(X[1, ], 28, 28)) # Digit 7
image(matrix(X[2, ], 28, 28)) # Digit 3

# A function for finding K nearest neighbours: 

k_nn = function(X1, X2, k = 10){
  
  N1 = dim(X1)[1]
  N2 = dim(X2)[1]
  d  = dim(X1)[2]
  ones  = matrix(1,N1,1)
  inds  = matrix(0,N2,k)
  edges = inds
  for(i in 1:N2)
  {
    dists     = sqrt(rowSums((ones%*%X2[i,] -X1)^2))
    wh        = order(dists)[2:(k+1)]
    inds[i,]  = wh
    edges[i,] =dists[wh]
  }
  return(list(edges = edges, neighbours = inds, k = k))
}

# Calculate K nearest neighbours:

K   = 35
res = k_nn(X, X, K)

IsoMap = function(res, d){
  
  ## Your Work Here...
  
  N    = dim(res$edges)[1]
  Dg   = matrix(Inf, N, N)
  
  for(i in 1:N){
    
    Dg[i, res$neighbours[i, ]] = res$edges[i, ]
  }
  
  diag(Dg) = 0
  asp = allShortestPaths(Dg)
  
  Dg = asp$length
  
  # Construct the d-dimensional embedding
  
  H = diag(N) - (1/N * matrix(1, N, N))
  
  tau = -0.5 * H %*% (Dg^2) %*% H
  sv = svd(tau)
  
  y1 = sv$d[1] * sv$v[, 1]
  y2 = sv$d[2] * sv$v[, 2]
  
  # Return a list with the embedding in U:
  return(list(D1 = y1, D2 = y2, Dg = Dg, d = d))
}

# Q3.B
# Plot the embedding

isoMod = IsoMap(res, 2)

colours = c("darkblue", "darkorange")

isoData = data.frame("D1" = isoMod$D1,
                     "D2" = isoMod$D2,
                     "Response" = as.factor(Y))

pdf("embedPlot_Q3.pdf")
ggplot(isoData, aes(x = D1, y = D2, col = Response)) +
  geom_point(size = 3) +
  xlab("Dimension 1") +
  ylab("Dimension 2") +
  theme_bw(base_size = 14) +
  scale_color_manual(values = c("3" = "darkblue", "7" = "darkorange")) + 
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 15))
dev.off()
