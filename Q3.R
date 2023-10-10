rm(list = ls())
### Libraries (if necessary)
library(tidyverse)
library(pracma)
library(glmnet)

set.seed(2023)

##### Question 3.A


#======================================================
# Template code:
rm(list = ls())
dat    = read.csv('Digits2020.csv')

Y  =  dat[,1]
X  =  as.matrix(dat[,-1])

image(matrix(X[1,],28,28))

# A function for finding K nearest neighbours: 
k_nn = function(X1,X2,k = 10)
{
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
res = k_nn(X,X,K)

# Q3 a)
# A function that calculates 
IsoMap=function(res,d)
{
  ## Your Work Here...
  
  # Return a list with the embedding in U:
  return(list(U = ..., Dg = Dg, d = d))
}

# Q3 b)
# Plot the embedding