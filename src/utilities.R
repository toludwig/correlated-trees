# Functions in this file are mostly due to Charley Wu
# see: https://github.com/charleywu/gridsearch/blob/master/analysis1D/Models.R

library("MASS")
library("Matrix")

##############################################################################################################
#Diffusion Kernel and GP
##############################################################################################################

diffusionKernel<- function(network, alpha = 1, precision = 10, normalizedLaplacian = F){
  #1. Construct Graph Primatives
  D <- diag(strength(network)) #degree matrix
  #(weighted) adjacency matrix
  if (is_weighted(network)){# Weighted graph
    W <- as_adjacency_matrix(network, attr="weight", sparse=F)
  }else{
    W <- as_adjacency_matrix(network, sparse=F)
  }
  #2. construct laplacian
  L <- D - W
  if (normalizedLaplacian==T){
    L <- sqrt(solve(D))%*%L%*%sqrt(solve(D)) #if degree normalized Laplacian
  }
  #3. Eigenvalue decomposition
  eig <- eigen(-L)
  #4. Matrix Exponential of Laplacian matrix
  K <- eig$vectors %*% diag(exp(alpha * eig$values)) %*% t(eig$vectors) #exp(alpha * L)
  K <- round(K, precision) #necessary since loss of precision can make other functions not recognize K as positive semi-definite 
  return(K)
}


############################################

#Gaussian Process function
#X.test: matrix for predictions
#X; matrix of observations
#y: vector of observed outcomes
#k: covariance matrix (e.g., from diffusion kernel)
#mu_0:  prior mean
#var_0: prior variance
#noise: noise variance
gpr<- function(X.test, X, Y, k, mu_0 = 0, var_0 = 1, noise = 0.0001){
  Y <- Y - mu_0 #invariant mean scaling
  k <- var_0 * k #invariant var scaling
  #make it a matrix
  Xstar <- as.matrix(X.test)
  #dimensions
  d <- ncol(as.matrix(X))
  #Calculate K:
  K <- as.matrix(k[X,X]) +  (diag(length(Y))*noise) 
  KK.inv <- chol2inv(chol(K)) #MASS implementation of Cholesky  
  #times y
  Ky <- KK.inv %*% Y
  #apply the kernel
  result <- apply(Xstar, 1, function(x){
    XX <- matrix(x,nrow=1) 
    Kstar <- k[X, XX]
    Kstarstar <- k[XX,XX]
    #get mean vector
    mu <- t(Kstar) %*% Ky
    #get covariance
    cv <- Kstarstar - (t(Kstar) %*% KK.inv %*% Kstar) 
    if (cv<0){ cv <- abs(cv)} #DEBUG SOLUTION: MANUALLY SET CV TO BE POSITIVE IF NEGATIVE
    #return means and variance
    return(c(mu, cv))
  })
  #as a data frame with names mu and var
  prediction <- as.data.frame(t(result))
  colnames(prediction) <- c("mu", "var")
  prediction$mu <- prediction$mu + mu_0 #account for mean function
  return(prediction)
}


##############################################################################################################
# MISCELLANEOUS
##############################################################################################################

# return index of maximum, breaks ties randomly
argmaxRandomTie <- function(x) {
  y <- seq_along(x)[x == max(x)]
  if (length(y) > 1L)
    sample(y, 1L)
  else y
}

## plot colorbar
# copied from: https://stackoverflow.com/a/9314880
color.bar <- function(lut, min, max=-min, nticks=11, ticks=seq(min, max, len=nticks), title='') {
  scale = (length(lut)-1)/(max-min)
  
  dev.new(width=1.75, height=5)
  plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', xlab='', yaxt='n', ylab='', main=title)
  for (i in 1:(length(lut)-1)) {
    y = (i-1)/scale + min
    rect(0,y,10,y+1/scale, col=lut[i], border=NA)
  }
}

# TEST
# cscale <- rev(heat_hcl(1000))
# color.bar(cscale, 0,1000)
# 
# # save
# dev.copy(svg, "../fig/trees/colorbar.svg")
# dev.off()



#Normalize values to min max range
normalizeMinMax <- function(x, min, max){
  normalized = (max - min) * ((x - min(x))/(max(x) - min(x))) + min
  return(normalized)
}

#rescale values to new min max from old min max range
rescaleMinMax <- function(x, newmin, newmax, oldmin = 1, oldmax = 50){
  return((newmax-newmin)/(oldmax-oldmin)*(x-oldmax)+newmax)
}

