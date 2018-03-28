#######################################################################
# Correlation Matrix  #################################################
# Author: Oliver Brose ################################################
#######################################################################

# calculate mean of a variable
arithmetic.mean = function (x) {
  n     = length(x)
  sum.x = 0
  
  for (i in 1:n) {
    sum.x = sum.x + x[i]
  }
  
  mean.x = sum.x/n
  
  # Return value 
  mean.x
}

# calculate variance of a variable
variance <- function (x) {
  n = length(x)
  m = mean(x)
  
  centered.sum.x = 0
  
  for (i in 1:n) {
    centered.sum.x = centered.sum.x + (x[i] - m)^2
  }
  
  var.x = (1/(n-1)) * (centered.sum.x)
  
  # Return value
  var.x
}

# calculate covariance between two variables
covariance = function (x,y) {
  n.x = length(x)
  n.y = length(y)
  
  mean.x = mean(x)
  mean.y = mean(y)
  
  centered.sum.x.y = 0
  
  for (i in 1:n.x) {
    centered.sum.x.y = centered.sum.x.y + (x[i] - mean.x)*(y[i] - mean.y)
  }
  
  covariance.x.y = (1/(n.x-1)) * (centered.sum.x.y)
  
  # Return value
  covariance.x.y
}

# calculate correlation between two variables

correlation = function(x,y) {
  variance.x      = variance(x)
  variance.y      = variance(y)
  covariance.x.y  = covariance(x,y)
  correlation.x.y = covariance.x.y/((sqrt(variance.x)) * (sqrt(variance.y)))
  
  # Return value
  correlation.x.y
}

# calculate correlation matrix

correlation.matrix <- function(x) {
  k = ncol(X)
  n = nrow(X)
  
  corr.matrix = matrix(nrow = k, ncol = k)
  
  for (i in 1:k) {
    for (j in 1:k) {
      x.i = X[,i]
      x.j = X[,j]
      
      corr.matrix[i,j] = correlation(x.i,x.j)
    }
  }
  
  # Return value
  corr.matrix
}