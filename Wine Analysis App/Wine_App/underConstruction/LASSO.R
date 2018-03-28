#####################################################
# repeated cv to get variability of lambda ##########
#####################################################

# x is set of explanatory variables
# y is the dependent variable
# l is the number of repeating 10-fold-crossvalidation  
# n is nlambda

l.times.cv <- function (x,y,l,n) {
  
  lambda.list = matrix(nrow = l, ncol = 2)
  lambda.list = as.data.frame(lambda.list)
  names(lambda.list) = c("iteration", "lambda")
  
  for (i in 1:l) {
    lasso.cv = cv.glmnet(x, y, nlambda = n)
    lambda.list[i,1]=i
    lambda.list[i,2]=lasso.cv$lambda.min
    print(i)
  }
  
  return(lambda.list)
  
}

##############################################################################
#### LASSO-Regression ########################################################
##############################################################################

# load data
data=read.csv("C:/Users/Oliver Brose/Desktop/Studium/Statistical Programming Languages/Datensatz/winequality-red.csv", sep=";")

# Loading relevant packages
library("glmnet")
library("plotmo")
library("ggplot2")
library("selectiveInference")

#################################
# 1. preprocessing data
#################################

# scaling data frame
data=as.data.frame(scale(data))

# create vector for dependend variable and matrix of explanatory variables
y = data$quality
x = data
x$quality = NULL
x = as.matrix(x)

#####################################################################
# fitting a LASSO model and plotting for different nlambda ##########
#####################################################################

lasso.100 <- glmnet(x, y, nlambda = 100, family="gaussian", alpha=1, standardize = FALSE)
plot_glmnet(lasso.100, label=5, col=c(3,3,1,1,1,1,1,1,2,2,2,2))

lasso.1000 <- glmnet(x, y, nlambda = 1000, family="gaussian", alpha=1, standardize = FALSE)
plot_glmnet(lasso.1000, label=5, col=c(3,3,1,1,1,1,1,1,2,2,2,2))

lasso.10000 <- glmnet(x, y, nlambda = 10000, family="gaussian", alpha=1, standardize = FALSE)
plot_glmnet(lasso.10000, label=5, col=c(3,3,1,1,1,1,2,2,1,1,1,2))

lasso.100000 <- glmnet(x, y, nlambda = 100000, family="gaussian", alpha=1, standardize = FALSE)
plot_glmnet(lasso.100000, label=5, col=c(3,3,2,2,2,1,2,2,1,1,1,1))

#####################################################################
# perform cross validation on time ##################################
#####################################################################

# crossvalidation with nlambda = 100
lasso.cv.100 = cv.glmnet(x, y, nlambda = 100)
plot(lasso.cv.100)

# crossvalidation with nlambda = 1.000
lasso.cv.1000 = cv.glmnet(x, y, nlambda = 1000)
plot(lasso.cv.1000)

# crossvalidation with nlambda = 10.000
lasso.cv.10000 = cv.glmnet(x, y, nlambda = 10000)
plot(lasso.cv.10000)

# crossvalidation with nlambda = 100.000
lasso.cv.100000 = cv.glmnet(x, y, nlambda = 100000)
plot(lasso.cv.100000)

#####################################################################
# perform cross validation 500 times and get distribution ###########
#####################################################################

# repeat crossvalidation 500 times with nlambda = 100 and get distribution of "optimal" lambda 
cv.nlambda.100 = l.times.cv(x,y,l = 500, n = 100)
histogram(data = cv.nlambda.100, variable = "lambda", bandwith = 0.001, header = "nlamba = 100", yrange = c(0,0.5))

# repeat crossvalidation 500 times with nlambda = 1000 and get distribution of "optimal" lambda
cv.nlambda.1000 = l.times.cv(x,y,l = 500, n = 1000)
histogram(data = cv.nlambda.1000, variable = "lambda", bandwith = 0.001, header = "nlamba = 1000", yrange = c(0,0.5))

# repeat crossvalidation 500 times with nlambda = 10000 and get distribution of "optimal" lambda
cv.nlambda.10000 = l.times.cv(x,y,l = 500, n = 10000)
histogram(data = cv.nlambda.10000, variable = "lambda", bandwith = 0.0005, header = "nlamba = 10000", yrange = c(0,0.5))

# get medians of the distributions
median(cv.nlambda.100$lambda)
median(cv.nlambda.1000$lambda)
median(cv.nlambda.10000$lambda) # we choose 0.018

############################################################################################
############### LASSO coefficients with inference ##########################################
############################################################################################

# set parameters
lambda = 0.018
n = nrow(data)

# recalculate coefficients accor
beta = round(coef(lasso.10000, s=lambda/n),2)[-1]

# perform post-selective inference 
out = fixedLassoInf(x, y, beta, lambda, alpha = 0.05)
hello = as.matrix(out)

# extract values from list "out" and create data frame
variable = names(out$vars)
coefficient = round(out$coef0,2)
p_value = round(out$pv,2)
lower_bound_ci = round(out$ci[,1],2)
upper_bound_ci = round(out$ci[,2],2)

# create data frame from inference
inference = cbind.data.frame(variable, coefficient, p_value, lower_bound_ci, upper_bound_ci)

# print table
xtable(inference)

############################################################################################
############### safe workspace #############################################################
############################################################################################

save.image()
