##############################################################################
#### LASSO-Regression ########################################################
##############################################################################

# load data
data=read.csv("/home/flocf/Documents/git/SPL/Wine_App/data/winequality-red.csv", sep=";")

# Loading relevant packages
library("glmnet")
library("plotmo")

#################################
# 1. preprocessing data
#################################

# scaling data frame
data=as.data.frame(scale(data))

# create vector for dependend variable
y = data$quality

# create matrix of explanatory variables
x = data
x$quality = NULL
x = as.matrix(x)

#####################################################################
# fitting a LASSO model and plotting for different nlambda ##########
#####################################################################

lasso1 <- glmnet(x, y, nlambda = 100, family="gaussian", alpha=1)
plot_glmnet(lasso1, label=5, col=c(3,3,1,1,1,1,1,1,2,2,2,2))

lasso2 <- glmnet(x, y, nlambda = 1000, family="gaussian", alpha=1)
plot_glmnet(lasso2, label=5, col=c(3,3,1,1,1,1,1,1,2,2,2,2))

lasso3 <- glmnet(x, y, nlambda = 10000, family="gaussian", alpha=1)
plot_glmnet(lasso3, label=5, col=c(3,3,1,1,1,1,2,2,1,1,1,2))

lasso4 <- glmnet(x, y, nlambda = 100000, family="gaussian", alpha=1)
plot_glmnet(lasso4, label=5, col=c(3,3,2,2,2,1,2,2,1,1,1,1))

lasso5 <- glmnet(x, y, nlambda = 500000, family="gaussian", alpha=1)
plot_glmnet(lasso5, label=5, col=c(3,2,1,1,1,1,1,1,1,1,1,1))

# to safe these plots in high resolution click 
# "Export" -> "Save as PDF" -> "PDF-Size = Device Size"
# and set directory as wished

# Residual Plot for Lasso from http://www.milbo.org/doc/plotres-notes.pdf#page=7
plotres(lasso)

#####################################################
# repeated cv to get variability of lambda ##########
#####################################################

l.times.cv <- function (x,y,l,n) {
  
  # x is set of explanatory variables
  # y is the dependent variable
  # l is the number of repeating 10-fold-crossvalidation  
  # n is nlambda
  
  lambda.list=matrix(nrow = l, ncol = 2)
  
  for (i in 1:l) {
    lasso.cv = cv.glmnet(x, y, nlambda = n)
    lambda.list[i,1]=i
    lambda.list[i,2]=lasso.cv$lambda.min
  }
  
  return(lambda.list)
  
}

#####################################################################
# Model selection via cross validation ##############################
#####################################################################

# model selection via cross validation
lasso.cv.100 = cv.glmnet(x, y, nlambda = 100)
plot(lasso.cv.100)

lasso.cv.1000 = cv.glmnet(x, y, nlambda = 1000)
plot(lasso.cv.1000)

lasso.cv.10000 = cv.glmnet(x, y, nlambda = 10000)
plot(lasso.cv.10000)

lasso.cv.100000 = cv.glmnet(x, y, nlambda = 100000)
plot(lasso.cv.100000)

# Looking at distributions for several nlambdas
liste_nlambda_100=l.times.cv(x,y,l = 1000,n = 100)
hist(liste_nlambda_100[,2])

liste_nlambda_1000=l.times.cv(x,y,l = 1000,n = 1000)
hist(liste_nlambda_1000[,2])

liste_nlambda_10000=l.times.cv(x,y,l = 500,n = 5000)
hist(liste_nlambda_10000[,2])

# to safe this plot click "Export" -> "Save as PDF" -> "PDF-Size = Device Size" and set directory

#################################################################
# LASSO Coefficients ############################################
#################################################################

coef(lasso.cv.100, s = 0.1)

coef(lasso.cv.1000, s = 0.1)

coef(lasso.cv.10000, s = 0.15)

coef(lasso.cv.100000, s = 0.15) 