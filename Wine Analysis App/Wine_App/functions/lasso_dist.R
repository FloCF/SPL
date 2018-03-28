#######################################################################
# Repeated CV to get Distribution of Lambda ############################
# Author: Oliver Brose ################################################
#######################################################################

# x is set of explanatory variables
# y is the dependent variable
# runs is the number of repeating 10-fold-crossvalidation  
# nlambda is nlambda

lasso_dist = function (x, y, runs, nlambda, type.measure = "mse") {
  
  cv_lambdas = numeric()
  
  for (i in 1:runs) {
    # setting seed for reproducibility
    set.seed(i)
    # Conduct CV Lasso regression
    lasso_cv = cv.glmnet(x, y, type.measure = type.measure, nlambda = nlambda, standardize = FALSE)
    # save result
    cv_lambdas[i] = lasso_cv$lambda.min
    # Include status report
    if (i %% 50 == 0) print(paste("Finished", i, "runs..."))
  }
  # Return cv lambdas as data frame
  data.frame(lambda = cv_lambdas)
}