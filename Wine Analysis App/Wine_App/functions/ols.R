#######################################################################
# OLS Schätzer with HC robust White Standard Errors ###################
# Author: Florian Schulz ##############################################
#######################################################################

ols = function(formula, data, intercept = TRUE, var_method = "Standard") {
    # Transform formula into data frame (uses stats package)
    out = list(model = model.frame(formula, data))
    
    # Define dependent variable
    y = as.matrix(out$model[, 1])
    # ... and independet variable
    if (intercept) {
        X = as.matrix(cbind(1, out$model[, -1]))
        colnames(X)[1] = "(const)"
    } else {
        X = as.matrix(out$model[, -1])
    }
    
    # Estimate beta based on Standard formula (X'X)^-1 X'y
    out$beta = as.numeric(solve(crossprod(X)) %*% crossprod(X, y))
    
    # Calculate fitted values
    out$fitted.values = as.numeric(crossprod(t(X), out$beta))
    
    # Calculate residuals (res = y - X beta)
    out$residuals = as.numeric(y - out$fitted.values)
    
    # Calculate degrees of freedom
    out$rank = ncol(X)
    out$df.residual = nrow(X) - out$rank
    
    # Calculate sum of squared residuals
    out$ssr = sum(out$residuals^2)
    
    # Calculate variance-covariance matrix Maybe TODO: implement HAC estimators (kind of difficult, since needs weights)
    if (var_method == "Standard") {
        out$Vp = 1/out$df.residual * out$ssr * solve(crossprod(X))
    } else if (var_method == "HC") {
        out$Vp = solve(crossprod(X)) %*% (crossprod(X, diag(as.numeric(out$residuals^2))) %*% X) %*% solve(crossprod(X))
    } else {
        warning("Use 'Standard' or 'HC' as var_method, else 'Standard' will be used.")
        Vp = 1/out$df.residual * as.numeric(crossprod(out$residuals)) * solve(crossprod(X))
    }
    
    # Calculate Standard errors of estimates
    out$se = sqrt(diag(out$Vp))
    
    # Calculate p-values
    out$p_value = 2 * pt(abs(out$beta/out$se), df = out$df.residual, lower.tail = FALSE)
    
    # Calculate sum of squared explained values
    sse = if (intercept) {
        sum((out$fitted.values - mean(out$fitted.values))^2)
    } else {
        sum(out$fitted.values^2)
    }
    
    # Calculate R squared
    out$r.squared = sse/(sse + out$ssr)
    
    # Calculate adjusted R squared
    out$adj.r.squared = 1 - (1 - out$r.squared) * ((nrow(X) - as.numeric(intercept))/out$df.residual)
    
    # Calculate F statistic
    ftest.stat = (sse/(out$rank - as.numeric(intercept)))/(out$ssr/out$df.residual)
    out$f.stat = c(test.stat   = ftest.stat,
                   num.par     = out$rank - as.numeric(intercept),
                   df.residual = out$df.residual,
                   p.value     = pf(ftest.stat, out$rank - as.numeric(intercept), out$df.residual, lower.tail = FALSE))
    
    # Name the Coefficients
    names(out$beta) = names(out$p_value)
    
    # Return final value
    out
}
