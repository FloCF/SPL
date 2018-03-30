#######################################################################
# Ckecking for Heteroscedasticity #####################################
#   # By Breusch-Pagan test ###########################################
#   # By White test (own function with PB) ############################
# Author: Florian Schulz and Alex Döbele ##############################
#######################################################################

testing_heterosced_ols = function(lmobject) {
    
    # Get explanetory variables from orginal regression
    input.vars = as.matrix(lmobject$model[, -1])
    # Get squared redisuals as dependent variable. Orginal explanetory variable remain in auxilarly regression
    aux_data = data.frame(resid.sqr = lmobject$residuals^2, input.vars)
    
    # Conduct Breusch-Pagan-test Extract R squared and multiply by number of observation for BP test statistic
    bp_test.stat = summary(lm(resid.sqr ~ ., aux_data))$r.squared * nrow(input.vars)
    # Degree of Freedom
    bp_df = ncol(input.vars)
    # Under H0 BP test statistic follows Chi-squared, hence get p value
    bp_test.pvalue = pchisq(bp_test.stat, bp_df, lower.tail = FALSE)
    # Save output in data frame
    bptest = data.frame(test.stat = bp_test.stat, df = bp_df, p.value = bp_test.pvalue)
    
    # Conduct White test White test is basically the same as the Breusch-Pagan, with additionaly including cross-terms and sqaured of
    # the explanetory variables.
    
    # If only one input no cross-term possible
    if (bp_df == 1) {
        
        # Calculate white test stat directly (analog to BP test) by first run a auxilary regression
        white.aux_reg = lm(resid.sqr ~ input.vars + I(input.vars^2), aux_data)
        # Calculate test test statistic
        white.stat = summary(white.aux_reg)$r.squared * nrow(input.vars)
        # Get degree of freedom
        white_df = nrow(input.vars) - white.aux_reg$df.residual
    } else {
        
        # Generate cross-terms formula as character
        cross.terms_char = paste0("(", paste(colnames(input.vars), collapse = " + "), ")^2")
        # Combine with rest (resid.sqr, inputs and input squared)
        white.form_char = paste("resid.sqr ~ I(input.vars^2) +", cross.terms_char)
        # Run auxilary regression
        white.aux_reg = lm(as.formula(white.form_char), aux_data)
        # Calculate test statistic
        white.stat = summary(white.aux_reg)$r.squared * nrow(input.vars)
        # Get degree of freedom directly from function
        white_df = nrow(input.vars) - white.aux_reg$df.residual
    }
    
    white.pvalue = pchisq(white.stat, white_df, lower.tail = FALSE)
    # Save output in data frame
    whitetest = data.frame(test.stat = white.stat, df = white_df, p.value = white.pvalue)
    
    # Return solutions
    list(bptest = bptest, whitetest = whitetest)
}
