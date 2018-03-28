#######################################################################
# Plot residuals and fitted values with ggplot2 #######################
# Author: Alex Döbele #################################################
#######################################################################

plot_res_fit = function(ols_object) {
  data = data.frame(fitted = ols_object$fitted.values, resids = ols_object$residuals)
  
  ggplot(data, aes(x = fitted, y = resids)) + 
    geom_point(size = 1) +
    labs(y = "Residuals", x = "Fitted values") +
    scale_x_continuous(limits = c( 4, 8)) +
    scale_y_continuous(limits = c(-4, 3)) +
    geom_hline(yintercept=0, color = "red", size = 0.5) +
    theme_bw()
}