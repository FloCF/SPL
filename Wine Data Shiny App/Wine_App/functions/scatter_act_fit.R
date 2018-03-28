#######################################################################
# Scatterplots actual vs fitted with ggplot2 ##########################
# Author: Florian Schulz ##############################################
#######################################################################

plot_fit_actual = function(x, fitted_qual, data, round = FALSE) {
  if (round) {
    subtitle = paste("Actual vs. Rounded Fitted with Respect to", x)
    fitted_qual = round(fitted_qual)
  } else {
    subtitle = paste("Actual vs. Fitted with Respect to", x)
  }
  subtitle = paste("Actual vs. Fitted with Respect to", x)
  ggplot(data, aes_string(x = x)) +
    geom_point(aes(y = quality, col = "actual"), alpha = 0.5) +
    geom_point(aes(y = fitted_qual, col = "fitted"), alpha = 0.5) +
    scale_colour_manual("", breaks = c("actual", "fitted"), values=c("red", "blue")) +
    labs(title = "Scatterplot", subtitle = subtitle, color = "Quality") +
    theme_bw()
}