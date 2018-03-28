#######################################################################
# Create histogram function ###########################################
# Author: Oliver Brose ################################################
#######################################################################

histograms = function (data, var_name, bins = 30, binwidth = NULL, density = TRUE, adjust = 1) {
  
  p = ggplot(data, aes_string(x = var_name)) + 
    geom_histogram(aes(y = ..density..), bins = bins, binwidth = binwidth, colour = "white", fill = "#0072B2")  +
    xlab(var_name) + ylab("frequency") + labs(title = paste("Histogram of", var_name)) +
    theme_bw()
  
  if (density) {
    p = p + geom_density(alpha = 0.3, colour = "#56B4E9", fill = "#56B4E9", adjust = adjust)
  }
  # Return the final plot
  p
}