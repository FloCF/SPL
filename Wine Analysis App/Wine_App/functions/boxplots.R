#######################################################################
# Boxplots with ggplot2 ###############################################
# Author: Oliver Brose ################################################
#######################################################################

boxplots = function(y, data) {
  subtitle = paste("quality vs.", y)
  
  ggplot(data, aes_string(x = "as.character(quality)", y = y)) + 
    geom_boxplot(fill = "#4271AE", colour = "#1F3552", outlier.colour = "red", alpha = 0.8, outlier.shape = 1) +
    xlab("quality") + labs(title = "Boxplot", subtitle = subtitle) +
    theme_bw()
}