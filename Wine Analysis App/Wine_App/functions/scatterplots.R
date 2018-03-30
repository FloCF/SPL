#######################################################################
# Scatterplots with ggplot2 ###########################################
# Author: Florian Schulz ##############################################
#######################################################################

scatterplots = function(x, y, data, group = FALSE) {
  # group data if selected  
  if (group) {
        color = with(data, ifelse(quality %in% c("3", "4"), "[3,4]",
                                  ifelse(quality %in% c("5", "6", "7"), "[5,6,7]", "[8,9]")))
    } else color = as.character(data[["quality"]])
    # Subtitiles
    subtitle = paste(x, "vs.", y)
    # Plot outout
    ggplot(data, aes_string(x = x, y = y)) +
      geom_point(aes(col = color), size = 2, alpha = 0.8) +
      scale_colour_brewer(palette = "Set1") + 
      labs(title = "Scatterplot", subtitle = subtitle, color = "Quality") +
      theme_bw()
}
