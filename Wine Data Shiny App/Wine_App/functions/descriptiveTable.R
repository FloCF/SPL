#######################################################################
# Descriptive table ###################################################
# Author: Oliver Brose ################################################
#######################################################################

descriptiveTable = function (df) {
  
  out_tab = data.frame(
    Minimum = apply(df, 2, min),
    q10     = apply(df, 2, quantile, probs = 0.1),
    q25     = apply(df, 2, quantile, probs = 0.25),
    Median  = apply(df, 2, median),
    Mean    = apply(df, 2, mean),
    q75     = apply(df, 2, quantile, probs = 0.75),
    q90     = apply(df, 2, quantile, probs = 0.9),
    Maximum = apply(df, 2, max),
    IQR     = apply(df, 2, IQR),
    SD      = apply(df, 2, sd)
  )
  colnames(out_tab)[c(2,3,6,7)] = paste(c("10%", "25%", "75%", "90%"), "Qu.")
  # Return value
  out_tab
}