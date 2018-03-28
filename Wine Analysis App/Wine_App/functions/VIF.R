#######################################################################
# Variance Inflation Factor Function ##################################
# Author: Alex Döbele #################################################
#######################################################################

VIF = function(lmobject) {
  input = lmobject$model[-1]
  
  out = unlist(lapply(seq_along(input), function(i) {
    r.squared = summary(lm(as.matrix(input[i]) ~ as.matrix(input[-i])))$r.squared
    1/(1 - r.squared)
  }))
  names(out) = colnames(input)
  
  out
}