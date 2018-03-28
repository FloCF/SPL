#######################################################################
# Methods for checking Regression Assumptions #########################
# Author: Alex Döbele #################################################
#######################################################################

require(corrplot)

white_wine = read.csv("winequality-white.csv", sep= ";")
red_wine   = read.csv("winequality-red.csv", sep = ";")

#Here we dont need the colour as it makes the computation harder.
#white_wine['colour'] = 'white' 
#red_wine['colour']   = 'red'

wines = rbind(white_wine, red_wine)
#1.Multicollinearity:

#Compute the correlation matrix with the correalation function
cor_mat(wines)


#plot of correlation matrix:
corr_plot = corrplot(cor_ma, type = "upper", order = "hclust", 
                     tl.col = "black", tl.srt = 45)

det = det(cor_ma, method = c("eigenvalues"))

ifelse(det < 0.0001, print("determinant is smaller than 0.0001, therefore a first indicator against multicolliniarity is found."),
       print("determinant is bigger than 0.0001, therefore a first indicator for multicolliniarity is found"))

##Compute the variance inflation factor(VIF):

vif_func(wines, treshhold = 10, Trace = FALSE)

#reduced regression without variables exceeding the treshhold of vif
red_var = vif_func(wines, thresh = 5, trace = FALSE)
insert = paste("quality ~", paste(red_var , collapse = "+"))
vif_reg = lm(insert, wines)
summary(vif_reg)
