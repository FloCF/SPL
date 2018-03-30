#######################################################################
# Correlation_matrix function #########################################
# Author: Alex Döbele #################################################
#######################################################################

cor_mat = function(data) {
    nv = ncol(data)  #number of variables
    ns = nrow(data)  #number of subjects
    
    # Calculate the mean matrix
    mean_matrix = matrix(data = 1, nrow = ns) %*% colMeans(data)
    
    # Create the difference matrix
    data_mat = data.matrix(data, rownames.force = NA)
    diff     = data_mat - mean_matrix
    
    # Create covariance matrix
    cov_ma = (1/nv) * t(diff) %*% diff
    
    # Compute standard deviations
    std = diag(diag(cov_ma)^(-1/2))
    
    # Correlation Matrix
    cor_ma = std %*% cov_ma %*% std
    
    # Naming the columns and rows of the correlation matrix equally
    colnames(cor_ma) = rownames(cor_ma) = colnames(data)
    
    # Round solution to be computationally more robust
    round(cor_ma, 8)
}
