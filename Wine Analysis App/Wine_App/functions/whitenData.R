#######################################################################
# Whitening the data based on training set only  ######################
# Author: Florian Schulz ##############################################
#######################################################################

whiten_train_test = function(df_train, df_test) {
  # Transform data to matrix
  mat_train = as.matrix(df_train)
  mat_test  = as.matrix(df_test)
  # Demean train and test using train
  train_demean = sweep(mat_train, 2, apply(mat_train, 2, mean))
  test_demean  = sweep(mat_test, 2, apply(mat_train, 2, mean))
  # Conduct Eigendecomposition
  eigen_train = eigen(cov(train_demean))
  # Whiten the data
  white_train = train_demean%*%eigen_train$vectors%*%diag(eigen_train$values^(-0.5))%*%t(eigen_train$vectors)
  white_test  = test_demean%*%eigen_train$vectors%*%diag(eigen_train$values^(-0.5))%*%t(eigen_train$vectors)
  # transform back to data frame
  white_df_train = as.data.frame(white_train)
  white_df_test  = as.data.frame(white_test)
  
  # Rename dfs to have original names
  colnames(white_df_train) = colnames(white_df_test) = colnames(df_train)
  
  list(train = white_df_train, test = white_df_test)
}