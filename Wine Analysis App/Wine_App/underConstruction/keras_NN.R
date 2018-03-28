setwd("/home/flocf/Documents/git/SPL/")

WhiteWine = read.csv("Wine_App/data/winequality-white.csv", sep = ";")
RedWine   = read.csv("Wine_App/data/winequality-red.csv", sep = ";")

library(keras)
# White wine
set.seed(123)
sample_index = sample(1:nrow(WhiteWine),size = round(nrow(WhiteWine)*0.2))
White_test = WhiteWine[sample_index,]
White_test_y = White_test[,12]
White_test_X = as.matrix(White_test[,-12])

White_train = WhiteWine[-sample_index, ]
White_train_y = White_train[,12]
White_train_X = as.matrix(White_train[,-12])
# 
# # maxs = apply(White_train_X, 2, max)
# # mins = apply(White_train_X, 2, min)
# # White_train_X = scale(White_train_X, center = mins, scale = maxs - mins)
# # White_test_X = scale(White_test_X, center = mins, scale = maxs - mins)
# # # 
# whitening_data = whiten_train_test(White_train_X, White_test_X)
# means = apply(whitening_data$train, 2, mean)
# sds = apply(whitening_data$train, 2, sd)
# White_train_X = scale(whitening_data$train, center = means, scale = sds)
# White_test_X = scale(whitening_data$test, center = means, scale = sds)

White_test_y = to_categorical(White_test_y-3, num_classes = 7)
White_train_y = to_categorical(White_train_y-3, num_classes = 7)

# Redwine
set.seed(123)
sample_index = sample(1:nrow(RedWine),size = round(nrow(RedWine)*0.2))
Red_test = RedWine[sample_index,]
Red_test_y = Red_test[,12]
Red_test_X = Red_test[,-12]

Red_train = RedWine[-sample_index, ]
Red_train_y = Red_train[,12]
Red_train_X = Red_train[,-12]

maxs = apply(Red_train_X, 2, max)
mins = apply(Red_train_X, 2, min)
Red_train_X = scale(Red_train_X, center = mins, scale = maxs - mins)
Red_test_X = scale(Red_test_X, center = mins, scale = maxs - mins)

Red_test_y = to_categorical(Red_test_y-3, num_classes = 7)
Red_train_y = to_categorical(Red_train_y-3, num_classes = 7)

########################################################################

model = keras_model_sequential()
model %>%
  layer_dense(units = 100, activation = 'relu', input_shape = c(11)) %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 80, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 60, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 40, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 20, activation = 'relu') %>%
  layer_dense(units = 7, activation = 'softmax')

# model %>% layer_dense(units = 100, activation = 'relu', input_shape = c(11))
# for (i in 1:4) {
#   model %>% layer_dense(units = 80, activation = 'relu') 
# }

# model %>% compile(
#   loss = 'mse',
#   optimizer = optimizer_rmsprop(),
#   metrics = c('mae')
# )

model %>% compile(loss = 'categorical_crossentropy',
                  optimizer = optimizer_rmsprop(),
                  metrics = c('accuracy'))

print(model)
history <- model %>% fit(
  White_train_X, White_train_y, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

plot(history)

model %>% evaluate(White_test_X, White_test_y)
sum(round(predict(model, Red_test_X))==Red_test_y)/nrow(Red_test_X)