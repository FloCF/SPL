#######################################################################
# Challenger Models UI ################################################
# Author: Florian Schulz ##############################################
#######################################################################

Challengers_ui = function() {
  
  fluidPage(title = "Try Out more Advanced Methods",
    navlistPanel(widths = c(3, 9),
      
      # Ordinal/Multinomial - UI        
      tabPanel("Multinomial/Ordinal Logit",
        h3("Multinomial Logit"),
        fluidRow(
          column(4,
            wellPanel(
              checkboxInput(inputId = "cv_multinom", label = "Use Cross-Validation?", value = TRUE)
            ),
            uiOutput("cv_spec_multinom")
          ),
          column(8,
            h4("Confusion Matrix"),
            tableOutput("conf_mat_multinom"),
            h4("Performance"),
            tableOutput("perform_train_multinom")
          )
        ),
        hr(),
        h3("Ordinal Logit"),
        fluidRow(
          column(4,
            wellPanel(
              checkboxInput(inputId = "cv_polr", label = "Use Cross-Validation?", value = TRUE)
            ),
            uiOutput("cv_spec_polr")
          ),
          column(8,
            h4("Confusion Matrix"),
            tableOutput("conf_mat_polr"),
            h4("Performance"),
            tableOutput("perform_train_polr")
          )
        )
      ),
      # Random Forest - UI
      tabPanel("Random Forest",
        h3("Random Forest"),
        fluidRow(
          column(4,
            wellPanel(
              h4("Define CV Specifications:"),
              p("To avoid ovefitting, we only allow model estimation with cross-validation."),
              numericInput(inputId = "cv_folds_rf", label = "K Fold CV", 
                                value = 5, min = 0, max = 100, step = 1),
              p(tags$b("Note:"), "The amount of K-Folds will increase the computational time, so if you select a large number, please be patient..."),
              selectInput(inputId = "metric_cv_rf", label = "CV selection metric",
                          choices = c("Accuracy", "Kappa"), selected = "Accuracy")
            )
          ),
          column(8,
            wellPanel(
              h4("Specify Hyperparameters:"),
              fluidRow(
                column(4,
                  selectInput(inputId = "mtry_rf", label = "# of variables to split",
                              choices = 2:11, multiple = TRUE, selected = c(2,3,7))
                ),
                column(4,
                  selectInput(inputId = "splitrule_rf", label = "Splitting rule",
                              choices = c("extratrees", "gini"), multiple = TRUE, selected = "extratrees")
                ),
                column(4,
                  selectInput(inputId = "min.node.size_rf", label = "Minimum # of Obs. in Node",
                              choices = 1:30, multiple = TRUE, selected = 3)
                )
              ),
              p(tags$b("Details:"), "'# of variables to split' is the number of variables to possibly split at in each node. 2, 3, and 7 have proven
                to show great results. Also 'extratrees' seem to perform better than 'gini'. Increasing the 'minimum # of observation in noce' might
                prevent overfitting."),
              actionButton(inputId = "action_rf", label = "Run Random Forest Algorithm")
            )
          )
        ),
        fluidRow(
          column(6, plotOutput("plot_rf")),
          column(6,
            h4("Confusion Matrix"),
            tableOutput("conf_mat_rf"),
            h4("Performance"),
            tableOutput("perform_train_rf")
          )
        )
      ),
      
      # One-vs-Rest SVM - UI
      tabPanel("One-vs-Rest SVM",
        h3("One-vs-Rest SVM"),
        fluidRow(
          column(3,
            wellPanel(
              h4("Preprocess the Data"),
              radioButtons(inputId  = "preproc_svm", label = "Choose Method",
                           choices = c("standard normalize"   = "norm", "normalize between 0 & 1" = "0_1",
                                       "whiten data" = "white", "none" = "none"),
                           selected = "norm")
            )
          ),
          column(9,
            wellPanel(
              h4("Specify Hyperparameters:"),
              fluidRow(
                column(6,
                  selectInput(inputId  = "kernel_svm", label = "Choose SVM Kernel:",
                              choices  = c("linear" = "linear", "polynomial" = "poly", "radial basis" = "radial"),
                              selected = "radial"),
                  sliderInput(inputId = "svm_C", label = "Define panalty parameter C:", min = 0, max = 100, value = 1, step = 1)
                ),
                column(6,
                  uiOutput("specify_svm"),
                  actionButton(inputId = "action_svm", label = "Start SVM Calculation")
                )
              )
            )
          )
        ),
        fluidRow(
          column(4,
            h4("Performance"),
            tableOutput("accuracy_svm")
          ),
          column(7, offset = 1,
            h4("Confusion Matrix"),
            tableOutput("conf_mat_svm")
          )
        )
      ),
      
      # (Deep) Neural Networks
      tabPanel("(Deep) Neural Networks",
        h3("(Deep) Neural Networks"),
        fluidRow(
          column(6,
            wellPanel(
              h4("Preprocess the Data"),
              radioButtons(inputId = "preproc_nn", label = "Choose Method",
                           choices = c("standard normalize"   = "norm", "normalize between 0 & 1" = "0_1",
                                       "whiten data" = "white", "none" = "none"),
                           selected = "norm")
            ),
            wellPanel(
              h4("General Neural Network Structure"),
              radioButtons(inputId = "type_nn", label = "What Type of model do you want to run?",
                           choices = c("Multiclass", "Regression"), selected = "Multiclass", inline = TRUE),
              numericInput(inputId = "num_layers_nn", label = "Select number of hidden layers",
                           min = 1, max = 20, step = 1, value = 4),
              actionButton(inputId = "action_nn", "Run NN Algorithm")
            ),
            plotOutput("perform_plot_nn"),
            h4("Confusion Matrix"),
            tableOutput("conf_mat_nn"),
            h4("Performance"),
            tableOutput("perform_train_nn")
          ),
          column(6, uiOutput("specify_layers_nn"))
        )
      ),
      HTML("<hr>"),
      # Final Model Comparison
      tabPanel("Final Model Comparison",
        h3("Final Model Comparison"),
        h4("Performance"),
        tableOutput("final_perform")
      )
    )
  )
}

#######################################################################
# Challenger Models Server-file #######################################
# Author: Florian Schulz ##############################################
#######################################################################

Challengers = function(input, output, session) {
  
## Multinomial Regression #################################################################
  
  output$cv_spec_multinom = renderUI({
    if (!input$cv_multinom) return()
    
    wellPanel(
      h4("Define CV Specifications:"),
      numericInput(inputId = "cv_folds_multinom", label = "K Fold CV", 
                   value = 5, min = 2, max = 100, step = 1),
      
      p(tags$b("Note:"), "The amount of K-Folds will increase the computational time, so if you select a large number, please be patient...")
    )
  })
  
  model_multinom = reactive({
    
    if (!input$cv_multinom) {
      train(as.factor(quality) ~ .,
            data     = train_data(), method = "multinom",
            tuneGrid = data.frame(decay= 0),
            trControl = trainControl(method = "none")
      )
    } else {
      train(as.factor(quality) ~ .,
            data = train_data(), method = "multinom",
            tuneGrid = data.frame(decay= 0),
            trControl = trainControl(method = "cv", number = input$cv_folds_multinom)
      )
    }
  })
  
  # Define reactive Confusion Matrix
  conf_mat_multinom = reactive({
    confusionMatrix(data = predict(model_multinom()), reference = train_data()$quality)
  })
  
  output$conf_mat_multinom = renderTable({
    if (!input$cv_multinom) {
      conf_mat = as.data.frame.matrix(conf_mat_multinom()$table)
      rownames(conf_mat) = paste0("<b>", rownames(conf_mat), "</b>")
      return(conf_mat)
    }
    
    if (is_empty(input$cv_folds_multinom)) return("wait...")
    if (is.na(input$cv_folds_multinom) | input$cv_folds_multinom==1) return("K-fold must be > 1")
    
    conf_mat = as.data.frame.matrix(conf_mat_multinom()$table)
    rownames(conf_mat) = paste0("<b>", rownames(conf_mat), "</b>")
    # Return Confusion Matrix
    conf_mat
  }, rownames = TRUE, caption = "Prediction (vertical) vs. Actual (horizontal)", sanitize.text.function = function(x) x)
  
  output$perform_train_multinom = renderTable({
    if (!input$cv_multinom) return(data.frame(Accuracy = conf_mat_multinom()$overall[1], Kappa = conf_mat_multinom()$overall[2]))
    if (is_empty(input$cv_folds_multinom)) return()
    
    model_multinom()$results[2:5]
  }, caption = "Model Accuracy & Kappa on the Whole Training (without) or Average on k-Fold Validation Sets (with CV).")
  
## Ordinal Regression #################################################################
  
  output$cv_spec_polr = renderUI({
    if (!input$cv_polr) return()
    
    wellPanel(
      h4("Define CV Specifications:"),
      numericInput(inputId = "cv_folds_polr", label = "K Fold CV", 
                   value = 5, min = 2, max = 100, step = 1),
      
      p(tags$b("Note:"), "The amount of K-Folds will increase the computational time, so if you select a large number, please be patient...")
    )
  })
  
  model_polr = reactive({
    
    if (!input$cv_polr) {
      train(as.factor(quality) ~ .,
            data = train_data(), method = "polr",
            tuneGrid = data.frame(method= "logistic"),
            trControl = trainControl(method = "none")
      )
    } else {
      train(as.factor(quality) ~ .,
            data = train_data(), method = "polr",
            tuneGrid = data.frame(method= "logistic"),
            trControl = trainControl(method = "cv", number = input$cv_folds_polr)
      )
    }
  })
  
  # Define reactive Confusion Matrix
  conf_mat_polr = reactive({
    confusionMatrix(data = predict(model_polr()), reference = train_data()$quality)
  })
  
  output$conf_mat_polr = renderTable({
    if (!input$cv_polr) {
      conf_mat = as.data.frame.matrix(conf_mat_polr()$table)
      rownames(conf_mat) = paste0("<b>", rownames(conf_mat), "</b>")
      return(conf_mat)
    }
    
    if (is_empty(input$cv_folds_polr)) return("wait...")
    if (is.na(input$cv_folds_polr) | input$cv_folds_polr==1) return("K-fold must be > 1")
    
    conf_mat = as.data.frame.matrix(conf_mat_polr()$table)
    rownames(conf_mat) = paste0("<b>", rownames(conf_mat), "</b>")
    # Return Confusion matrix
    conf_mat
  }, rownames = TRUE, caption = "Prediction (vertical) vs. Actual (horizontal)", sanitize.text.function = function(x) x)
  
  output$perform_train_polr = renderTable({
    if (!input$cv_polr) return(data.frame(Accuracy = conf_mat_polr()$overall[1], Kappa = conf_mat_polr()$overall[2]))
    if (is_empty(input$cv_folds_polr)) return()
    
    model_polr()$results[2:5]
  }, caption = "Model Accuracy & Kappa on the Whole Training (without) or Average on k-Fold Validation Sets (with CV).")
  
## Random Forest #################################################################
  
  observeEvent(input$action_rf, {
    
    model_rf <<- reactive({
      
      # Load reactive inputs
      mtry_rf          = as.numeric(isolate(input$mtry_rf))
      splitrule_rf     = isolate(input$splitrule_rf)
      min.node.size_rf = as.numeric(isolate(input$min.node.size_rf))
      
      train(as.factor(quality) ~ .,
            tuneGrid  = expand.grid(mtry = mtry_rf, splitrule = splitrule_rf, min.node.size = min.node.size_rf),
            data      = train_data(), method = "ranger",
            trControl = trainControl(method = "cv", number = isolate(input$cv_folds_rf), verboseIter = TRUE),
            metric    = isolate(input$metric_cv_rf))
    })
    
    output$plot_rf = renderPlot({
      if (length(isolate(input$mtry_rf)) <= 1) return()
      
      ggplot(model_rf()) + theme_bw()
    })
    
    output$conf_mat_rf = renderTable({
      if (is.na(input$cv_folds_rf) | input$cv_folds_rf==1) return("K-fold must be > 1")
      
      conf_mat = as.data.frame.matrix(confusionMatrix(data = predict(model_rf()), reference = train_data()$quality)$table)
      # Naming the rows with bold classes
      rownames(conf_mat) = paste0("<b>", rownames(conf_mat), "</b>")
      # Return
      conf_mat
    }, rownames = TRUE, caption = "Prediction (vertical) vs. Actual (horizontal)", sanitize.text.function = function(x) x)
    
    output$perform_train_rf = renderTable(
      model_rf()$results, rownames = FALSE,
      caption = "Average Model Accuracy & Kappa on the k-Fold Validation Sets."
    )
    
  })
  
## One-vs-Rest SVM #################################################################
  
  output$specify_svm = renderUI({
    switch(input$kernel_svm,
      "linear"  = withMathJax(helpText("Kernel Formula: $$k(x,x') = x^T x'$$")),
      "poly"    = {
        fluidPage(
          withMathJax(helpText("Kernel Formula: $$k(x,x') = (1 + x^T x')^d$$")),
          sliderInput(inputId = "svm_degree", label = "Define Degree of polynomial Kernel",
                       min = 2, max = 6, value = 3, step = 1)
        )
      },
      "radial"  = {
        fluidPage(
          withMathJax(helpText("Kernel Formula: $$k(x,x') = exp(-\\gamma ||x - x'||^2)$$")),
          sliderInput(inputId = "svm_gamma", label = "Define Gamma:",
                      min = 0.02, max = 1.2, value = 0.1, step = 0.02)
        )
      }
    )
  })
  
  data_svm = reactive({
    
    train_y = train_data()$quality
    test_y  = test_data()$quality
    train_X = train_data()[, !names(train_data()) %in% "quality"]
    test_X  = test_data()[, !names(train_data()) %in% "quality"]
    
    if (input$preproc_svm == "norm") {
      
      means = apply(train_X, 2, mean)
      sds   = apply(train_X, 2, sd)
      
      train_X = scale(train_X, center = means, scale = sds)
      test_X  = scale(test_X, center = means, scale = sds)
    } else if (input$preproc_svm == "white") {
      whiten_data = whiten_train_test(train_X, test_X)
      
      train_X = as.matrix(whiten_data$train)
      test_X  = as.matrix(whiten_data$test)
    } else if (input$preproc_svm == "0_1") {
      maxs = apply(train_X, 2, max)
      mins = apply(train_X, 2, min)
      
      train_X = scale(train_X, center = mins, scale = maxs - mins)
      test_X  = scale(test_X, center = mins, scale = maxs - mins)
    } else {
      train_X = as.matrix(train_X)
      test_X  = as.matrix(test_X)
    }
    
    list(train_X = train_X, train_y = train_y, test_X = test_X, test_y = test_y)
  })
  
  observeEvent(input$action_svm, {
    
    model_svm <<- reactive({
      
      train_X = train_data()[, !names(train_data()) %in% "quality"]
      test_X  = test_data()[, !names(test_data()) %in% "quality"]
      
      svm_models        = list()
      class_probs_train = list()
      class_probs_test  = list()
      
      for (i in sort(unique(c(train_data()$quality, test_data()$qualiy)))) {
        train_temp = data.frame(y = as.factor(ifelse(train_data()$quality == i, 1, 0)), train_X)
        test_temp  = data.frame(y = as.factor(ifelse(test_data()$quality == i, 1, 0)), test_X)
        
        svm_models[[as.character(i)]] = svm(y ~ ., data = train_temp, kernel = isolate(input$kernel_svm), probability = TRUE, coef0  = 1, 
                                            degree = if (is.null(isolate(input$svm_degree))) 3 else isolate(input$svm_degree),
                                            gamma  = if (is.null(isolate(input$svm_gamma))) 0.1 else isolate(input$svm_gamma),
                                            cost   = if (isolate(input$svm_C) == 0) 0.001 else isolate(input$svm_C))
        
        prob_pred_train_temp = predict(svm_models[[as.character(i)]], train_temp, probability = TRUE)
        prob_pred_test_temp  = predict(svm_models[[as.character(i)]], test_temp, probability = TRUE)
        
        class_probs_train[[as.character(i)]] = attr(prob_pred_train_temp, "probabilities")[, 2]
        class_probs_test[[as.character(i)]]  = attr(prob_pred_test_temp, "probabilities")[, 2]
        print(paste("Finished SVM for quality-class:", as.character(i)))
      }
      
      preds_train = apply(as.data.frame(class_probs_train), 1, FUN = which.max) + 2
      preds_test  = apply(as.data.frame(class_probs_test), 1, FUN = which.max) + 2
      
      list(models = svm_models,
           class_probs_train = as.data.frame(class_probs_train), class_probs_test = as.data.frame(class_probs_test),
           preds_train = preds_train, preds_test = preds_test)
    })
    
    conf_matrix_svm = reactive({
      # Get isolated data to avoid loop
      model_svm = isolate(model_svm())
      confusionMatrix(data = factor(model_svm$preds_train, levels = 3:9),
                      reference = factor(train_data()$quality, levels = 3:9))
    })
    
    output$accuracy_svm = renderTable(
      data.frame(Accuracy = conf_matrix_svm()$overall[1], Kappa = conf_matrix_svm()$overall[2]),
      
      rownames = FALSE,
      caption  = "Accuracy & Kappa for the Whole Training Set."
    )
    
    output$conf_mat_svm = renderTable({
      conf_mat = as.data.frame.matrix(conf_matrix_svm()$table)
      # Naming the rows with bold classes
      rownames(conf_mat) = paste0("<b>", rownames(conf_mat), "</b>")
      # Return
      conf_mat
    }, rownames = TRUE, caption = "Prediction (vertical) vs. Actual (horizontal)", sanitize.text.function = function(x) x)
  })
  
## (Deep) NN #################################################################
 
  output$specify_layers_nn = renderUI({
    wellPanel(
      lapply(1:input$num_layers_nn, function(i) {
        fluidRow(
          h4(paste0("Specify Hidden Layer #", as.character(i))),
          column(6,
            numericInput(inputId = paste0("num_nods", i), label = "Number of Nods", min = 1, max = 200, step = 1, value = 100), br(),
            checkboxInput(inputId = paste0("dropout", i), label = "Dropout")
          ),
          column(6,
            selectInput(inputId = paste0("activa", i), label = "Choose Activation Function",
                        choices = c("relu", "sigmoid", "linear", "tanh", "softmax"), selected = "relu"),
            numericInput(inputId = paste0("dropout_rate", i), label = "Dropout Rate", min = 0, max = 0.95, step = 0.05, value = 0.2)
            
          )
        )
      })
    )
  })
  
  data_nn = reactive({
  
    train_y = train_data()$quality
    test_y = test_data()$quality
    
    if (input$type_nn == "Multiclass") {
      train_y = to_categorical(train_y - 3, num_classes = 7)
      test_y  = to_categorical(test_y - 3, num_classes = 7)
    }
    
    train_X = train_data()[, !names(train_data()) %in% "quality"]
    test_X  = test_data()[, !names(train_data()) %in% "quality"]
    
    if (input$preproc_nn == "norm") {

      means = apply(train_X, 2, mean)
      sds   = apply(train_X, 2, sd)
      
      train_X = scale(train_X, center = means, scale = sds)
      test_X  = scale(test_X, center = means, scale = sds)
    } else if (input$preproc_nn == "white") {
      whiten_data = whiten_train_test(train_X, test_X)
      
      train_X = as.matrix(whiten_data$train)
      test_X  = as.matrix(whiten_data$test)
    } else if (input$preproc_nn == "0_1") {
      maxs = apply(train_X, 2, max)
      mins = apply(train_X, 2, min)
      
      train_X = scale(train_X, center = mins, scale = maxs - mins)
      test_X  = scale(test_X, center = mins, scale = maxs - mins)
    } else {
      train_X = as.matrix(train_X)
      test_X  = as.matrix(test_X)
    }
    
    list(train_X = train_X, train_y = train_y, test_X = test_X, test_y = test_y)
  })
  
  observeEvent(input$action_nn, {
    # Read in data
    data_nn = isolate(data_nn())
    
    model_nn <<- reactive({
      # Initialize Model
      model = keras_model_sequential()
      # Initialize first hidden layer
      model %>%  layer_dense(units       = isolate(input[[paste0("num_nods", 1)]]),
                             activation  = isolate(input[[paste0("activa", 1)]]),
                             input_shape = isolate(c(ncol(data_nn$train_X))))
      # If dropout is activated include dropout
      if (isolate(input[[paste0("dropout", 1)]])) {
        model %>% layer_dropout(rate = isolate(input[[paste0("dropout_rate", 1)]]))
      }
      
      # Define output layer and compiler
      if (isolate(input$type_nn) == "Multiclass") {
        output_layer = layer_dense(units = 7, activation = 'softmax')
      } else {
        output_layer = layer_dense(units = 1, activation = 'relu')
  
      }
      
      # Return model if only one hidden layer is selected
      if (isolate(input$num_layers_nn) != 1) {
        for (i in 2:isolate(input$num_layers_nn)) {
          model %>% layer_dense(units       = isolate(input[[paste0("num_nods", i)]]),
                                activation  = isolate(input[[paste0("activa", i)]]))
          if (isolate(input[[paste0("dropout", i)]])) {
            model %>% layer_dropout(rate = isolate(input[[paste0("dropout_rate", i)]]))
          }
        }
      }
      
      if (isolate(input$type_nn) == "Multiclass") {
        model %>% layer_dense(units = 7, activation = 'softmax') %>%
          compile(loss = 'categorical_crossentropy',
                  optimizer = optimizer_rmsprop(),
                  metrics = c('accuracy'))
      } else {
        model %>% layer_dense(units = 1, activation = 'relu') %>% 
          compile(loss = 'mse',
                  optimizer = optimizer_rmsprop(),
                  metrics = c('mae'))
      }
      # Return Final model
      model
    })

    fitted_model_nn = reactive({
      # Fit model
      fitted_model_nn = model_nn() %>% fit(data_nn$train_X, data_nn$train_y, epochs = 30, batch_size = 128, validation_split = 0.2)
      # Return fitted model
      fitted_model_nn
    })
    
    output$perform_plot_nn = renderPlot(
      plot(fitted_model_nn()) + theme_bw()
    )
    
    conf_mat_nn = reactive({
      if (isolate(input$type_nn) == "Multiclass") {
        prediction = model_nn() %>% predict_classes(data_nn$train_X) + 3
      } else {
        prediction = as.numeric(round(model_nn() %>% predict(data_nn$train_X)))
      }
      
      confusionMatrix(data = prediction, reference = train_data()$quality)
    })
    
    output$conf_mat_nn = renderTable({
      conf_mat = as.data.frame.matrix(conf_mat_nn()$table)
      rownames(conf_mat) = paste0("<b>", rownames(conf_mat), "</b>")
      # Return Confusion matrix
      conf_mat
    }, rownames = TRUE, caption = "Prediction (vertical) vs. Actual (horizontal)", sanitize.text.function = function(x) x)
    
    output$perform_train_nn = renderTable(
      data.frame(Accuracy = conf_mat_nn()$overall[1], Kappa = conf_mat_nn()$overall[2]),
      
      rownames = FALSE,
      caption  = "Accuracy & Kappa for the Whole Training Set."
    )
  })

## Performance Overview #################################################################
  
  final_perform = reactive({
    
    if (input$action_rf < 1 | input$action_svm < 1 | input$action_nn < 1) return()
    
    ols_train_data_hand = train_data()[, selected_vars()$hand]
    ols_hand            = ols(quality ~., ols_train_data_hand)
    ols_test_data_hand  = test_data()[, selected_vars()$hand]
    ols_test_data_hand  = as.matrix(cbind(1, ols_test_data_hand[, !names(ols_test_data_hand) %in% "quality"]))
    fitted_hand         = round(as.numeric(crossprod(t(ols_test_data_hand), ols_hand$beta)))
    conf_mat_ols_hand   = confusionMatrix(data      = factor(fitted_hand, levels = 3:9),
                                          reference = factor(test_data()$quality, levels = 3:9))
    
    ols_train_data_lasso = train_data()[, selected_vars()$lasso]
    ols_lasso            = ols(quality ~., ols_train_data_lasso)
    ols_test_data_lasso  = test_data()[, selected_vars()$lasso]
    ols_test_data_lasso  = as.matrix(cbind(1, ols_test_data_lasso[, !names(ols_test_data_lasso) %in% "quality"]))
    fitted_lasso         = round(as.numeric(crossprod(t(ols_test_data_lasso), ols_lasso$beta)))
    conf_mat_ols_lasso   = confusionMatrix(data      = factor(fitted_lasso, levels = 3:9),
                                           reference = factor(test_data()$quality, levels = 3:9))
    
    conf_mat_multinom = confusionMatrix(data      = factor(predict(model_multinom(), test_data()), levels = 3:9),
                                        reference = factor(test_data()$quality, levels = 3:9))
    
    conf_mat_polr     = confusionMatrix(data      = factor(predict(model_polr(), test_data()), levels = 3:9),
                                        reference = factor(test_data()$quality, levels = 3:9))
    
    conf_mat_rf       = confusionMatrix(data      = factor(predict(model_rf(), test_data()), levels = 3:9),
                                        reference = factor(test_data()$quality, levels = 3:9))
    
    conf_mat_svm      = confusionMatrix(data      = factor(model_svm()$preds_test, levels = 3:9),
                                        reference = factor(test_data()$quality, levels = 3:9))
    
    if (isolate(input$type_nn) == "Multiclass") {
      prediction_test_nn = model_nn() %>% predict_classes(isolate(data_nn())$test_X) + 3
    } else {
      prediction_test_nn = round(model_nn() %>% predict(isolate(data_nn())$test_X))
    }
    
    conf_mat_nn      = confusionMatrix(data      = factor(prediction_test_nn, levels = 3:9),
                                       reference = factor(test_data()$quality, levels = 3:9))
    
    list(ols_hand = conf_mat_ols_hand, ols_lasso = conf_mat_ols_lasso, multinom = conf_mat_multinom,
         polr = conf_mat_polr,rf = conf_mat_rf, svm = conf_mat_svm, nn = conf_mat_nn)
  })
  
  output$final_perform = renderTable({
    df = as.data.frame(rbind(final_perform()$ols_hand$overall, final_perform()$ols_lasso$overall, final_perform()$multinom$overall,
                            final_perform()$polr$overall, final_perform()$rf$overall,
                            final_perform()$svm$overall, final_perform()$nn$overall))
    rownames(df) = c("<b>OLS</b>", "<b>OLS (Lasso)</b>", "<b>Multinomial</b>", "<b>Ordered Logit</b>", "<b>Random Forest</b>",
                     "<b>One-vs-Rest SVM</b>", "<b>Neural Network</b>")
    
    df[,1:2]
  }, rownames = TRUE, sanitize.text.function = function(x) x)
}