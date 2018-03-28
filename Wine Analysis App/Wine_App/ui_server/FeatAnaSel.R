#######################################################################
# Feature Analysis and Selection UI-file ##############################
# Author: Florian Schulz ##############################################
#######################################################################

FeatAnaSel_ui = function() {
  fluidPage(
    navlistPanel(widths = c(3, 9),
      tabPanel("Selection by Hand",
        column(6, tableOutput("multicol")),
        column(6,
          wellPanel(
            selectInput(inputId  = "select_hand", label = "Which Variable(s) you want to leave out of the Regression?",
                        choices  = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide",
                                     "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol"),
                        multiple = TRUE)
          )
        )
      ),
                 
      tabPanel("Lasso-Selection",
        fluidRow(
          column(4,
            wellPanel(
              h4(tags$b("Best Fit Based on")),
              selectInput("fit_lasso_select", label = "Select Measure", choices = c("mse", "mae"), selected = "mse"),
              hr(),
              h4(tags$b("Define (Bootstrapping) Parameters")),
              selectInput(inputId = "runs_lasso_dist", label = "# of Runs for Bootstrapped Distribution",
                          choices = c(100, 200, 500, 1000), selected = 500),
              selectInput(inputId = "nlambda_lasso", label = "Select fineness of lambda during Optimization",
                          choices = c(100, 200, 500, 1000), selected = 200),
              actionButton(inputId = "run_lasso_cv", label = "Run Multiple CV Lasso for Distribution"),
              br(),br(),
              p(tags$b("Note:"), "Based on your bootstrapping parameters, calculation can take quite a while. 
                For example, with maximum specification, based on your machine, this can take 5- 10 minutes. 
                Check your console for status."),
              hr(),
              h4(tags$b("Distribution Statistics")),
              tableOutput("stats_lasso_dist")
            ),
            uiOutput("select_lambda_lasso")
          ),
          column(8,
            uiOutput("tune_hist_lasso"),
            plotOutput("dist_lasso"),
            plotOutput("lasso_algo"),
            tableOutput("coefs_lasso")
          )
        )
      )
    )
  )
}

#######################################################################
# Feature Analysis and Selection Server-file ##########################
# Author: Florian Schulz ##############################################
#######################################################################

FeatAnaSel = function(input, output, session) {
  
  selected_vars <<- reactive(
    list(hand = setdiff(colnames(train_data()), input$select_hand))
  )
  
  output$multicol = renderTable({
    # Variables that are droped based on users selection
    drops = input$select_hand
    # Drop variable
    data = train_data()[ , selected_vars()$hand]
    # Run own ols regression
    OLS = ols(quality ~ ., data = data)
    # Return own VIF function
    data.frame(VIF = VIF(OLS))
  }, rownames = TRUE)
  
  # Lasso selection #########################################################################################
  
  observeEvent(input$run_lasso_cv,{
    
    output$select_lambda_lasso = renderUI({
      fluidRow(
        hr(),
        radioButtons(inputId  = "selected_lambda", label = "Select Lambda based on",
                     choices  = c("Median" = "med", "Mean" = "mean" , "Maximum Kernel Density (mkd)" = "max"),
                     selected = "max"),
        h4(tags$b("Selected Variables:")),
        verbatimTextOutput("selected_lasso_text")
      )
    })
    
    output$tune_hist_lasso = renderUI({
      
      fluidRow(
        column(6,
          wellPanel(
            sliderInput(inputId = "hist_bins_lasso", "Scale # of Bins of Histogram", min = 4, max = 60,
                        value   = 20, step = 2)
          )
        ),
        column(6,
          wellPanel(
            sliderInput(inputId = "hist_adjust_lasso", "Adjust Kernel bandwidth", min = 0.1, max = 2,
                        value   = 1, step = 0.1)
          )
        )
      )
    })
    
    lasso_dist = lasso_dist(x            = scale(train_data()[, !names(train_data()) %in% "quality"]),
                            y            = scale(train_data()$quality),
                            runs         = as.numeric(input$runs_lasso_dist),
                            nlambda      = as.numeric(input$nlambda_lasso),
                            type.measure = input$fit_lasso_select)
    
    
    output$dist_lasso = renderPlot(
      histograms(data = lasso_dist, var_name = "lambda",
                 bins = input$hist_bins_lasso, adjust = input$hist_adjust_lasso)
    )
    
    output$stats_lasso_dist = renderTable({
      densi_lasso = density(lasso_dist$lambda, n = 1024)
      
      data.frame(Median   = median(lasso_dist$lambda),
                 Mean     = mean(lasso_dist$lambda),
                 Mkd      = densi_lasso$x[which.max(densi_lasso$y)])
      }, digits = 4)
    
    fit_lasso = reactive(
      glmnet(x       = scale(train_data()[,!names(train_data()) %in% "quality"]),
             y       = scale(train_data()$quality), 
             nlambda = as.numeric(isolate(input$nlambda_lasso)),
             family  = "gaussian", alpha = 1, standardize = FALSE)
    )
    
    choice_lambda = reactive({
      if (is_empty(input$selected_lambda)) return()
      
      densi_lasso = density(lasso_dist$lambda, n = 1024)
      switch (input$selected_lambda,
              "med"  = median(lasso_dist$lambda),
              "mean" = mean(lasso_dist$lambda),
              "max"  = densi_lasso$x[which.max(densi_lasso$y)])
    })
    
    output$lasso_algo = renderPlot({
      if (is_empty(choice_lambda())) return()
      
      plot_glmnet(fit_lasso())
      abline(v = c(log(choice_lambda()), log(mean(lasso_dist$lambda))) , col = c("red", "blue"), lty = c(2,3))
    })
    
    output$selected_lasso_text = renderPrint({
      if (is_empty(choice_lambda())) return()
      
      selected_vars <<- reactive(
        list(hand  = setdiff(colnames(train_data()), input$select_hand),
             lasso = c(colnames(train_data()[,-12])[as.logical(!(coef(fit_lasso(), s = choice_lambda())==0))[-1]],
                       "quality"))
      )
      
      cat(paste(selected_vars()$lasso[-length(selected_vars()$lasso)],
                collapse = ", \n"))
    })
    
    output$coefs_lasso = renderTable({
      if (is_empty(choice_lambda())) return()
      
      beta = coef(fit_lasso(), s = choice_lambda())[-1]
      out  = fixedLassoInf(x      = scale(train_data()[,!names(train_data()) %in% "quality"]),
                           y      = scale(train_data()$quality),
                           beta   = beta,
                           lambda = choice_lambda(),
                           alpha  = 0.05)
      
      # extract values from list "out" and create data frame
      variable = names(out$vars)
      coefficient = round(out$coef0, 2)
      p_value = round(out$pv, 2)
      lower_bound_ci = round(out$ci[,1], 2)
      upper_bound_ci = round(out$ci[,2], 2)
      
      # create data frame from inference
      cbind.data.frame(variable, coefficient, p_value, lower_bound_ci, upper_bound_ci)
    })
  })
}