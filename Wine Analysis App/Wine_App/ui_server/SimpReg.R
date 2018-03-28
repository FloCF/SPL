#######################################################################
# Simple Regression UI-file ###########################################
# Author: Florian Schulz ##############################################
#######################################################################

SimpReg_ui = function() {
  fluidPage(
    navlistPanel(widths = c(3, 9),
      
      tabPanel("Estimation",
        fluidRow(
          column(3,
            wellPanel(
              radioButtons(inputId = "var_method", label = "Choose Covariance Estimation",
                           choices = c("Standard", "HC"), selected = "Standard")
            )
          ),
          column(9,
            h3("Estimation Output"),
            tableOutput("coef_out")
          )
        ),
        hr(),
        fluidRow(
          column(3,
            wellPanel(
              h5(tags$b("Round fitted?")),
              checkboxInput(inputId = "round_fit", label = "")
            )
          ),
          column(9, tableOutput("goodnes_fit"))
        ),
        hr(),
        uiOutput("scat_fit_act_ui"),
        br(), br(), br()
      ),
      
      tabPanel("Checking Assumptions",
        fluidRow(
          column(6,
            h4(tags$b("Residual vs. Fitted")),
            plotOutput("fit_res")
          ),
          column(6,
            h4(tags$b("Testing for Heteroscedasticity")),
            tableOutput("hetero_test")
          )
        ), hr(),
        fluidRow(
          h4(tags$b("Checking for Normality")),
          column(6, plotOutput("hist_resid")),
          column(6, plotOutput("qq_plot"))
        )
      )
    )
  )
}

#######################################################################
# Simple Regression Server-file #######################################
# Author: Florian Schulz ##############################################
#######################################################################

SimpReg = function(input, output, session) {
  
  reg_out = reactive({
    if (input$var_select_method == "Lasso") {
      data = train_data()[, selected_vars()$lasso]
    } else{
      data = train_data()[, selected_vars()$hand]
    }
    ols(quality ~., data, var_method = input$var_method)
  })
  
  test_model = reactive({
    if (input$var_select_method == "Lasso") {
      data = test_data()[, selected_vars()$lasso]
    } else{
      data = test_data()[, selected_vars()$hand]
    }
    X = as.matrix(cbind(1, data[,-ncol(data)]))
    colnames(X)[1] = "(const)"
    
    fitted = as.numeric(crossprod(t(X), reg_out()$beta))
    
    if (input$round_fit) {
      fitted = round(fitted)
    }
    
    resids = as.numeric(data$quality - fitted)
    ssr = sum(resids^2)
    sse = sum((fitted - mean(fitted))^2)
    df.residual = nrow(X) - ncol(X)
    rmse   = sqrt((ssr/nrow(X)))
    r.squared = sse/(sse + ssr)
    adj.r.squared = 1 - (1 - r.squared) * ((nrow(X) - 1)/df.residual)
    cor.class = sum(fitted ==  data$quality)/length(data$quality)
    # Return
    list(X = X, beta = reg_out()$beta, fitted.values = fitted, residuals = resids, rmse = rmse, r.squared = r.squared,
         adj.r.squared = adj.r.squared, cor.class = cor.class)
  })
  
  output$coef_out = renderTable({
    df = data.frame(Coef = round(reg_out()$beta, 4), SE = round(reg_out()$se, 4),
                    tval = round(reg_out()$beta/reg_out()$se, 4), pVal = round(reg_out()$p_value, 4))
    colnames(df) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
    df
  }, rownames = TRUE)
  
  output$goodnes_fit = renderTable({
    
    df = data.frame(RMSE = test_model()$rmse, Rsquared = test_model()$r.squared,
                    adjRsquared = test_model()$adj.r.squared, correct = test_model()$cor.class)
    colnames(df) = c("RMSE", "R-Squared", "Adj. R-Squared", "% Correctly Classified")
    df
  })
  
  output$scat_fit_act_ui =  renderUI({
    choices = if (input$var_select_method == "Lasso") {
      selected_vars()$lasso[-length(selected_vars()$lasso)]
    } else {
      selected_vars()$hand[-length(selected_vars()$hand)]
    }
    
    fluidPage(
      column(4,
        selectInput(inputId = "scat_fit_act_x", label = "Select Variable for Actual vs Fitted Graph",
                    choices = choices)
      ),
      column(8, plotOutput("scat_fit_act"))
    )
  })
  
  output$scat_fit_act = renderPlot({
    plot_fit_actual(input$scat_fit_act_x, test_model()$fitted.values, test_data(), round = input$round_fit)
  })
  
  output$fit_res = renderPlot( plot_res_fit(reg_out()) )
  
  output$hetero_test = renderTable({
    hetero_test = testing_heterosced_ols(reg_out())
    df = rbind(hetero_test$bptest, hetero_test$whitetest)
    rownames(df) = c("Breusch-Pagan", "White")
    df
  }, rownames = TRUE)
  
  output$hist_resid = renderPlot({
    residuals = reg_out()$residuals
    hist(residuals, probability = TRUE)
    curve(dnorm(x, mean=mean(residuals), sd=sd(residuals)), add=TRUE)
  })
  
  output$qq_plot = renderPlot({
    residuals = reg_out()$residuals
    qqnorm(residuals)
    qqline(residuals)
  })
  
}