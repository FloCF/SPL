#######################################################################
# Exploratory Data Analysis UI-file ###################################
# Author: Florian Schulz ##############################################
#######################################################################

ExplorData_ui = function() {
  fluidPage(
    navlistPanel(widths = c(3, 9),
      tabPanel("Descriptive Statistics",
        tableOutput("desc_stat")
      ),
      
      tabPanel("Correlation",
        plotOutput("corr", height = "500px")
      ),
      
      tabPanel("Histograms",
        fluidRow(
          column(5,
            wellPanel(
              selectInput("selected_var_hist", label = "Choose Variable",
                          choices = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide",
                                      "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol", "quality"), selected = "alcohol")
            )
          ),
          column(5, uiOutput("scale_hist"))
        ),
        plotOutput("hists")
      ),
      
      tabPanel("Boxplots",
        fluidRow(
          column(5,
            wellPanel(
              selectInput("selected_var_box", label = "Choose Variable",
                          choices = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide",
                                      "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol"), selected = "alcohol")
            )
          )
        ),
        plotOutput("boxplot")
      ),
      
      tabPanel("Scatterplots",
        fluidRow(
          column(5,
            wellPanel(
              selectInput("selected_xvar_scat", label = "Choose x - Variable",
                          choices = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide",
                                                  "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol"), selected = "alcohol")
            )
          ),
          column(5,
            wellPanel(
              selectInput("selected_yvar_scat", label = "Choose y - Variable",
                          choices = c("fixed.acidity", "volatile.acidity", "citric.acid", "residual.sugar", "chlorides", "free.sulfur.dioxide",
                                      "total.sulfur.dioxide", "density", "pH", "sulphates", "alcohol"), selected = "residual.sugar")
            )
          ),
          column(2,
            wellPanel(
              checkboxInput(inputId = "group", label = "Group data")
            )
          )
        ),
        plotOutput("scatter")
      )
    )
  )
}

#######################################################################
# Exploratory Data Analysis Server-file ###############################
# Author: Florian Schulz ##############################################
#######################################################################

ExplorData = function(input, output, session) {
  
  output$desc_stat = renderTable(descriptiveTable(train_data()), rownames = TRUE, digits = 3)
  
  output$corr = renderPlot( corrplot::corrplot(cor(train_data()), method = "ellipse", title = "Correlation of all Variables",
                                               mar = c(0,0,2,0)) )
  
  output$scale_hist = renderUI({
    
    if (input$selected_var_hist != "quality") {
      fluidRow(
        wellPanel(
          sliderInput(inputId = "hist_bins", "Scale # of Bins of Histogram", min = 5, max = 60,
                      value = 30, step = 5)
        ),
        wellPanel(
          sliderInput(inputId = "hist_adjust", "Adjust Kernel bandwidth", min = 0.4, max = 2,
                      value = 1, step = 0.1)
        )
      )
    }
  })
  
  output$hists = renderPlot({
    
    if (input$selected_var_hist != "quality") {
      histograms(train_data(), var_name = input$selected_var_hist, bins = as.numeric(input$hist_bins),
                 adjust = as.numeric(input$hist_adjust))
    } else {
      histograms(train_data(), var_name = input$selected_var_hist, bins = NULL, binwidth = 1 , density = FALSE)
    }
  })
  
  output$boxplot = renderPlot( boxplots(input$selected_var_box, train_data()) )
  
  output$scatter = renderPlot(
    scatterplots(input$selected_xvar_scat, input$selected_yvar_scat, train_data(), group = input$group)
  )
}