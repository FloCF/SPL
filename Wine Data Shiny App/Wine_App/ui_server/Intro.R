#######################################################################
# Introduction UI-file ################################################
# Author: Florian Schulz ##############################################
#######################################################################

Intro_ui = function() {
  fluidPage(
    br(), br(), br(),
    h1("Welcome to the Interactive Wine Analysis App", align = "center"),
    p("This App willl be created as part of the curse",
      em("Statistical Programming Language"),
      "at HU Berlin.", align = "center"), 
    p("All codes and material is available at our",
      a("GitHub page.", href = "https://github.com/FloCF/SPL"),
      align = "center"
    ),
    hr(),
    column(2),
    column(8,
      wellPanel(h4("Select Data:", align = "center"),
        fluidPage(
          column(5,
            wellPanel(
              radioButtons(inputId = "wine_type", label = "Select with Type of Wine you want to Analyse:", choices = c("White", "Red"))
            ),
            textOutput("obs_data")
          ),
                       
          column(7,
            wellPanel(
              selectInput(inputId = "test_prop", label = "How do you want to devide the data into training/test set?",
                          choices = c("50% Training / 50% Test" = 0.5,
                                      "60% Training / 40% Test" = 0.6,
                                      "70% Training / 30% Test" = 0.7,
                                      "80% Training / 20% Test" = 0.8,
                                      "90% Training / 10% Test" = 0.9),
                          selected = 0.8)
            ),
            tableOutput("obs_train_test")
          )
        )
      )
    )
  )
}

#######################################################################
# Introduction Server-file ############################################
# Author: Florian Schulz ##############################################
#######################################################################

Intro = function(input, output, session) {
  
  wine_data = reactive({
    data = switch(input$wine_type,
                  "White" = read.csv("data/winequality-white.csv", sep = ";"),
                  "Red"   = read.csv("data/winequality-red.csv", sep = ";"))
    
    # Randomly reorder data with set.seed to ensure reproducibility 
    set.seed(127)
    # Shuffle row incicies
    rows = sample(nrow(data))
    # Return reordered data
    data[rows, ]
  })
  
  train_data <<- reactive({
    # Determine row to split on: split
    split = round(nrow(wine_data()) * as.numeric(input$test_prop))
    # Return training set
    wine_data()[1:split, ]
  })
  
  test_data <<- reactive({
    # Determine row to split on: split
    split = round(nrow(wine_data()) * as.numeric(input$test_prop))
    # Return training set
    wine_data()[(split + 1):nrow(wine_data()), ]
  })
  
  output$obs_data = renderText( paste0("Number of Observation: ", nrow(wine_data())) )
  
  output$obs_train_test = renderTable(data.frame(set = c("Training", "Test"), Observations = c(nrow(train_data()), nrow(test_data()))) )
  
}