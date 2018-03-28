#######################################################################
# Simple Shiny example for Paper ######################################
# Author: Florian Schulz ##############################################
#######################################################################

library(shiny)

ui = fluidPage(
  sliderInput(inputId = "slider", label = "Slide Me!",
              min     = 10, max = 1000, value = 500, step = 10),
  plotOutput(outputId = "hist")
)

server = function(input, output) {
  output$hist = renderPlot(
    hist(x    = rnorm(n = input$slider, mean = 0, sd = 1),
         main = "Histogram of Standard Normal Random Numbers",
         xlab = NULL))
}

shinyApp(ui, server)