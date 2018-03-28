library(shiny)

ui = fluidPage(
  sliderInput(inputId = "test", label = "Slide me!", min = 5, max = 1005, value = 5, step = 50),
  plotOutput("hist")
)

server = function(input, output, session) {
  
  output$hist = renderPlot({
    set.seed(123)
    data = rbinom(input$test, 20, 0.5)
    hist(data, probability = T)
    curve(dnorm(x, mean=mean(data), sd=sd(data)), add=TRUE)
  })
  
}

shinyApp(ui = ui, server = server)

