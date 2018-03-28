library(shiny)
library(shinythemes)
# Define global UI
ui = fluidPage(
  list(tags$head(HTML('<link rel = "icon", href = "HU-logo.png", type = "image/png" />'))),
  div(style = "padding: 1px 0px; width: '100%'",
      titlePanel(
        title = "", windowTitle = "Wine Analysis App"
      )
  ),
  withMathJax(),
  navbarPage(
    title = div(img(src="HU-logo.png", width = 30), "Wine Analysis App"),
    theme = shinytheme("cosmo"),
                   
    tabPanel("Introduction", Intro_ui()),
    tabPanel("Exploratory Data Analysis", titlePanel("Exploratory Data Analysis"), ExplorData_ui()),
    tabPanel("Feature Analysis and Selection", titlePanel("Feature Analysis and Selection"), FeatAnaSel_ui()),
    tabPanel("Simple Regression",
      fluidRow(
        column(5,titlePanel("Simple Regression")),
        column(5,
          wellPanel(
            selectInput(inputId = "var_select_method", label = "Select Variable Selection Method",
                        choices = c("Selection by Hand", "Lasso"), selected = "Selected by Hand")
          )
        )
      ), fluidRow(hr()),
      SimpReg_ui()
    ),
    tabPanel("Challenger Models", titlePanel("Challenger Models"), Challengers_ui())
  )
)

# Define global server function
server = function(input, output, session) {
  
  Intro(input, output, session)
  
  ExplorData(input, output, session)
  
  FeatAnaSel(input, output, session)
  
  SimpReg(input, output, session)
  
  Challengers(input, output, session)
}

# Run the application 
shinyApp(ui = ui, server = server)

