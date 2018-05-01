
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(withMathJax("Type One and Type Two Error \\(\\alpha\\) and \\(\\beta\\)")),
  
  sidebarLayout(
    sidebarPanel(
       selectInput("nulldist", "Distribution of the Null", c("Normal", "Uniform")),
       selectInput("altdist", "Distribution of the Alternative", c("Normal", "Uniform")),
       sliderInput("decision", "What is the decision boundary", min = 5, max = 20, value = 12)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # htmlOutput(withMathJax("\\(\mathbb{P}(\\alpha)\\) is  \\(\\beta\\)")),
       plotOutput("distPlot"),
       tags$body(withMathJax(htmlOutput("exampleText")))
    )
  )
))
