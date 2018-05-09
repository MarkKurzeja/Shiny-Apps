
library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Visualizing Conditional Distributions"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      tags$h4("Given that X is..."),
      fluidRow(
        column(4, radioButtons("givendirection", "", c("At most", "At least"), "At most")), 
        column(width = 4 , numericInput("givenVal", label = "", value = 15, min = 0, max = 20))),
      tags$hr(),
      tags$h4("What is the probability that X is..."),
      fluidRow(
        column(4, radioButtons("condDirection", "", c("At most", "At least"), "At least")), 
        column(width = 4 , numericInput("condVal", "", 10, 0, 20))
      )
      
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$h5(textOutput("prompt")),
      plotOutput("conditionalPlot")
    )
  )
))
