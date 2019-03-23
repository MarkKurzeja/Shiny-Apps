
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  navbarPage(
    "Stats 250 Demos",
    ################################################################################
    #                                                                              #
    #                            CLT Visualization App                             #
    #                                                                              #
    ################################################################################
    tabPanel(
      "CLT Application",
      # Application title
      titlePanel("Central Limit Theorem Visualization"),
      
      # Sidebar with a slider input for number of bins 
      sidebarLayout(
        sidebarPanel(
          selectInput("distr", "Choose the Distribution to Sample from", c("Normal", "Uniform", "Skewed", "Dragon"), "Uniform"),
          radioButtons("numSamp", "Size of the Samples to be taken and Averaged Over", c(10,25, 100, 200))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tags$h4("Population Distribution"),
          plotOutput("CLTPop", height = "300px"),
          tags$h4("AVERAGES of Samples taken from the Population Distribution"),
          plotOutput("CLTMean", height = "200px")
        )
      )
    ),
    ################################################################################
    #                                                                              #
    #                            Binomial Approximation                            #
    #                                                                              #
    ################################################################################
    tabPanel(
      "Binomial Approximation",
      # Application title
      titlePanel("Normal Approximation to the Binomial Distribution"),
      tags$h5(textOutput("instructions")),
      # Input options
      sidebarLayout(
        sidebarPanel(
          radioButtons("n", "Number of Trials", choices = c(5,10,20,50,100), inline = T, selected = 50),
          sliderInput("p", "Success Percentage", min = 5, max = 95, step = 5, post = "%", value = 50)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          tags$body(htmlOutput("binomPrompt")), 
          plotOutput("binomApprox")
        )
      )
    ),
    ################################################################################
    #                                                                              #
    #                          Type I and Type II Errors                           #
    #                                                                              #
    ################################################################################
    tabPanel(
      "Type I and Type II Errors",
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
    ),
    ################################################################################
    #                                                                              #
    #                          Conditional Distributions                           #
    #                                                                              #
    ################################################################################
    tabPanel(
      "Conditional Distributions",
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
          tags$h5(textOutput("condPrompt")),
          plotOutput("conditionalPlot")
        )
      )
    )
  )
))
