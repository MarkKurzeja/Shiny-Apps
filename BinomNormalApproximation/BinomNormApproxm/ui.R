################################################################################
#                                                                              #
# Purpose:       Binomial-Normal Approximation                                 #
#                                                                              #
# Author:        Mark Kurzeja                                                  #
# Contact:       mtkurzej                                                      #
# Client:        Mark Kurzeja                                                  #
#                                                                              #
# Code created:  2018-02-27                                                    #
# Last updated:  2018-02-27                                                    #
#                                                                              #
# Comment:       This app aims to show the relationship between the binomial   #
#                distribution and its normal approximation                     #
#                                                                              #
################################################################################

library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Normal Approximation to the Binomial Distribution"),
  tags$h5(textOutput("instructions")),
  # Input options
  sidebarLayout(
    sidebarPanel(
      radioButtons("n", "Number of Trails", choices = c(5,10,20,50,100), inline = T, selected = 50),
      sliderInput("p", "Success Percentage", min = 5, max = 95, step = 5, post = "%", value = 50),
      selectInput("discrete", "Should the Approximation be Discrete",
                  choices = c(TRUE, FALSE), selected = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$body(htmlOutput("prompt")), 
      plotOutput("binomApprox")
    )
  )
))
