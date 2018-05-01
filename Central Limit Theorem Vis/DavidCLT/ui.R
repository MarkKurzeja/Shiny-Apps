#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Central Limit Theorem Visualization"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       selectInput("distr", "Choose the Distribution to Sample from", c("Normal", "Uniform", "Skewed", "Dragon"), "Uniform"),
       radioButtons("numSamp", "Number of Samples to Average", c(10,25, 100, 200))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$h4("Population Distribution"),
       plotOutput("CLTPop", height = "300px"),
      tags$h4("AVERAGES of Samples taken from the Population Distribution"),
       plotOutput("CLTMean", height = "200px")
    )
  )
))
