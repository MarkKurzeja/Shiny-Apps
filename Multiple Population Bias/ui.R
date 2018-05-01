################################################################################
#                                                                              #
# Purpose:       Two Population Bias Simulator                                 #
#                                                                              #
# Author:        Mark Kurzeja                                                  #
# Contact:       mtkurzej@umich.edu                                            #
# Client:        Mark Kurzeja                                                  #
#                                                                              #
# Code created:  2018-02-26                                                    #
# Last updated:  2018-02-26                                                    #
# Source:        C:/Users/Mark/Documents                                       #
#                                                                              #
# Comment:       This shiny app aims to illustrate visually, motivated by sam  #
#                pling body weights at the gym and across UofM, the dangers    #
#                of sampling from a subgroup that is not representative of     #
#                the population that you wish to infer about                   #
#                                                                              #
################################################################################

library(shiny)

################################################################################
#                                                                              #
#               Define the UI for the Two Population Application               #
#                                                                              #
################################################################################
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Multiple Populations"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      ############################ Sampling Radio Button #############################
      radioButtons("radio_populationSelection", "Choose Your Target Population", c("All of UofM", "Gym Goers"),
                   "All of UofM", FALSE),
      ########################### Number of Samples Slider ###########################
      sliderInput("slider_numberOfSamples", "Number of Samples to Draw", 5, 100,
                  50, step = 5),     
      ############################ Size of Samples Slider ############################
      sliderInput("slider_sampleSize", "Size of Each Sample", 5, 100,
                  20, step = 5),    
      ############################ Gym Percentage Slider #############################
      sliderInput("slider_gymPercentage", "What Percentage of UofM goes to the gym", 5, 100,
                  20, step = 5, post = "%"),
      ########################## Button for Drawing Samples ##########################
      actionButton("action_drawSamples", "Draw Samples")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$h3("Populations"),
      htmlOutput("popText"),
      plotOutput("plotPopulations", height = "200px"),
      tags$h3("Sampling Distribution"),
      htmlOutput("sampText"),
      plotOutput("plotSamples", height = "200px"),
      htmlOutput("finalPrompt")
    )
  )
))
