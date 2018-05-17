################################################################################
#                                                                              #
# Purpose:       Bivariate Analysis Application                                #
#                                                                              #
# Author:        Mark Kurzeja                                                  #
# Contact:       mtkurzej@umich.edu                                            #
# Client:        Mark Kurzeja                                                  #
#                                                                              #
# Comment:       This app aims to plot a simple bi-variate set of data, visua  #
#                lizations, marginal distributions, and other metrics that     #
#                are useful in data analysis                                   #
#                                                                              #
################################################################################

################################################################################
#                                                                              #
#                                House Cleaning                                #
#                                                                              #
################################################################################
library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)
library(sjPlot)

################################################################################
#                                                                              #
#                          Define the UI for the app                           #
#                                                                              #
################################################################################

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    navbarPage(
      "Cousera Apps",
      ################################################################################
      #                                                                              #
      #                              Bivariate Plot Tab                              #
      #                                                                              #
      ################################################################################
      tabPanel(
        "Bivariate Plot",
        titlePanel("Bivariate Distributions"),
        wellPanel(
          fluidRow(
            column(
              11,
              fluidRow(
                column(
                  11,
                  tags$h4("Upload a dataset or choose a dataset")
                )
              ),
              column(
                8,
                fluidRow(
                  column(
                    8,
                    tags$h5("Choose a Default Dataset")
                  )
                ),
                fluidRow(
                  column(
                    8,
                    column(
                      6,
                      selectInput(
                        "defaultDataSelection",
                        label = NULL,
                        choices = c("Default - 1")
                      )
                    ),
                    column(
                      2,
                      actionButton(
                        "defaultData",
                        "Use Default Dataset",
                        width = 175
                      )
                    )
                  )
                ),
                fluidRow(
                  column(
                    8,
                    tags$h5("Or Upload your Own CSV")
                  )
                ),
                fluidRow(
                  column(
                    8,
                    column(
                      6,
                      fileInput(
                        "file1",
                        label = NULL,
                        multiple = TRUE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")
                      )
                    ),
                    column(
                      2,
                      actionButton(
                        "uploadedData",
                        "Use Uploaded Dataset",
                        width = 175
                      )
                    )
                  )
                )
              ),
              column(
                3,
                checkboxGroupInput(
                  "dispOptions",
                  "Display Options",
                  choices = c("Display Mean", "Display StdDev", "Display Fit")
                ),
                selectInput(
                  "whichPrompt",
                  "Display Option?",
                  choices = c(
                    "Discussion",
                    "Correlation",
                    "Diagnostics - 1",
                    "Diagnostics - 2",
                    "Diagnostics - 3",
                    "None"
                  ),
                  selected = "None"
                )
              )
            )
          )
        ),
        
        ################################# Prompt Line ##################################
        fluidRow(
          column(
            10,
            withMathJax(htmlOutput("prompt"))
          )
        ),
        ######################### Main Panel of Bivariate Plot #########################
        fluidRow(
          column(
            3,
            h4("Marginal Distribution of Y"),
            plotOutput("yMarginal", height = 350)
          ),
          column(
            9,
            h4("Joint Distribution"),
            plotOutput("mainPlot", height = 350, click = "plot_click"),
            h4("Marginal Distribution of X"),
            plotOutput("xMarginal", height = 150)
          )
        )
      ),
      # End of Bivariate Tab
      tabPanel("Interactive Histogram",
               titlePanel("Interactive Histogram"))
    ) # End of tabset Panel
  )
) # End of fluid page and UI)