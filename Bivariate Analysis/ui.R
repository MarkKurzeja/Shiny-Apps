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
shinyUI(fluidPage(
  titlePanel("Bivariate Distributions"),
  wellPanel(
    fluidRow(
      column(
        4,
        tags$h4("Upload a dataset or choose a dataset"),
        fileInput(
          "file1",
          "Choose CSV File",
          multiple = TRUE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        actionButton("defaultData", 
                     "Use Default Dataset",
                     width = 175
        ),
        actionButton("uploadedData", 
                     "Use Uploaded Dataset",
                     width = 175
        )
      ),
      column(
        3,
        radioButtons(
          "showLine",
          "Show Regression Line?",
          choices = c("Yes", "No"),
          selected = "No"
        ),
        radioButtons(
          "showMeans",
          "Show mean of X & Y?",
          choices = c("Yes", "No"),
          selected = "No"
        )
      ),
      column(
        3,
        radioButtons(
          "showBounds",
          "Show Standard Deviation?",
          choices = c("Yes", "No"),
          selected = "No"
        ),
        selectInput(
          "whichPrompt",
          "Display Option?",
          choices = c("Discussion", "Diagnostics - 1", "Diagnostics - 2", "Diagnostics - 3", "None"),
          selected = "Diagnostics - 2"
        )
      )
    )
  ),
  fluidRow(column(10, 
                  htmlOutput(HTML("prompt"))
  )),
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
))
