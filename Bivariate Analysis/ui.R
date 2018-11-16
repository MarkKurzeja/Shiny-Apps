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
library(tidyr)
library(stargazer)

################################################################################
#                                                                              #
#                          Define the UI for the app                           #
#                                                                              #
################################################################################

# Define UI for application that draws a histogram
shinyUI(
  fluidPage(
    navbarPage(
      "Coursera Apps",
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
              12,
              fluidRow(
                column(
                  12,
                  tags$h4("Upload a dataset or choose a dataset")
                )
              ),
              column(
                7,
                fluidRow(
                  column(
                    7,
                    tags$h5("Choose a Default Dataset")
                  )
                ),
                fluidRow(
                  column(
                    7,
                    column(
                      6,
                      selectInput(
                        "defaultDataSelection",
                        label = NULL,
                        choices = c(
                          "Wt | MPG",
                          "Bivar. Normal"
                        )
                      )
                    ),
                    column(
                      1,
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
                    7,
                    tags$h5("Or Upload your Own CSV")
                  )
                ),
                fluidRow(
                  column(
                    7,
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
                      1,
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
                2,
                tags$h4("Plotting Options"),
                checkboxGroupInput(
                  "dispOptions",
                  label = NULL,
                  choices = c("Display Mean", "Display StdDev", "Display Fit")
                )
              ),
              column(
                3,
                tags$h4("Prompt Display Options"),
                selectInput(
                  "whichPrompt",
                  label = NULL,
                  choices = c(
                    "Discussion",
                    "Correlation",
                    "Diagnostics - 1",
                    "Diagnostics - 2",
                    "Diagnostics - 3",
                    "None"
                  ),
                  selected = "None"
                ),
                tags$h4("Correlation Slider"),
                sliderInput(
                  "corrSlider",
                  label = NULL,
                  min = -1, 
                  max = 1, 
                  value = 0, 
                  step = 0.10
                )
              )
            )
          )
        ),
        ################################# Prompt Line ##################################
        fluidRow(
          column(
            10,
            withMathJax(htmlOutput("bivarprompt"))
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
      ################################################################################
      #                                                                              #
      #                                Binary Plot Tab                               #
      #                                                                              #
      ################################################################################
      tabPanel(
        "Binary Regression",
        titlePanel("Binary Regression"),
        wellPanel(
          fluidRow(
            column(
              12,
              fluidRow(
                column(
                  12,
                  tags$h4("Upload a dataset or choose a dataset")
                )
              ),
              column(
                7,
                fluidRow(
                  column(
                    7,
                    tags$h5("Choose a Default Dataset")
                  )
                ),
                fluidRow(
                  column(
                    7,
                    column(
                      6,
                      selectInput(
                        "defaultDataSelection",
                        label = NULL,
                        choices = c(
                          "Wt | MPG",
                          "Bivar. Normal"
                        )
                      )
                    ),
                    column(
                      1,
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
                    7,
                    tags$h5("Or Upload your Own CSV")
                  )
                ),
                fluidRow(
                  column(
                    7,
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
                      1,
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
                2,
                tags$h4("Plotting Options"),
                checkboxGroupInput(
                  "dispOptions",
                  label = NULL,
                  choices = c("Display Mean", "Display Fit")
                )
              ),
              column(
                3,
                tags$h4("Prompt Display Options"),
                selectInput(
                  "whichPrompt",
                  label = NULL,
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
                # tags$h4("Correlation Slider"),
                # sliderInput(
                #   "corrSlider",
                #   label = NULL,
                #   min = -1, 
                #   max = 1, 
                #   value = 0, 
                #   step = 0.10
                # )
              )
            )
          )
        ),
        ################################# Prompt Line ##################################
        fluidRow(
          column(
            10,
            withMathJax(htmlOutput("bivarprompt"))
          )
        ),
        ######################### Main Panel of Bivariate Plot #########################
        fluidRow(
          column(
            3,
            h4("Distribution of Responses"),
            plotOutput("yMarginal", height = 350)
          ),
          column(
            9,
            h4("Response Vs. X"),
            plotOutput("mainPlot", height = 350, click = "plot_click"),
            h4("Distribution of X"),
            plotOutput("xMarginal", height = 150)
          )
        )
      ),
      # End of binary tab
      ################################################################################
      #                                                                              #
      #                                Demo Dataset                                  #
      #                                                                              #
      ################################################################################
      tabPanel(
        "Data Visualization",
        titlePanel("Data Visualization"),
        sidebarLayout(
          sidebarPanel(
            fileInput(
              "file2",
              label = NULL,
              multiple = FALSE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
            ),
            actionButton(
              "uploadSummaryData",
              "Upload User Data"
            ),
            actionButton(
              "uploadDefaultData",
              "Use Default Dataset"
            ),
            selectInput(
              "cartWheelPlotVar",
              "Which variable would you like to plot?",
              choices = c(
                ""
              ),
              selected = ""
            ),
            selectInput(
              "cartWheelFacet",
              "Which variable would you like to facet by?",
              choices = c(
                "None"
              ),
              selected = "None"
            ),
            sliderInput(
              "cartWheelHistBinsNum",
              "Number of Bins",
              min = 3, 
              max = 30,
              step = 1, 
              value = 20
            ),
            actionButton(
              "cartWheelUpdate",
              "Update Plots"
            )
          ),
          mainPanel(
            tags$h4("Summaries"),
            withMathJax(htmlOutput("cartWheelPromptFirst")),
            tags$h4("Boxplot"),
            plotOutput("cartWheelBoxPlot", height = 200),
            tags$h4("Histogram"),
            plotOutput("cartWheelHistogram", height = 200)
          )
        ) # End of the Cartwheel Visualization      
      ) # End of tabset Panel
    ) # End of the nav bar page
  ) # End of fluid page and UI)
)