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


################################################################################
#                                                                              #
#                          Define the UI for the app                           #
#                                                                              #
################################################################################
ui <- fluidPage(
  titlePanel("Bivariate Distributions"),
  wellPanel(
    fluidRow(
      column(
        4,
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
        radioButtons(
          "showPrompt",
          "Show Prompt?",
          choices = c("Yes", "No"),
          selected = "Yes"
        )
      )
    ),
    fluidRow(column(10, h5(
      textOutput("prompt")
    )))
  ),
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
)


################################################################################
#                                                                              #
#                           Define the Server Logic                            #
#                                                                              #
################################################################################
server <- function(input, output) {
  mdat <- reactiveValues()
  sessionData <- reactiveValues(values = list(LOADED_DATA <- FALSE),
                                selected = list(On = NA))
  
  ################################################################################
  #                                                                              #
  # Code for loading in the uploaded data - currently this function only         #
  # supports CSV files                                                           #
  #                                                                              #
  ################################################################################
  observeEvent(input$uploadedData, {
    req(input$file1)
    mdat$data <- read.csv(input$file1$datapath,
                          header = T,
                          sep = ",")
    # Remove all NA rows
    data_in <- mdat$data[complete.cases(mdat$data), ][,c(1,2)]
    mdat$data <- data.frame(x = data_in[ ,1], y = data_in[ ,2]) 
    mdat$x <- mdat$data[, 1]
    mdat$y <- mdat$data[, 2]
    mdat$keep    <- data.frame(x = mdat$x, y = mdat$y)
    mdat$exclude <- data.frame(x = mdat$x, y = mdat$y)
    sessionData$values$LOADED_DATA <- TRUE
    sessionData$selected$On <- rep(TRUE, length(mdat$x))
  })
  
  ################################################################################
  #                                                                              #
  # Code for importing the MTCARS dataset which is currently the default data    #
  # set for visualization                                                        #
  #                                                                              #
  ################################################################################
  observeEvent(input$defaultData, {
    mdat$data <- data.frame(x = mtcars$wt, y = mtcars$mpg)
    # Remove all NA rows
    mdat$data <- mdat$data[complete.cases(mdat$data), ]
    mdat$x <- mdat$data$x
    mdat$y <- mdat$data$y
    mdat$keep    <- data.frame(x = mdat$x, y = mdat$y)
    mdat$exclude <- data.frame(x = mdat$x, y = mdat$y)
    sessionData$values$LOADED_DATA <- TRUE
    sessionData$selected$On <- rep(TRUE, length(mdat$x))
  })
  
  ################################################################################
  #                                                                              #
  #            Function for toggling the selected points when brushed            #
  #                                                                              #
  ################################################################################  
  observeEvent(input$plot_click, {
    req(sessionData$values$LOADED_DATA)
    # Get the point that were clicked
    res <- nearPoints(mdat$data, input$plot_click, allRows = TRUE)
    sessionData$selected$On <- xor(res$selected_, sessionData$selected$On)
    mdat$keep = mdat$data[sessionData$selected$On, ]
    mdat$exclude = mdat$data[!sessionData$selected$On, ]
    mdat$x <- mdat$keep$x
    mdat$y <- mdat$keep$y
  })
  
  ################################################################################
  #                                                                              #
  # Create the prompt that can be dynamically updated for the user and explains  #
  # what is going on including the mean, standard deviation, line of best fit,   #
  # etc                                                                          #
  #                                                                              #
  ################################################################################
  output$prompt <- renderText({
    req(sessionData$values$LOADED_DATA)
    req(input$showPrompt == "Yes")
    
    histeff <-
      "We can see that outliers in the graph show up as extreme points in the histogram."
    lineeff <- ""
    meaneff <- ""
    boundseff <- ""
    
    if (input$showLine == "Yes") {
      lmres <- lm(y ~ x, data = mdat$keep)
      lineeff = sprintf(
        "The intercept of the regression line is %.2f and the slope is %.2f. The regression line is 'pulled towards' values that are outliers. Outliers can have a very large effect on the slope and intercept of a regression line, and should always be included with caution.",
        lmres$coefficients[1],
        lmres$coefficients[2]
      )
    }
    if (input$showMeans == "Yes") {
      meaneff = sprintf(
        "The mean of X is %.2f and the mean of Y is %.2f. The mean is NOT robust to outliers, and it is pulled towards outliers more significantly than the median is.",
        mean(mdat$keep$x),
        mean(mdat$keep$y)
      )
    }
    if (input$showBounds == "Yes") {
      boundseff = sprintf(
        "The standard deviation of X is %.2f and the standard deviation of Y is %.2f. The standard deviation is NOT robust to outliers, and it widens when we include outliers in our analysis and narrows when we take outliers out.",
        sd(mdat$keep$x),
        sd(mdat$keep$y)
      )
    }
    
    sprintf("%s %s %s %s", histeff, lineeff, meaneff, boundseff)
  })
  
  ################################################################################
  #                                                                              #
  # The limits for the plot have to be established for the various datasets.     #
  # This function aims to dynamically find the limits that would look good when  #
  # plotting the data as well as determine the graphical parameters for the      #
  # plots                                                                        #
  #                                                                              #
  ################################################################################
  plotlimits <- reactive({
    result <- list()
    
    result$xmin = pretty(mdat$data$x, n = 5) %>% min %>% {. * 0.95}
    result$xmax = pretty(mdat$data$x, n = 5) %>% max %>% {. * 1.05}
    result$ymin = pretty(mdat$data$y, n = 5) %>% min %>% {. * 0.95}
    result$ymax = pretty(mdat$data$y, n = 5) %>% max %>% {. * 1.05}
    
    fauxhist = hist(mdat$data$x, plot = F, breaks = 30)
    result$xmincount <- 0
    result$xmaxcount <- fauxhist$counts %>% max
    fauxhist = hist(mdat$data$y, plot = F, breaks = 30)
    result$ymincount <- 0
    result$ymaxcount <- fauxhist$counts %>% max()
    
    result
  })
  
  ################################################################################
  #                                                                              #
  # Create the main bi-variate plot that we will plot the line of best fit,      #
  # statistics, etc on                                                           #
  #                                                                              #
  ################################################################################
  output$mainPlot <- renderPlot({
    req(sessionData$values$LOADED_DATA)
    base <- ggplot(mdat$keep, aes(x, y)) + geom_point() +
      coord_cartesian(
        xlim = c(plotlimits()$xmin, plotlimits()$xmax),
        ylim = c(plotlimits()$ymin, plotlimits()$ymax)
      ) +
      labs(x = "X", y = "Y") + 
      annotate("text", 
               label = sprintf("The Correlation between Y and X is: %.2f", 
                               cor(mdat$y, mdat$x)), 
               x = plotlimits()$xmax, 
               y = plotlimits()$ymax,
               hjust=1)
    if (mdat$exclude %>% nrow() != 0) {
      base <- base + geom_point(
        data = mdat$exclude,
        shape = 21,
        fill = NA,
        color = "black",
        alpha = 0.25
      ) 
    }
    if (input$showLine == "Yes") {
      base <-
        base + geom_smooth(method = lm,
                           fullrange = TRUE,
                           color = "black")
    }
    if (input$showMeans == "Yes") {
      base <-
        base + geom_hline(yintercept = mean(mdat$keep$y), alpha = 1)
      base <-
        base + geom_vline(xintercept = mean(mdat$keep$x), alpha = 1)
    }
    if (input$showBounds == "Yes") {
      base <-
        base + geom_vline(
          xintercept = mean(mdat$keep$x) - 1 * sd(mdat$keep$x),
          color = "red",
          linetype = 2,
          alpha = .75
        )
      base <-
        base + geom_vline(
          xintercept = mean(mdat$keep$x) + 1 * sd(mdat$keep$x),
          color = "red",
          linetype = 2,
          alpha = .75
        )
      base <-
        base + geom_hline(
          yintercept = mean(mdat$keep$y) + 1 * sd(mdat$keep$y),
          color = "red",
          linetype = 2,
          alpha = .75
        )
      base <-
        base + geom_hline(
          yintercept = mean(mdat$keep$y) - 1 * sd(mdat$keep$y),
          color = "red",
          linetype = 2,
          alpha = .75
        )
    }
    base
  })
  
  ################################################################################
  #                                                                              #
  #                     Plot the Marginal Distribution of X                      #
  #                                                                              #
  ################################################################################
  output$xMarginal <- renderPlot({
    req(sessionData$values$LOADED_DATA)
    base <- ggplot(mdat$keep, aes(x)) + geom_histogram() +
      scale_x_continuous(limits = c(plotlimits()$xmin, plotlimits()$xmax)) +
      scale_y_continuous(
        limits = c(plotlimits()$xmincount, plotlimits()$xmaxcount),
        labels = function(x)
          sprintf("%.0f", x)
      ) +
      labs(x = "X", y = "Counts")
    if (input$showMeans == "Yes") {
      base <- base + geom_vline(xintercept = mean(mdat$keep$x))
    }
    if (input$showBounds == "Yes") {
      base <-
        base + geom_vline(
          xintercept = mean(mdat$keep$x) - 1 * sd(mdat$keep$x),
          color = "red",
          linetype = 2,
          alpha = .5
        )
      base <-
        base + geom_vline(
          xintercept = mean(mdat$keep$x) + 1 * sd(mdat$keep$x),
          color = "red",
          linetype = 2,
          alpha = .5
        )
    }
    base
  })
  
  ################################################################################
  #                                                                              #
  #                     Plot the Marginal Distribution of Y                      #
  #                                                                              #
  ################################################################################
  output$yMarginal <- renderPlot({
    req(sessionData$values$LOADED_DATA)
    base <-
      ggplot(mdat$keep, aes(y)) + geom_histogram() +  coord_flip() +
      scale_x_continuous(limits = c(plotlimits()$ymin, plotlimits()$ymax)) +
      scale_y_continuous(limits = c(plotlimits()$ymincount, plotlimits()$ymaxcount)) +
      labs(x = "Y", y = "Counts")
    if (input$showMeans == "Yes") {
      base <- base + geom_vline(xintercept = mean(mdat$keep$y))
    }
    if (input$showBounds == "Yes") {
      base <-
        base + geom_vline(
          xintercept = mean(mdat$keep$y) + 1 * sd(mdat$keep$y),
          color = "red",
          linetype = 2,
          alpha = .5
        )
      base <-
        base + geom_vline(
          xintercept = mean(mdat$keep$y) - 1 * sd(mdat$keep$y),
          color = "red",
          linetype = 2,
          alpha = .5
        )
    }
    base
  })
}

################################################################################
#                                                                              #
#                             Create the Shiny App                             #
#                                                                              #
################################################################################
shinyApp(ui, server)