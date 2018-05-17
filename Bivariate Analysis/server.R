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
#                           Define the Server Logic                            #
#                                                                              #
################################################################################

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  mdat <- reactiveValues()
  sessionData <- reactiveValues(values = list(LOADED_DATA <- FALSE),
                                selected = list(On = NA))
  
  # Code for loading in the uploaded data - currently this function only         #
  # supports CSV files                                                           #
  observeEvent(input$uploadedData, {
    req(input$file1)
    mdat$data <- read.csv(input$file1$datapath,
                          header = T,
                          sep = ",")
    # Get the row names for the variable names
    mdat$x_lab_name = colnames(mdat$data)[1]
    mdat$y_lab_name = colnames(mdat$data)[2]
    
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
  
  # Code for importing the MTCARS dataset which is currently the default data    #
  # set for visualization                                                        #
  observeEvent(input$defaultData, {
    
    if (input$defaultDataSelection == "Default - 1") {
      mdat$data <- data.frame(x = mtcars$wt, y = mtcars$mpg)
      
      # Get the row names for the variable names
      mdat$x_lab_name = "Weight of a Vehicle (1000 lbs)"
      mdat$y_lab_name = "Miles Per Gallon"
      
      # Remove all NA rows
      mdat$data <- mdat$data[complete.cases(mdat$data), ]
      mdat$x <- mdat$data$x
      mdat$y <- mdat$data$y
      mdat$keep    <- data.frame(x = mdat$x, y = mdat$y)
      mdat$exclude <- data.frame(x = mdat$x, y = mdat$y)
      sessionData$values$LOADED_DATA <- TRUE
      sessionData$selected$On <- rep(TRUE, length(mdat$x))
    } else {
      stop("A default option has not been implemented yet")
    }
  })
  
  ############ Function for toggling the selected points when brushed ############ 
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
  
  # Create the prompt that can be dynamically updated for the user and explains  #
  # what is going on including the mean, standard deviation, line of best fit,   #
  # etc                                                                          #
  output$prompt <- renderText({
    req(sessionData$values$LOADED_DATA)
    req(input$whichPrompt != "None")
    lmres <- lm(y ~ x, data = mdat$keep)
    
    if(input$whichPrompt == "Discussion") {
      histeff <-
        "We can see that outliers in the graph show up as extreme points in the histogram."
      lineeff <- ""
      meaneff <- ""
      boundseff <- ""
      if ("Display Fit" %in% input$dispOptions) {
        lineeff = sprintf(
          "The intercept of the regression line is %.2f and the slope is %.2f. The regression line is 'pulled towards' values that are outliers. Outliers can have a very large effect on the slope and intercept of a regression line, and should always be included with caution.",
          lmres$coefficients[1],
          lmres$coefficients[2]
        )
      }
      if ("Display Mean" %in% input$dispOptions) {
        meaneff = sprintf(
          "The mean of <i>%s</i> is %.2f and the mean of <i>%s</i> is %.2f. The mean is NOT robust to outliers, and it is pulled towards outliers more significantly than the median is.",
          mdat$x_lab_name,
          mean(mdat$keep$x),
          mdat$y_lab_name,
          mean(mdat$keep$y)
        )
      }
      if ("Display StdDev" %in% input$dispOptions) {
        boundseff = sprintf(
          "The standard deviation of <i>%s</i> is %.2f and the standard deviation of <i>%s</i> is %.2f. The standard deviation is NOT robust to outliers, and it widens when we include outliers in our analysis and narrows when we take outliers out.",
          mdat$x_lab_name,
          sd(mdat$keep$x),
          mdat$y_lab_name,
          sd(mdat$keep$y)
        )
      }
      sprintf("%s %s %s %s", histeff, lineeff, meaneff, boundseff) %>% return()
    } else if(input$whichPrompt == "Correlation") {
      sprintf("<strong>Correlation</strong>:<br>
              &emsp;\\(R\\): %.2f<br>
              &emsp;\\(R^2\\): %.2f<br><script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>",
              cor(mdat$y, mdat$x),
              summary(lmres)$r.squared
      ) %>% return()
    } else if(input$whichPrompt == "Diagnostics - 1") {
      stargazer::stargazer(lmres, type = "html", ci = T)
    } else if(input$whichPrompt == "Diagnostics - 2") {
      sjt.lm(lmres,
             string.est = "Estimate",
             string.ci = "Conf. Int.",
             string.p = "p-value")$page.complete
    } else if(input$whichPrompt == "Diagnostics - 3") {
      sprintf("<strong>Relationships</strong>:<br>
              &emsp;\\(R\\): %.2f<br>
              &emsp;\\(R^2\\): %.2f<br>
              <strong>Estimates</strong>:<br>
              &emsp;Intercept: %.2f<br>
              &emsp;Slope: %.2f<br>
              &emsp;Line of Best Fit: \\(%.2f + %.2fx\\)<br>
              <strong>Standard Errors</strong>:<br>
              &emsp;Intercept: %.2f<br>
              &emsp;Slope: %.2f<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>",
              cor(mdat$y, mdat$x),
              summary(lmres)$r.squared,
              lmres$coefficients[1],
              lmres$coefficients[2],
              lmres$coefficients[1],
              lmres$coefficients[2],
              summary(lmres)$coefficients[1,2],
              summary(lmres)$coefficients[2,2]
      ) %>% return()
    } else { # Something went wrong
      stop()
    }
  })
  
  # The limits for the plot have to be established for the various datasets.     #
  # This function aims to dynamically find the limits that would look good when  #
  # plotting the data as well as determine the graphical parameters for the      #
  # plots                                                                        #
  plotlimits <- reactive({
    result <- list()
    
    result$xmin = pretty(mdat$data$x, n = 5) %>% min %>% {. * 0.95}
    result$xmax = pretty(mdat$data$x, n = 5) %>% max %>% {. * 1.05}
    result$ymin = pretty(mdat$data$y, n = 5) %>% min %>% {. * 0.95}
    result$ymax = pretty(mdat$data$y, n = 5) %>% max %>% {. * 1.05}
    
    result
  })
  
  # Create the main bi-variate plot that we will plot the line of best fit,      #
  # statistics, etc on                                                           #
  output$mainPlot <- renderPlot({
    req(sessionData$values$LOADED_DATA)
    base <- ggplot(mdat$keep, aes(x, y)) + geom_point() +
      coord_cartesian(
        xlim = c(plotlimits()$xmin, plotlimits()$xmax),
        ylim = c(plotlimits()$ymin, plotlimits()$ymax)
      ) +
      labs(x = mdat$x_lab_name, y = mdat$y_lab_name)  
    if (mdat$exclude %>% nrow() != 0) {
      base <- base + geom_point(
        data = mdat$exclude,
        shape = 21,
        fill = NA,
        color = "black",
        alpha = 0.25
      ) 
    }
    if ("Display Fit" %in% input$dispOptions) {
      base <-
        base + geom_smooth(method = lm,
                           fullrange = TRUE,
                           color = "blue")
    }
    if ("Display Mean" %in% input$dispOptions) {
      base <-
        base + geom_hline(yintercept = mean(mdat$keep$y), alpha = 1)
      base <-
        base + geom_vline(xintercept = mean(mdat$keep$x), alpha = 1)
    }
    if ("Display StdDev" %in% input$dispOptions) {
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
  
  ##################### Plot the Marginal Distribution of X ######################
  output$xMarginal <- renderPlot({
    req(sessionData$values$LOADED_DATA)
    base <- ggplot(mdat$keep, aes(x)) + geom_histogram() +
      scale_x_continuous(limits = c(plotlimits()$xmin, plotlimits()$xmax)) +
      scale_y_continuous(
        labels = function(x)
          sprintf("%.0f", x)
      ) +
      labs(x = mdat$x_lab_name, y = "Counts")
    if ("Display Mean" %in% input$dispOptions) {
      base <- base + geom_vline(xintercept = mean(mdat$keep$x))
    }
    if ("Display StdDev" %in% input$dispOptions) {
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
  ##################### Plot the Marginal Distribution of Y ######################
  output$yMarginal <- renderPlot({
    req(sessionData$values$LOADED_DATA)
    base <-
      ggplot(mdat$keep, aes(y)) + geom_histogram() +  coord_flip() +
      scale_x_continuous(limits = c(plotlimits()$ymin, plotlimits()$ymax)) +
      labs(x = mdat$y_lab_name, y = "Counts")
    if ("Display Mean" %in% input$dispOptions) {
      base <- base + geom_vline(xintercept = mean(mdat$keep$y))
    }
    if ("Display StdDev" %in% input$dispOptions) {
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
})
