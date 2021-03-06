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
library(mvtnorm)

################################################################################
#                                                                              #
#                           Define the Server Logic                            #
#                                                                              #
################################################################################

# Define server logic required to draw a histogram
shinyServer(function(session, input, output) {
  ###############################################################################
  #                                                                              #
  #                            Bivariate Visualisation                           #
  #                                                                              #
  ################################################################################
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
    mdat$corr <- NA
    sessionData$values$LOADED_DATA <- TRUE
    sessionData$selected$On <- rep(TRUE, length(mdat$x))
  })
  
  # Code for importing the MTCARS dataset which is currently the default data    #
  # set for visualization                                                        #
  observeEvent(input$defaultData, {
    
    if (input$defaultDataSelection == "Wt | MPG") {
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
    } else if(input$defaultDataSelection == "Bivar. Normal") {
      set.seed(123)
      corr <- input$corrSlider
      k = mvtnorm::rmvnorm(400, sigma = matrix(c(1, corr, corr, 1), nrow = 2))
      mdat$data <- data.frame(x = k[,1], y = k[,2])
      
      # Get the row names for the variable names
      mdat$x_lab_name = "X"
      mdat$y_lab_name = "Y"
      
      mdat$x <- mdat$data$x
      mdat$y <- mdat$data$y
      mdat$keep    <- data.frame(x = mdat$x, y = mdat$y)
      mdat$exclude <- data.frame(x = mdat$x, y = mdat$y)
      mdat$corr <- input$corrSlider
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
  output$bivarprompt <- renderText({
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
              ifelse(input$defaultDataSelection == "Bivar. Normal", mdat$corr, cor(mdat$y, mdat$x)),
              ifelse(input$defaultDataSelection == "Bivar. Normal", mdat$corr^2, summary(lmres)$r.squared)
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
              ifelse(input$defaultDataSelection == "Bivar. Normal", mdat$corr, cor(mdat$y, mdat$x)),
              ifelse(input$defaultDataSelection == "Bivar. Normal", mdat$corr^2, summary(lmres)$r.squared),
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
        labels = function(x) {
          sprintf("%.0f", x)
        }
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
  ################################################################################
  #                                                                              #
  #                              Cartwheel Dataset                               #
  #                                                                              #
  ################################################################################
  
  cwdata <- reactiveValues()
  cwsessionData <- reactiveValues(values = list(LOADED_DATA <- FALSE))
  
  # Function for updating the UI that happens in both the default case and in the 
  # user-data case
  UpdateUserInterfaceChoices <- function() {
    updateSelectInput(session, 
                      inputId = "cartWheelPlotVar",
                      choices = colnames(cwdata$data$data),
                      selected = colnames(cwdata$data$data)[1]
    )
    
    # For the faceting variables, we need to ensure that the number of categories
    # is less than some bound or else this could blow up in the number of unique options
    # by default, anything with more than four categories will not be included
    facetingColnames = apply(X = cwdata$data$data, 
                             MARGIN = 2, 
                             FUN = function(x) {
                               length(unique(x))
                             }) %>% {.[. <= 4]} %>% names()
    # Update the faceting variables
    updateSelectInput(session, 
                      inputId = "cartWheelFacet",
                      choices = c("None", facetingColnames),
                      selected = "None"
    )
  }
  
  observeEvent(input$uploadSummaryData, {
    req(input$file2)
    cwdata$data$data = read.csv(input$file2$datapath,
                                header = T,
                                sep = ",")
    
    # Update the UI with the new variable names
    UpdateUserInterfaceChoices()
    
    cwsessionData$values$LOADED_DATA <- TRUE
  })
  
  observeEvent(input$uploadDefaultData, {
    cwdata$data$data = mtcars
    
    # Update the UI with the new variable names
    UpdateUserInterfaceChoices()
    
    cwsessionData$values$LOADED_DATA <- TRUE
  })
  
  observeEvent(input$cartWheelUpdate, {
    req(cwsessionData$values$LOADED_DATA)
    
    # Return back to updating the reactiveValues
    cwdata$data$x = cwdata$data$data[[input$cartWheelPlotVar]]
    cwdata$data$name = input$cartWheelPlotVar
    cwdata$data$min = cwdata$data$x %>% pretty() %>% min %>% {. * 1}
    cwdata$data$max = cwdata$data$x %>% pretty() %>% max %>% {. * 1}
    if (input$cartWheelFacet == "None") {
      cwdata$data$facet <- NA
    } else {
      cwdata$data$facet <- cwdata$data$data[[input$cartWheelFacet]]
    }
    cwdata$data$firstPlot = TRUE
  })
  
  # This is the description render that outputs to the user the
  # text that gives the description - now handles faceting
  output$cartWheelPromptFirst <- renderText({
    req(cwdata$data$firstPlot)
    
    # alias long variable names
    var_name = input$cartWheelPlotVar
    facet_name = input$cartWheelFacet
    
    # Do some data-ninja to handle faceting 
    temp_text <- cwdata$data$data 
    if (facet_name != "None") {
      temp_text %<>% group_by_(facet_name)
    }
    temp_text %<>% 
      summarize_(
        Min = sprintf("min(%s)", var_name),
        Q1 = sprintf("quantile(%s, prob = 0.25)", var_name),
        Mean = sprintf("round(mean(%s),2)", var_name),
        Median = sprintf("median(%s)", var_name),
        Q3 = sprintf("quantile(%s, prob = 0.75)", var_name),
        Max = sprintf("max(%s)", var_name),
        n = sprintf("n()", var_name),
        SD = sprintf("round(sd(%s),3)", var_name)
      )
    if(facet_name != "None") {
      temp_text %<>% gather("key", "value", -facet_name) %>% # gather to long
        tidyr::spread(facet_name, value) %>% # spread the table
        .[c(4, 6, 2, 3, 7, 1, 5, 8), ] %>% # order the rows
        dplyr::rename(" " = key)
    } else {
      temp_text %<>% gather("key", "value") 
      colnames(temp_text) <- c(" ", var_name)
    }
    
    # Push through stargazer as a html format for 
    # dataframe
    temp_text %<>% stargazer::stargazer(
      summary = F, rownames = F, type = "html")
    
    # Include spacing constraints in Custom CSS
    temp_text <- paste("<style>
                       
                       table, td, th {
                       border: none;
                       padding-left: 1em;
                       padding-right: 1em;
                       min-width: 50%;
                       margin-left: auto;
                       margin-right: auto;
                       margin-top: 1em;
                       margin-bottom: 1em;
                       }
                       
                       </style>", temp_text)
    
    return(temp_text)
  })  
  
  # The box plot
  output$cartWheelBoxPlot <- renderPlot({
    req(cwdata$data$firstPlot)
    if(all(is.na(cwdata$data$facet))) {
      ggplot(data.frame(y = cwdata$data$x)) + 
        geom_boxplot(aes(x = 1, y = y)) + 
        coord_flip() + 
        labs(y = cwdata$data$name, x = " ") 
    } else {
      ggplot(data.frame(y = cwdata$data$x, facet = cwdata$data$facet)) + 
        facet_wrap(~facet, ncol = 1)+ 
        geom_boxplot(aes(x = 1, y = y)) + 
        coord_flip() + 
        labs(y = cwdata$data$name, x = " ") 
    }
  })
  
  # The histogram
  output$cartWheelHistogram <- renderPlot({
    req(cwdata$data$firstPlot)
    if(all(is.na(cwdata$data$facet))) {
      ggplot(data.frame(y = cwdata$data$x)) + 
        geom_histogram(aes(y), bins = input$cartWheelHistBinsNum) + 
        labs(x = cwdata$data$name, y = " ")
    } else {
      ggplot(data.frame(y = cwdata$data$x, facet = cwdata$data$facet)) +
        facet_wrap(~facet, ncol = 1) + 
        geom_histogram(aes(y), bins = input$cartWheelHistBinsNum) + 
        labs(x = cwdata$data$name, y = " ")
    }
  })
})
