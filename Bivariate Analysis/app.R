
################################################################################
#                                                                              #
# Purpose:       Sensitivity to Outliers Application                           #
#                                                                              #
# Contact:       mtkurzej@umich.edu                                            #
#                                                                              #
# Code created:  2018-04-06                                                    #
# Last updated:  2018-04-06                                                    #
#                                                                              #
# Comment:       Application that can dynamically adjust means, standard devi  #
#                ations, and regression lines to a scatterplot of data         #
#                                                                              #
################################################################################

library(ggplot2)
library(ggExtra)
library(magrittr)

################################################################################
#                                                                              #
#                                  Impl of UI                                  #
#                                                                              #
################################################################################
ui <- fluidPage(
  titlePanel("Sensitivity to Outliers"),
  wellPanel(fluidRow(
    column(3,
     radioButtons("showLine", "Show Regression Line?", choices = c("Yes", "No"), selected = "No")
    ),
    column(3,
     radioButtons("showMeans", "Show mean of X & Y?", choices = c("Yes", "No"), selected = "No")
    ),
    column(3,
     radioButtons("showBounds", "Show Standard Deviation?", choices = c("Yes", "No"), selected = "No")
    ),
    column(3, 
     actionButton("exclude_reset", "Reset Points")
    )
  )), 
  fluidRow(
    h5(textOutput("prompt"))
  ),
  fluidRow(
    h5(textOutput("corrtxt"))
  ),
  fluidRow(
    column(3,
       h4("Marginal Distribution of Y"),
       plotOutput("yMarginal",height = 350)
       ),
    column(9,
       h4("Joint Distribution"),
       plotOutput("mainPlot", height = 350,
                  click = "plot1_click",
                  brush = brushOpts(
                    id = "plot1_brush"
                  )
       ),
       h4("Marginal Distribution of X"),
       plotOutput("xMarginal",height = 150)
    )
  )
)

################################################################################
#                                                                              #
#                                 Server Logic                                 #
#                                                                              #
################################################################################
server <- function(input, output) {
  # For storing which rows have been excluded
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(mtcars))
  )
  
  # Express limits of the plotting range as finite or dynamic values
  plotlimits <- reactive({
    result <- list()
    # result$xmin = min(mtcars$wt)
    # result$xmax = max(mtcars$wt) + .05
    # result$ymin = min(mtcars$mpg)
    # result$ymax = max(mtcars$mpg)
    result$xmin = 1.25
    result$xmax = 5.5
    result$ymin = 8
    result$ymax = 35
    
    # fauxhist = hist(mtcars$wt, plot = F, breaks = 30)
    # result$xmincount <- fauxhist$counts %>% min()
    # result$xmaxcount <- fauxhist$counts %>% max
    # fauxhist = hist(mtcars$mpg, plot = F, breaks = 30)
    # result$ymincount <- fauxhist$counts %>% min()
    # result$ymaxcount <- fauxhist$counts %>% max()
    # fauxhist = hist(mtcars$wt, plot = F, breaks = 30)
    result$xmincount <- 0
    result$xmaxcount <- 4
    # fauxhist = hist(mtcars$mpg, plot = F, breaks = 30)
    result$ymincount <- 0
    result$ymaxcount <- 5
    result
  })
  
  # Get the correlation and plot it out to the user
  output$corrtxt <- renderText({
    
    keep    <- mtcars[ vals$keeprows, , drop = FALSE]
    exclude <- mtcars[!vals$keeprows, , drop = FALSE]
    
    sprintf("The Correlation between X and Y is: %.2f", cor(keep$mpg, keep$wt))
    
  })
  
  
  # Dynamic Interactive prompt
  output$prompt <- renderText({
    
    keep    <- mtcars[ vals$keeprows, , drop = FALSE]
    exclude <- mtcars[!vals$keeprows, , drop = FALSE]
    
    histeff <- "We can see that outliers in the graph show up as extreme points in the histogram."
    lineeff <- ""
    meaneff <- ""
    boundseff <- ""
    # browser()
    
    if(input$showLine == "Yes") {
      lmres <- lm(mpg ~ wt, data = keep)
    lineeff = sprintf("The intercept of the regression line is %.2f and the slope is %.2f. The regression line is 'pulled towards' values that are outliers. Outliers can have a very large effect on the slope and intercept of a regression line, and should always be included with caution.", lmres$coefficients[1], lmres$coefficients[2
                                                                                                                                                                                                                                                                                                                                            ])
    }
    if(input$showMeans == "Yes") {
      meaneff = sprintf("The mean of X is %.2f and the mean of Y is %.2f. The mean is NOT robust to outliers, and it is pulled towards these values more significently than the median is.", mean(keep$wt), mean(keep$mpg))
    }
    if(input$showBounds == "Yes") {
      boundseff = sprintf("The standard deviation of X is %.2f and the standard deviation of Y is %.2f. The standard deviation is NOT robust to outliers, and it widens when we include them in our analysis and narrows when we take them out.",  sd(keep$wt), sd(keep$mpg))
    }
    
    sprintf("%s %s %s %s", histeff, lineeff, meaneff, boundseff)
    
  })
  
  # Main plot
  output$mainPlot <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- mtcars[ vals$keeprows, , drop = FALSE]
    exclude <- mtcars[!vals$keeprows, , drop = FALSE]
    
    base <- ggplot(keep, aes(wt, mpg)) + geom_point() +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
      coord_cartesian(xlim = c(plotlimits()$xmin, plotlimits()$xmax), 
                      ylim = c(plotlimits()$ymin,plotlimits()$ymax)) +
      labs(x = "X", y = "Y")
    if(input$showLine == "Yes") {
      base <- base + geom_smooth(method = lm, fullrange = TRUE, color = "black") 
    }
    if(input$showMeans == "Yes") {
      base <- base + geom_hline(yintercept = mean(keep$mpg), alpha = .5)
      base <- base + geom_vline(xintercept = mean(keep$wt), alpha = .5)
    }
    if(input$showBounds == "Yes") {
      base <- base + geom_vline(xintercept = mean(keep$wt) - 1 * sd(keep$wt), color = "red", linetype = 2, alpha = .75)
      base <- base + geom_vline(xintercept = mean(keep$wt) + 1 * sd(keep$wt), color = "red", linetype = 2, alpha = .75)
      base <- base + geom_hline(yintercept = mean(keep$mpg) + 1 * sd(keep$mpg), color = "red", linetype = 2, alpha = .75)
      base <- base + geom_hline(yintercept = mean(keep$mpg) - 1 * sd(keep$mpg), color = "red", linetype = 2, alpha = .75)
    }
    base
  })
  
  # Plot of the x-marginal
  output$xMarginal <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- mtcars[ vals$keeprows, , drop = FALSE]
    exclude <- mtcars[!vals$keeprows, , drop = FALSE]
    # browser()
    base <- ggplot(keep, aes(wt)) + geom_histogram() +
      scale_x_continuous(limits = c(plotlimits()$xmin, plotlimits()$xmax)) +
      scale_y_continuous(limits = c(plotlimits()$xmincount, plotlimits()$xmaxcount)) + 
      labs(x = "X", y = "Counts")
    if(input$showMeans == "Yes") {
      base <- base + geom_vline(xintercept = mean(keep$wt))
    }
    if(input$showBounds == "Yes") {
      base <- base + geom_vline(xintercept = mean(keep$wt) - 1 * sd(keep$wt), color = "red", linetype = 2, alpha = .5)
      base <- base + geom_vline(xintercept = mean(keep$wt) + 1 * sd(keep$wt), color = "red", linetype = 2, alpha = .5)
    }
    base
  })
  
  # Plot of the y-marginal
  output$yMarginal <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    keep    <- mtcars[ vals$keeprows, , drop = FALSE]
    exclude <- mtcars[!vals$keeprows, , drop = FALSE]
    # browser()
    base <- ggplot(keep, aes(mpg)) + geom_histogram() +  coord_flip() +
      scale_x_continuous(limits = c(plotlimits()$ymin, plotlimits()$ymax)) +
      scale_y_continuous(limits = c(plotlimits()$ymincount, plotlimits()$ymaxcount)) +
      labs(x = "Y", y = "Counts")
    if(input$showMeans == "Yes") {
      base <- base + geom_vline(xintercept = mean(keep$mpg))
    }
    if(input$showBounds == "Yes") {
      base <- base + geom_vline(xintercept = mean(keep$mpg) + 1 * sd(keep$mpg), color = "red", linetype = 2, alpha = .5)
      base <- base + geom_vline(xintercept = mean(keep$mpg) - 1 * sd(keep$mpg), color = "red", linetype = 2, alpha = .5)
    }
    base
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    res <- nearPoints(mtcars, input$plot1_click, allRows = TRUE)
    
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(mtcars))
  })
  
}

# Load the app
shinyApp(ui, server)