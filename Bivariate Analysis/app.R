
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
  fluidRow(column(8,
                  h5(textOutput("prompt"))
  ),
  column(4,
         fileInput("infile", "Upload Your Data",
                   accept = c("text/csv", 
                              "text/comma-seperated-values,text/plain",
                              ".csv"))
  )),
  fluidRow(
    h4(textOutput("corrtxt"))
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
  # Get the data either from the user or by default
  getData <- reactive({
    inFile <- input$infile
    
    result <- list()
    if(is.null(inFile)) {
      # Work with MTCARS
      result$data <- mtcars
      result$data$x <- mtcars$wt
      result$data$y <- mtcars$mpg
    } else {
      # Use the file that was supplied for the data
      result$data <- read.csv(inFile$datapath, header = input$header)
    }
    result
  })
  
  # For storing which rows have been excluded
  vals <- reactiveValues({
    keeprows = rep(TRUE, nrow(getData()$data))
  })
  
  # Function for getting the add and dropped rows
  keepExclude <- reactive({
    data <- getData()
    result <- list()
    result$keep <- data$data[ vals$keeprows, , drop = FALSE]
    result$exclude <- data$data[!vals$keeprows, , drop = FALSE]
    result
    
  })
  
  
  
  # Express limits of the plotting range as finite or dynamic values
  plotlimits <- reactive({
    result <- list()
    
    data <- getData()
    
    result$xmin = pretty(data$data$x, n = 5) %>% min %>% {. * 0.99}
    result$xmax = pretty(data$data$x, n = 5)%>% max %>% {. * 1.01}
    result$ymin = pretty(data$data$y, n = 5)%>% min %>% {. * 0.99}
    result$ymax = pretty(data$data$y, n = 5)%>% max %>% {. * 1.01}
    
    fauxhist = hist(data$data$x, plot = F, breaks = 30)
    result$xmincount <- 0
    result$xmaxcount <- fauxhist$counts %>% max
    fauxhist = hist(data$data$y, plot = F, breaks = 30)
    result$ymincount <- 0
    result$ymaxcount <- fauxhist$counts %>% max()
    
    # browser()
    
    ############################# Hard Code for MTCARS #############################
    # result$xmin = 1.25
    # result$xmax = 5.5
    # result$ymin = 8
    # result$ymax = 35
    # result$xmincount <- 0
    # result$xmaxcount <- 4
    # result$ymincount <- 0
    # result$ymaxcount <- 5
    ########################### End Hard Code for MTCARS ###########################
    
    result
  })
  
  # Get the correlation and plot it out to the user
  output$corrtxt <- renderText({
    data <- keepExclude()
    keep    <- data$keep
    exclude <- data$exclude
    
    sprintf("The Correlation between Y and X is: %.2f", cor(keep$y, keep$x))
  })
  
  
  # Dynamic Interactive prompt
  output$prompt <- renderText({
    data <- keepExclude()
    keep    <- data$keep
    exclude <- data$exclude
    
    histeff <- "We can see that outliers in the graph show up as extreme points in the histogram."
    lineeff <- ""
    meaneff <- ""
    boundseff <- ""
    
    if(input$showLine == "Yes") {
      lmres <- lm(y ~ x, data = keep)
      lineeff = sprintf("The intercept of the regression line is %.2f and the slope is %.2f. The regression line is 'pulled towards' values that are outliers. Outliers can have a very large effect on the slope and intercept of a regression line, and should always be included with caution.", lmres$coefficients[1], lmres$coefficients[2
                                                                                                                                                                                                                                                                                                                                              ])
    }
    if(input$showMeans == "Yes") {
      meaneff = sprintf("The mean of X is %.2f and the mean of Y is %.2f. The mean is NOT robust to outliers, and it is pulled towards these values more significantly than the median is.", mean(keep$x), mean(keep$y))
    }
    if(input$showBounds == "Yes") {
      boundseff = sprintf("The standard deviation of X is %.2f and the standard deviation of Y is %.2f. The standard deviation is NOT robust to outliers, and it widens when we include them in our analysis and narrows when we take them out.",  sd(keep$x), sd(keep$y))
    }
    
    sprintf("%s %s %s %s", histeff, lineeff, meaneff, boundseff)
    
  })
  
  # Main plot
  output$mainPlot <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    data <- keepExclude()
    keep    <- data$keep
    exclude <- data$exclude
    
    base <- ggplot(keep, aes(x, y)) + geom_point() +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
      coord_cartesian(xlim = c(plotlimits()$xmin, plotlimits()$xmax), 
                      ylim = c(plotlimits()$ymin,plotlimits()$ymax)) +
      labs(x = "X", y = "Y")
    if(input$showLine == "Yes") {
      base <- base + geom_smooth(method = lm, fullrange = TRUE, color = "black") 
    }
    if(input$showMeans == "Yes") {
      base <- base + geom_hline(yintercept = mean(keep$y), alpha = .5)
      base <- base + geom_vline(xintercept = mean(keep$x), alpha = .5)
    }
    if(input$showBounds == "Yes") {
      base <- base + geom_vline(xintercept = mean(keep$x) - 1 * sd(keep$x), color = "red", linetype = 2, alpha = .75)
      base <- base + geom_vline(xintercept = mean(keep$x) + 1 * sd(keep$x), color = "red", linetype = 2, alpha = .75)
      base <- base + geom_hline(yintercept = mean(keep$y) + 1 * sd(keep$y), color = "red", linetype = 2, alpha = .75)
      base <- base + geom_hline(yintercept = mean(keep$y) - 1 * sd(keep$y), color = "red", linetype = 2, alpha = .75)
    }
    base
  })
  
  # Plot of the x-marginal
  output$xMarginal <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    data <- keepExclude()
    keep    <- data$keep
    exclude <- data$exclude
    
    base <- ggplot(keep, aes(x)) + geom_histogram() +
      scale_x_continuous(limits = c(plotlimits()$xmin, plotlimits()$xmax)) +
      scale_y_continuous(limits = c(plotlimits()$xmincount, plotlimits()$xmaxcount)) + 
      labs(x = "X", y = "Counts")
    if(input$showMeans == "Yes") {
      base <- base + geom_vline(xintercept = mean(keep$x))
    }
    if(input$showBounds == "Yes") {
      base <- base + geom_vline(xintercept = mean(keep$x) - 1 * sd(keep$x), color = "red", linetype = 2, alpha = .5)
      base <- base + geom_vline(xintercept = mean(keep$x) + 1 * sd(keep$x), color = "red", linetype = 2, alpha = .5)
    }
    base
  })
  
  # Plot of the y-marginal
  output$yMarginal <- renderPlot({
    # Plot the kept and excluded points as two separate data sets
    data <- keepExclude()
    keep    <- data$keep
    exclude <- data$exclude
    
    base <- ggplot(keep, aes(y)) + geom_histogram() +  coord_flip() +
      scale_x_continuous(limits = c(plotlimits()$ymin, plotlimits()$ymax)) +
      scale_y_continuous(limits = c(plotlimits()$ymincount, plotlimits()$ymaxcount)) +
      labs(x = "Y", y = "Counts")
    if(input$showMeans == "Yes") {
      base <- base + geom_vline(xintercept = mean(keep$y))
    }
    if(input$showBounds == "Yes") {
      base <- base + geom_vline(xintercept = mean(keep$y) + 1 * sd(keep$y), color = "red", linetype = 2, alpha = .5)
      base <- base + geom_vline(xintercept = mean(keep$y) - 1 * sd(keep$y), color = "red", linetype = 2, alpha = .5)
    }
    base
  })
  
  # Toggle points that are clicked
  observeEvent(input$plot1_click, {
    res <- nearPoints(getData$data, input$plot1_click, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  
  # Reset all points
  observeEvent(input$exclude_reset, {
    vals()$keeprows <- rep(TRUE, nrow(getData$data))
  })
  
}

# Load the app
shinyApp(ui, server)