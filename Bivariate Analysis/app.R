library(shiny)
library(ggplot2)
library(magrittr)
library(dplyr)

# Define UI for data upload app ----
ui <- fluidPage(
  titlePanel("Bivariate Distributions"),
  wellPanel(
    fluidRow(
      column(
        3,
        fileInput(
          "file1",
          "Choose CSV File",
          multiple = TRUE,
          accept = c("text/csv",
                     "text/comma-separated-values,text/plain",
                     ".csv")
        ),
        actionButton("defaultData", 
                     "Use Default Dataset"
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
      plotOutput("mainPlot", height = 350),
      # click = "plot1_click",
      # brush = brushOpts(
      # id = "plot1_brush"
      # )),
      h4("Marginal Distribution of X"),
      plotOutput("xMarginal", height = 150)
    )
  )
)


# Define server logic to read selected file ----
server <- function(input, output) {
  mdat <- reactiveValues()
  LOADED_DATA <- FALSE
  observe({
    req(input$file1)
    mdat$data <- read.csv(input$file1$datapath,
                          header = T,
                          sep = ",")
    mdat$x <- mdat$data[, 1]
    mdat$y <- mdat$data[, 2]
    mdat$keep    <- data.frame(x = mdat$x, y = mdat$y)
    mdat$exclude <- data.frame(x = mdat$x, y = mdat$y)
    LOADED_DATA <- TRUE
  })
  
  observeEvent(input$defaultData, {
    mdat$data <- mtcars
    mdat$x <- mdat$data[, 1]
    mdat$y <- mdat$data[, 2]
    mdat$keep    <- data.frame(x = mdat$x, y = mdat$y)
    mdat$exclude <- data.frame(x = mdat$x, y = mdat$y)
    LOADED_DATA <- TRUE
  })
  
  
  
  # Dynamic Interactive prompt
  output$prompt <- renderText({
    req(input$file1)
    
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
        "The mean of X is %.2f and the mean of Y is %.2f. The mean is NOT robust to outliers, and it is pulled towards these values more significantly than the median is.",
        mean(mdat$keep$x),
        mean(mdat$keep$y)
      )
    }
    if (input$showBounds == "Yes") {
      boundseff = sprintf(
        "The standard deviation of X is %.2f and the standard deviation of Y is %.2f. The standard deviation is NOT robust to outliers, and it widens when we include them in our analysis and narrows when we take them out.",
        sd(mdat$keep$x),
        sd(mdat$keep$y)
      )
    }
    
    sprintf("%s %s %s %s", histeff, lineeff, meaneff, boundseff)
  })
  
  # Express limits of the plotting range as finite or dynamic values
  plotlimits <- reactive({
    result <- list()
    
    result$xmin = pretty(mdat$x, n = 5) %>% min %>% {. * 0.95}
    result$xmax = pretty(mdat$x, n = 5) %>% max %>% {. * 1.05}
    result$ymin = pretty(mdat$y, n = 5) %>% min %>% {. * 0.95}
    result$ymax = pretty(mdat$y, n = 5) %>% max %>% {. * 1.05}
    
    fauxhist = hist(mdat$x, plot = F, breaks = 30)
    result$xmincount <- 0
    result$xmaxcount <- fauxhist$counts %>% max
    fauxhist = hist(mdat$y, plot = F, breaks = 30)
    result$ymincount <- 0
    result$ymaxcount <- fauxhist$counts %>% max()
    
    result
  })
  
  output$mainPlot <- renderPlot({
    req(input$file1)
    base <- ggplot(mdat$keep, aes(x, y)) + geom_point() +
      geom_point(
        data = mdat$exclude,
        shape = 21,
        fill = NA,
        color = "black",
        alpha = 0.25
      ) +
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
    if (input$showLine == "Yes") {
      base <-
        base + geom_smooth(method = lm,
                           fullrange = TRUE,
                           color = "black")
    }
    if (input$showMeans == "Yes") {
      base <-
        base + geom_hline(yintercept = mean(mdat$keep$y), alpha = .5)
      base <-
        base + geom_vline(xintercept = mean(mdat$keep$x), alpha = .5)
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
  
  # Plot of the x-marginal
  output$xMarginal <- renderPlot({
    req(input$file1)
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
  
  # Plot of the y-marginal
  output$yMarginal <- renderPlot({
    req(input$file1)
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

# Create Shiny app ----
shinyApp(ui, server)