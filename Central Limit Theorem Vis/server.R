
library(shiny)
library(ggplot2)
library(ggpubr)
library(magrittr)
library(splines)

totalSamps = 2000

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  samples <- reactive({
    if(input$distr == "Uniform") {
      return(runif(totalSamps, 0, 10))
    } else if (input$distr == "Normal") {
      return(rnorm(totalSamps, 5, 1))
    } else if (input$distr == "Skewed") {
      return(10 * rbeta(totalSamps, 4, 1))
    } else if (input$distr == "Dragon") {
      # pdf propto sin(2 * x) + 2 using inverse splines
      x = seq(0, 10, length = 100)
      fx <- (2 * x + sin(x)^2) / (2 * 10 + sin(10)^2)
      sp <- interpSpline(x, fx)
      bs <- backSpline(sp)
      predict(bs, runif(totalSamps))$y
    }
  })
  
  means <- reactive({
    samples() %>%
      sample(size = 2000 * as.numeric(input$numSamp), replace = T) %>%
      matrix(ncol = as.numeric(input$numSamp)) %>%
      rowMeans() %>%
      as.numeric()
  })
  
  output$CLTPop <- renderPlot({
    s <- samples()
    m <- means()
    baseplot <- ggplot(data.frame(x = s)) + 
      geom_histogram(aes(x), bins = 75) + 
      scale_x_continuous(limits = c(0,10))
    baseplot
  })
  output$CLTMean <- renderPlot({
    s <- samples()
    m <- means()
    meansplot <- ggplot(data.frame(x = m)) +
      geom_histogram(aes(x, y= ..density..), bins = 75) +
      scale_x_continuous(limits = c(0,10)) +
      stat_function(fun = dnorm, 
                    args = list(mean = mean(s), 
                                sd = sd(s) / sqrt(as.numeric(input$numSamp))),
                    n = 300)
    meansplot
  })
})
