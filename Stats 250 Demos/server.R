

library(shiny)
library(ggplot2)
library(ggpubr)
library(magrittr)
library(splines)
library(plyr)
library(dplyr)
library(RColorBrewer)

color1 <- brewer.pal(n = 3, name = "Set1")[1]
color2 <- brewer.pal(n = 3, name = "Set1")[2]
blue1 = RColorBrewer::brewer.pal(3, "Blues")[1]
blue2 = RColorBrewer::brewer.pal(3, "Blues")[2]
blue3 = RColorBrewer::brewer.pal(3, "Blues")[3]

totalSamps = 2000

fillDist <- function(f, from, to, baseobj, fillcolor, alpha, n = 200) {
  upper <- ldply(seq(from, to, length = n), function(i) {
    data.frame(x = i, y = f(i))
  })
  result <- rbind(data.frame(x = from, y = 0), 
                  upper,
                  data.frame(x = to, y = 0),
                  data.frame(x = from, y= 0))
  baseobj + geom_polygon(data = result, aes(x = x, y = y), fill = fillcolor, alpha = alpha)
}

shinyServer(function(input, output) {
  ################################################################################
  #                                                                              #
  #                            CLT Visualization App                             #
  #                                                                              #
  ################################################################################
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
  
  
  ################################################################################
  #                                                                              #
  #                            Binomial Approximation                            #
  #                                                                              #
  ################################################################################
  output$instructions <- renderText("Approximations make our life easy, but sometimes they aren't this best. This app shows how well the normal distribution (red curve) approximates the binomial distribution (black vertical lines) under certain conditions. Play around with the sliders to see it in action!")
  
  #                          Render the commentary text                          #
  output$binomPrompt <- renderUI({
    n = as.numeric(input$n)
    p = input$p / 100
    
    successOutput <- withMathJax("Because \\(np \\geq 10\\) and \\(n (1-p)\\geq 10\\), we can see that the approximation of the binomial with the normal distribution is pretty good and we can proceed with our assumptions")
    failoutput <- withMathJax("Because \\(np < 10\\) or \\(n (1-p)< 10\\), we can see that the approximation of the binomial with the normal distribution is starting to break down. The binomial distribution is the 'true' distribution, and the normal approximation fails to recover it")
    
    if ((n * p >= 10) & (n * (1-p) >= 10)) {
      return(successOutput)
    } else {
      return(failoutput)
    }
    
  })
  
  
  #               Render the Binomial-Normal Approximation Graphic               #
  output$binomApprox <- renderPlot({
    
    ######################### Get the inputs from the user #########################
    n = as.numeric(input$n)
    p = input$p / 100
    cont = ifelse(input$discrete == "TRUE", TRUE, FALSE)
    xmin = max(0, n * p - 4 * sqrt(n * p * (1 - p)))
    xmax = min(n, n * p + 4 * sqrt(n * p * (1 - p)))
    
    ############################## Build up the math ###############################
    x = 0:n
    actual <- dbinom(x = x, size = n, prob = p)
    normapprox <- dnorm(x = x, mean = n * p, sd = sqrt(n * p * (1-p)))
    mydnorm <- function(x) {dnorm(x = x, mean = n * p, sd = sqrt(n * p * (1-p)))}
    
    ############################## Build up the plot ###############################
    result <- ggplot(data.frame(x = x, binom = actual, normal = normapprox)) +
      geom_point(aes(x = x, y = binom)) +
      geom_segment(aes(x=x,xend=x,y=0,yend=binom)) +
      # ggtitle(sprintf("Normal Approximation vs Exact Binomial | N = %i | p = %.2f", n, p)) +
      scale_x_continuous(limits = c(xmin, xmax)) +
      labs(y = "Density")
    if (!cont) {
      result = result + stat_function(fun = mydnorm, color = "red", size = 1.5)
    } else {
      result = result + geom_line(aes(x = x, y = normal), color = "red", size = 1.5)
    }
    result
  })
  
  
  ################################################################################
  #                                                                              #
  #                          Type I and Type II Errors                           #
  #                                                                              #
  ################################################################################
  
  computeAlphaBeta <- reactive({
    result <- list()
    
    if(input$nulldist == "Normal") {
      null = function(i) {dnorm(i, 10, 1)}
    } else if (input$nulldist == "Uniform") {
      null = function(i) {dunif(i, 8, 14)}
    }
    if (input$altdist == "Normal") {
      alt = function(i) {dnorm(i, 14, 2)}
    } else if (input$altdist == "Uniform") {
      alt = function(i) {dunif(i, 10, 19)}
    }
    
    plotnull <- T
    plotalt <- T
    plotalpha <- T
    plotbeta <- T
    lower = 5
    upper = 20
    
    decision = input$decision %>% as.numeric()
    
    baseplot <- ggplot(data.frame(x = 0, y = 0)) +
      scale_x_continuous(limits = c(lower, upper), breaks = sort(c(0,5,10,15,20, decision))) + 
      scale_y_continuous(limits = c(0, NA))
    
    if(plotalpha) {
      baseplot <- fillDist(null, decision, upper, baseplot, color1, 1)
    }
    if(plotbeta) {
      baseplot <- fillDist(alt, lower, decision, baseplot, color2, 1)
    }
    
    if(plotnull) {
      baseplot <- baseplot + stat_function(fun = null, n = 1000)
    }
    if(plotalt) {
      baseplot <- baseplot + stat_function(fun = alt, n = 1000)
    }
    
    result$plot <- baseplot + geom_vline(xintercept = decision) 
    
    result$alpha = integrate(f = null, lower = input$decision, upper = upper)
    result$beta = integrate(f = alt, lower = lower, upper = input$decision)
    result
  })
  # })
  
  genText <- reactive({
    if(input$nulldist == "Normal") {
      amydist = "normally with mean 10 and standard deviation 1"
    } else if (input$nulldist == "Uniform") {
      amydist = "uniformly from 8 to 14"
    }
    if (input$altdist == "Normal") {
      braddist = "normally with mean 14 and standard deviation 2"
    } else if (input$altdist == "Uniform") {
      braddist = "uniformly from 10 to 19"
    }
    
    sprintf("Amy and Brad are pumpkin farmers. You know that Amy's pumpkin's weights are
            distributed %s and Brad's pumpkins are distributed %s. <br><br>You find a random pumpkin
            on the side of the road and will say that it comes from Amy's pumpkin patch if
            it is less than %i pounds, and conclude it will come from Brad's pumpkin patch if it is
            greater than %i pounds. <br><br>The probability of a Type I Error \\(\\alpha\\) (saying that a pumpkin is Brad's
            when it is actually Amy's) is %.2f%% and the probability of a Type II Error \\(\\beta\\) (saying that
            a pumpkin is Amy's when it is actually Brad's) is %.2f%%<script>MathJax.Hub.Queue([\"Typeset\", MathJax.Hub]);</script>",
            amydist, braddist, input$decision, input$decision, 
            computeAlphaBeta()$alpha$value * 100,computeAlphaBeta()$beta$value * 100)    
  })
  
  output$exampleText <- renderText({
    genText()
  })
  
  output$distPlot <- renderPlot({
    computeAlphaBeta()$plot
  })
  
  ################################################################################
  #                                                                              #
  #                          Conditional Distributions                           #
  #                                                                              #
  ################################################################################
  
  getProb <- reactive({
    if(input$givendirection == "At most" & input$condDirection == "At least") {
      prob = max((input$givenVal - input$condVal) * 1/20, 0)
    } else if(input$givendirection == "At least" & input$condDirection == "At most") {
      prob = max((input$condVal - input$givenVal) * 1/20, 0)
    } else if(input$givendirection == "At least" & input$condDirection == "At least") {
      prob = (min(input$givenVal, input$condVal)) * 1/20
    } else if(input$givendirection == "At most" & input$condDirection == "At most") {
      prob = (min(input$givenVal, input$condVal)) * 1/20
    }
    return(prob)
  })
  
  output$condPrompt <- renderText({
    sprintf("Given that X is %s %i, the probability that X is %s %i is %.2f%%", 
            tolower(input$givendirection), 
            input$givenVal, 
            tolower(input$condDirection), 
            input$condVal, 
            getProb()*100)
  })
  
  output$conditionalPlot <- renderPlot({
    
    baseplot <- ggplot(data.frame(x = 0, y = 0)) + 
      scale_x_continuous(limits = c(0, 20)) + 
      scale_y_continuous(limits = c(0, 1/ 20)) + 
      stat_function(fun = dunif, args = list(min = 0, max = 20)) 
    
    if(input$givendirection == "At most" & input$condDirection == "At least") {
      # Shade at-most part
      baseplot <- baseplot + 
        geom_polygon(data = 
                       data.frame(x = c(0,0,input$givenVal, input$givenVal, 0), 
                                  y = c(0, 1/20, 1/20,0,0)), 
                     aes(x,y), alpha = 0.75, fill = blue1)
      if (input$givenVal > input$condVal) {
        baseplot <- baseplot + 
          geom_polygon(data = 
                         data.frame(x = c(input$condVal, input$condVal, input$givenVal, input$givenVal, input$condVal), 
                                    y = c(0, 1/20, 1/20, 0,0)), 
                       aes(x,y), alpha = 0.5, fill = blue2)
      }
    } else if(input$givendirection == "At least" & input$condDirection == "At most") {
      # Shade at-least part
      baseplot <- baseplot + 
        geom_polygon(data = 
                       data.frame(x = c(input$givenVal, input$givenVal, 20, 20, 0), 
                                  y = c(0, 1/20, 1/20,0,0)), 
                     aes(x,y), alpha = 0.75, fill = blue1)
      if (input$givenVal < input$condVal) {
        baseplot <- baseplot + 
          geom_polygon(data = 
                         data.frame(x = c(input$givenVal, input$givenVal, input$condVal, input$condVal, input$givenVal), 
                                    y = c(0, 1/20, 1/20, 0,0)), 
                       aes(x,y), alpha = 0.5, fill = blue2)
      }
    } else if(input$givendirection == "At least" & input$condDirection == "At least") {
      # Shade at-least part
      baseplot <- baseplot + 
        geom_polygon(data = 
                       data.frame(x = c(max(input$condVal, input$givenVal), max(input$condVal, input$givenVal), 20,20, max(input$condVal, input$givenVal)), 
                                  y = c(0, 1/20, 1/20, 0,0)), 
                     aes(x,y), alpha = 0.5, fill = blue2)
    } else if(input$givendirection == "At most" & input$condDirection == "At most") {
      # Shade at-most part
      baseplot <- baseplot + 
        geom_polygon(data = 
                       data.frame(x = c(0,0, min(input$condVal, input$givenVal), min(input$condVal, input$givenVal), 0), 
                                  y = c(0, 1/20, 1/20, 0,0)), 
                     aes(x,y), alpha = 0.5, fill = blue2)
    }
    
    # Finish the plot
    baseplot + 
      geom_path(data = data.frame(x = c(0,0, 20,20,0), y = c(0,1/20,1/20,0,0)), aes(x,y)) +
      labs(y = "Density")
  })
  
  
  
  
  
})
