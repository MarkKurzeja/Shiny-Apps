################################################################################
#                                                                              #
# Purpose:       Binomial-Normal Approximation                                 #
#                                                                              #
# Author:        Mark Kurzeja                                                  #
# Contact:       mtkurzej                                                      #
# Client:        Mark Kurzeja                                                  #
#                                                                              #
# Code created:  2018-02-27                                                    #
# Last updated:  2018-02-27                                                    #
#                                                                              #
# Comment:       This app aims to show the relationship between the binomial   #
#                distribution and its normal approximation                     #
#                                                                              #
################################################################################

library(shiny)
library(plyr)
library(magrittr)
library(ggplot2)


# Define server logic
shinyServer(function(input, output) {
  
  ################################################################################
  #                                                                              #
  #                           Render the instructions                            #
  #                                                                              #
  ################################################################################
  output$instructions <- renderText("Approximations make our life easy, but sometimes they aren't this best. This app shows how well the normal distribution (red curve) approximates the binomial distribution (black vertical lines) under certain conditions. Play around with the sliders to see it in action!")
  
  ################################################################################
  #                                                                              #
  #                          Render the commentary text                          #
  #                                                                              #
  ################################################################################
  output$prompt <- renderUI({
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
  
  
  ################################################################################
  #                                                                              #
  #               Render the Binomial-Normal Approximation Graphic               #
  #                                                                              #
  ################################################################################
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
  
})


