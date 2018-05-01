#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(plyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(RColorBrewer)

color1 <- brewer.pal(n = 3, name = "Set1")[1]
color2 <- brewer.pal(n = 3, name = "Set1")[2]


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


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
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
  
})
