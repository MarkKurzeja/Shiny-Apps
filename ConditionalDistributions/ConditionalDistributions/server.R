#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(RColorBrewer)
color1 = RColorBrewer::brewer.pal(3, "Blues")[1]
color2 = RColorBrewer::brewer.pal(3, "Blues")[2]
color3 = RColorBrewer::brewer.pal(3, "Blues")[3]


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
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
  
  output$prompt <- renderText({
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
                     aes(x,y), alpha = 0.75, fill = color1)
      if (input$givenVal > input$condVal) {
        baseplot <- baseplot + 
          geom_polygon(data = 
                         data.frame(x = c(input$condVal, input$condVal, input$givenVal, input$givenVal, input$condVal), 
                                    y = c(0, 1/20, 1/20, 0,0)), 
                       aes(x,y), alpha = 0.5, fill = color2)
      }
    } else if(input$givendirection == "At least" & input$condDirection == "At most") {
      # Shade at-least part
      baseplot <- baseplot + 
        geom_polygon(data = 
                       data.frame(x = c(input$givenVal, input$givenVal, 20, 20, 0), 
                                  y = c(0, 1/20, 1/20,0,0)), 
                     aes(x,y), alpha = 0.75, fill = color1)
      if (input$givenVal < input$condVal) {
        baseplot <- baseplot + 
          geom_polygon(data = 
                         data.frame(x = c(input$givenVal, input$givenVal, input$condVal, input$condVal, input$givenVal), 
                                    y = c(0, 1/20, 1/20, 0,0)), 
                       aes(x,y), alpha = 0.5, fill = color2)
      }
    } else if(input$givendirection == "At least" & input$condDirection == "At least") {
      # Shade at-least part
      baseplot <- baseplot + 
        geom_polygon(data = 
                       data.frame(x = c(max(input$condVal, input$givenVal), max(input$condVal, input$givenVal), 20,20, max(input$condVal, input$givenVal)), 
                                  y = c(0, 1/20, 1/20, 0,0)), 
                     aes(x,y), alpha = 0.5, fill = color2)
    } else if(input$givendirection == "At most" & input$condDirection == "At most") {
      # Shade at-most part
      baseplot <- baseplot + 
        geom_polygon(data = 
                       data.frame(x = c(0,0, min(input$condVal, input$givenVal), min(input$condVal, input$givenVal), 0), 
                                  y = c(0, 1/20, 1/20, 0,0)), 
                     aes(x,y), alpha = 0.5, fill = color2)
    }
    
    # Finish the plot
    baseplot + 
      geom_path(data = data.frame(x = c(0,0, 20,20,0), y = c(0,1/20,1/20,0,0)), aes(x,y)) +
      labs(y = "Density")
  })
  
})
