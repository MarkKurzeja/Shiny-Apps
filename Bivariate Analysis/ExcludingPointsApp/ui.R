

library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Excluding Points"),
  
  sidebarLayout(
    actionButton("exclude_toggle", "Toggle points"),
    actionButton("exclude_reset", "Reset"),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput(outputId = "plot1", height = 350,
                 click = "plot1_click",
                 brush = brushOpts( id = "plot1_brush" ))
    )
  )
))





