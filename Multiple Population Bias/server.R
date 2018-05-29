################################################################################
#                                                                              #
# Purpose:       Two Population Bias Simulator                                 #
#                                                                              #
# Author:        Mark Kurzeja                                                  #
# Contact:       mtkurzej@umich.edu                                            #
# Client:        Mark Kurzeja                                                  #
#                                                                              #
# Code created:  2018-02-26                                                    #
# Last updated:  2018-02-26                                                    #
# Source:        C:/Users/Mark/Documents                                       #
#                                                                              #
# Comment:       This shiny app aims to illustrate visually, motivated by sam  #
#                pling body weights at the gym and across UofM, the dangers    #
#                of sampling from a subgroup that is not representative of     #
#                the population that you wish to infer about                   #
#                                                                              #
################################################################################

library(shiny)
library(ggplot2)
library(ggpubr)

################################################################################
#                                                                              #
#          Define Server Logic for Drawing from multiple populations           #
#                                                                              #
################################################################################
shinyServer(function(input, output) {
  
  ################################################################################
  #                                                                              #
  # Driver-function - on the drawSamples button press, generate the data from    #
  # two gaussian curves and sample to create a biased/unbiased mean depending    #
  # on the choice of the radio buttons                                           #
  #                                                                              #
  ################################################################################
  genSamples <- eventReactive(input$action_drawSamples, {
    
    ############################# Set the Base Params ##############################
    mean_uofm <- 155
    sd_uofm <- 5
    mean_gym <- 185 
    sd_gym <- 5 
    numberSamps = input$slider_numberOfSamples
    sampSize = input$slider_sampleSize
    gymperc <- input$slider_gymPercentage
    totalPopSize = 40000
    
    ######### Draw from both parent populations to get our UofM Population #########
    pop = c(rnorm(totalPopSize * (100 - gymperc), mean_uofm, sd_uofm), 
            rnorm(totalPopSize * gymperc, mean_gym, sd_gym))
    
    ################## Define the color and style of the samples ###################
    # browser()
    if(input$radio_populationSelection == "All of UofM"){
      meandist = matrix(sample(x = pop, 
                        size = numberSamps * sampSize, 
                        replace = F), 
                 ncol = sampSize) %>% 
        rowMeans() %>% as.numeric()
      color_in = "blue"
    } else {
      meandist = matrix(rnorm(numberSamps * sampSize, 
                       mean = mean_gym, sd = sd_gym), ncol = sampSize) %>% 
        rowMeans() %>% as.numeric()
      color_in = "red"
    }
    ######################### Plot the Parent Populations ##########################
    p1 <- ggplot(data.frame(x = sample(pop, 2000), y = 0)) +
      stat_function(aes(color = "All of UofM"), fun = function(i) {
        (100-gymperc)/100 * dnorm(x = i, mean = mean_uofm, sd = sd_uofm) + 
          gymperc/100 * dnorm(x = i, mean = mean_gym, sd = sd_gym)
      }, n = 400) +
      # geom_density(aes(x = x, color = "All of UofM")) +
      stat_function(aes(color = "Non-Gym Goers"), linetype = 2,  fun = function(i) {dnorm(x = i, mean = mean_uofm, sd = sd_uofm)}) +
      stat_function(aes(color = "Gym Goers"), linetype = 2, fun = function(x) {dnorm(x = x, mean = mean_gym, sd = sd_gym)}, n = 400) +
      scale_x_continuous(limits = c(140,200)) +
      scale_color_brewer(palette = "Set1") +
      guides(color = guide_legend(title = "Population")) +
      labs(x = "Weight", y = "Density") + 
      geom_vline(xintercept = mean(pop), color = "blue") 
    
    ############################### Plot the Samples ###############################
    p2 <- ggplot(data = data.frame(x = meandist, y = 0)) +
      # geom_density(aes(x = x, y = ..density.., color = "Sampling Distribution"), linetype = 1) +
      stat_density(aes(x = x,  color = "Sampling Distribution"), geom = "line", linetype = 1) +
      geom_point(aes(x = x, y = y), color = color_in) + 
      scale_x_continuous(limits = c(140,200)) +
      scale_linetype_manual(values = c("solid")) + 
      geom_vline(xintercept = mean(meandist)) +
      geom_vline(xintercept = mean(pop), color = "blue") + 
      labs(x = "Weight") +
      guides(color = guide_legend(title = "Population")) +
      labs(x = "Weight", y = "Density") 

    ############### Combine the two plots into one object and return ###############
    # ggpubr::ggarrange(p1, p2, ncol = 1, nrow = 2, common.legend = T, align = "v", heights = c(1,.5))
    list(p1 = p1, p2 = p2)
  })
  
  ################################################################################
  #                                                                              #
  #                       Functions for Rendering the Text                       #
  #                                                                              #
  ################################################################################
  
  output$popText <- renderText({
    "The red curve shows the distribution of a <strong>variable of interest</strong> (weight in this case) 
    for the entire U of M population. The dashed green curve shows the distribution of 
    weight for the non-gym goers, and the dashed blue curve shows the distribution 
    for gym goers. In this case, we see that the two subgroups clearly vary in terms 
    of their weights, and that the smaller the percentage of the population that goes 
    to the gym, the more that the population distribution tends toward lower weight. "
  })
  
  output$sampText <- renderText({
    sprintf("The red curve shows the <strong>sampling distribution</strong> of the estimated means based 
    on the %i samples that you selected. When sampling at 
    random from <strong>all of U of M</strong>, we see that the estimated means from the repeated 
    samples are distributed evenly around the overall mean for the population 
    (in blue), but that there is <strong>sampling variability</strong>, and the mean of the 
    sampling distribution (in black) is close to the true population mean 
    for weight. The standard deviation of these estimates is the <strong>standard 
    error</strong> of a single estimated mean, or a measure of how variable that estimated 
    mean would be if repeated samples were selected. When sampling at random from 
    <strong>Gym Goers</strong> (as might be the case in a non-representative, non-probability 
    convenience sample of people at the gym), we see that the sampling distribution 
    veers away from the true population mean. This is an example of <strong>biased sampling</strong>, 
    where the samples are not representative of the true overall population, and 
    the expected value of the estimated means is far from the true mean. Note 
    that this bias becomes larger if less of the population goes to the gym.",input$slider_numberOfSamples)
  })
  
  output$finalPrompt <- renderText({
    "This application demonstrates the sampling bias in survey estimates of 
    means that can result when a population is defined by different subgroups 
    (e.g., gym-goers and not gym-goers), those subgroups vary in terms of a 
    variable of interest (e.g., weight), and a random sampling strategy does 
    not result in a representative random sample of those subgroups, but 
    rather an over-sample from one subgroup in particular (e.g., gym-goers). 
    This is why knowing what strategy was used to select the random sample 
    is so important for evaluating the quality of survey estimates. Remember 
    that in reality, we only select <strong>one sample</strong>; we just want to ensure that 
    the distribution of estimates based on all samples that we might select 
    using a given sample design is roughly centered at the population mean. 
    This is what it means for an estimated mean based on one sample to be an 
    <strong>unbiased estimate</strong>."
  })
  
  ################################################################################
  #                                                                              #
  #                       Function for rendering the plot                        #
  #                                                                              #
  ################################################################################
  output$plotPopulations <- renderPlot({
    plot(genSamples()$p1)
  })
  output$plotSamples <- renderPlot({
    plot(genSamples()$p2)
  })
})
