rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)

setwd("C:/Users/Mark K/Dropbox/Graduate School/10) Shiny Apps/Univariate Visualization Scripts")

# Fxn for plotting univariate distributions
plothist <- function(x,  xlab, facetvar = NA, filename = xlab, facetvarname = NA, bins = 30) {
  if(is.na(facetvar)) {
    base <- ggplot(data.frame(x = x)) 
  } else {
    base <- ggplot(data.frame(x = x, fv = facetvar)) +
      facet_grid(~fv)
  }
  base = base +
    theme_minimal()
  
  for(i in c("hist", "boxplot")) {
    if (i == "hist") {
      new_base = base + 
        geom_histogram(aes(x), bins = bins) +
        labs(x = xlab, y = "Count") 
    } else {
      new_base = base + 
        geom_boxplot(aes(y = x, x = 1)) +
        theme(axis.text.y = element_blank()) +
        coord_flip() +
        labs(y = xlab, x = "") 
    }
    plot(new_base)
    if(is.na(facetvar)) {
      ggsave(filename = sprintf("./rplots/%s - %s.svg", i, filename), plot = new_base, device = "svg", height = 3, width = 5)
    } else {
      ggsave(filename = sprintf("./rplots/%s - %s + %s.svg", i, filename, facetvarname), plot = new_base, device = "svg", height = 3, width = 5)
    }
    
  }
}

# Read in Nhanes
nhanes <- read.csv("./nhanes.csv", header = T)
nhanes$ridstatr %<>% as.factor()
levels(nhanes$ridstatr) <- c("Male", "Female")

# Histograms & Boxplots
plothist(x = nhanes$age, xlab = "Age")
plothist(x = nhanes$BPXSY1, xlab = "Systolic Blood Pressure")
plothist(x = nhanes$bmxbmi, xlab = "BMI")
plothist(x = nhanes$lbdhdd, xlab = "HDL Cholesterol")
plothist(x = nhanes$BPXSY1, xlab = "Systolic Blood Pressure", facetvar = nhanes$ridstatr, facetvarname = "Gender")
plothist(x = nhanes$bmxbmi, xlab = "BMI", facetvar = nhanes$ridstatr, facetvarname = "Gender")
plothist(x = nhanes$lbdhdd, xlab = "HDL Cholesterol", facetvar = nhanes$ridstatr, facetvarname = "Gender")

# Read in the cartwheel data
cwdata <- read.csv("./Cartwheeldata.csv", header = T)
cwdata$Gender %<>% as.factor()
levels(cwdata$Gender) <- c("Female", "Male")

# Histograms & Boxplots
plothist(x = cwdata$Height, xlab = "Height", bins = 10)
plothist(x = cwdata$Height, xlab = "Height", facetvar = cwdata$Gender, facetvarname = "Gender", bins = 10)
completecwdata <- cwdata %>% filter(Complete == "Y")
plothist(x = completecwdata$CWDistance, xlab = "Distance", bins = 10)
plothist(x = completecwdata$CWDistance, xlab = "Distance", facetvar = completecwdata$Gender, facetvarname = "Gender", bins = 10)

# Read in the Salaries data
saldat <- read.csv("./Salaries.csv", header = T, stringsAsFactors = F)
saldat$BasePay %<>% as.numeric()
saldat$Benefits %<>% as.numeric()
saldat$TotalPayBenefits %<>% as.numeric()
plothist(x = saldat$BasePay, xlab = "Base Pay")
plothist(x = saldat$TotalPayBenefits, xlab = "Total Pay and Benefits")
plothist(x = saldat$Benefits, xlab = "Benefits")




