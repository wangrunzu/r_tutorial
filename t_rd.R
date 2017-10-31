
#==============================================================================
#   Regression discontinuity using mortality rates and MLDA (see MM ch. 4)
#==============================================================================

  # Regression discontinuity study of effect of minimum legal drinking age on 
  # mortality. See Angrist and Pischke, Mastering 'Metrics, chapter 4

  # Thanks to Carlos Dobkin for data and original code
  # Latest version: Bill Sundstrom 6/27/2016

#==============================================================================
#   Settings, packages, and options
#==============================================================================

  # Clear the working space
  
  rm(list = ls())
  
  # Set working directory 
  setwd("C:/Users/MKevane/Google Drive/dataprog R/files42/data/")
  
  ### Load packages (all must have been installed)
  library(AER)
  library(sandwich)
  library(lmtest)
  library(car)
  library(stargazer)
  library(ggplot2)
  library(dplyr)

  ### More settings
  
  # turn off scientific notation except for big numbers
  options(scipen = 9)
  
  ### functions for correct SEs in regression tables
  
  # function to calculate corrected SEs for OLS regression 
  cse = function(reg) {
    rob = sqrt(diag(vcovHC(reg, type = "HC1")))
    return(rob)
  }
  

#==============================================================================
#   Data and analysis
#==============================================================================

### Read the data: mortality rates by month of age 19-22
  
  mort <- read.csv("mortality_data.csv", header=TRUE)

### Create new variables
  
  # Note that age is measured in fractions of a year, using 30-day increments
  mort$D <- ifelse(mort$Age>=21,1,0)     # treatment dummy
  mort$agec = mort$Age - 21              # running variable relative to threshold

### Run regressions and obtain predicted values of mortality (All) 
  # We run various specifications. Let's start with the simplest,
  # which is named "parallel" because the slope is the same above and below the threshold.
  
  # slope same before and after age threshold:
  parallel <- lm(All ~  D + agec, data=mort)
  
  # table of results  
  stargazer(parallel, 
            se=list(cse(parallel)),           
            title="RD regression", type="text", 
            column.labels=c("parallel"), 
            df=FALSE, digits=2)
  
  # Plot of the data with the RD regression line
  # first predict the mortality rate from the regression
  mort$pred_par <- predict(parallel)
  
  # Now the plot. Note that we add the before and after 
  # prediction lines separately to show the discontinuity
  ggplot(mort, aes(x=Age, y=All)) + 
    geom_point(shape=1) + 
    labs(y = "Mortality rate, all causes", x = "Age", 
         title = "Linear model") + 
    ylim(85,110) + 
    geom_line(data=subset(mort,Age<21), aes(x=Age, y=pred_par)) + 
    geom_line(data=subset(mort,Age>=21), aes(x=Age, y=pred_par))  
  
### Additional specifications 
  
  # linear, allowing slope to change... use * for interaction plus main effects:
  linear <- lm(All ~  D*agec, data=mort)
  # quadratic and cubic:
  quad <- lm(All ~ D*agec + D*I(agec^2), data=mort)
  cubic <- lm(All ~ D*agec + D*I(agec^2) + D*I(agec^3), data=mort)
  # predictions for plotting
  mort$pred_lin <- predict(linear)
  mort$pred_quad <- predict(quad)
  mort$pred_cub <- predict(cubic)

  # table of results  
  stargazer(parallel, linear, quad, cubic, 
            se=list(cse(parallel), cse(linear), cse(quad), cse(cubic)),           
            title="RD regressions", type="text", 
            column.labels=c("parallel", "linear", "quadratic", "cubic"), 
            df=FALSE, digits=2)
  
  # Based on the table, which specification do you prefer and why?
  
  # Plot of the cubic specification
  
  ggplot(mort, aes(x=Age, y=All)) + 
    geom_point(shape=1) + 
    labs(y = "Mortality rate, all causes", x = "Age", 
         title = "Cubic model") + 
    ylim(85,110) + 
    geom_line(data=subset(mort,Age<21), aes(x=Age, y=pred_cub)) + 
    geom_line(data=subset(mort,Age>=21), aes(x=Age, y=pred_cub))  
  