
#==============================================================================
#   Data Analysis Tutorial: Sample selection models
#==============================================================================

# Original Bill Sundstrom 5/3/2015
# edits Michael Kevane 12/17/2016 minor

# Description: Estimation of models with sample selection: Heckman and ML

#==============================================================================
#   1. Settings, packages, and options (run these every R session)
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR econ 42 folder)
setwd("/Users/yournamehere/files42/data")

# Load the packages (must have been installed: see tutorial_2)
# install.packages(c("plm", "ivpack","sampleSelection"))
library(AER)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(ggplot2)
library(gdata)
library(doBy)
library(dplyr)
library(plm)
library(ivpack)
library(sampleSelection)

# turn off scientific notation except for big numbers
options(scipen = 9)

### functions for correct SEs in regression tables

# function to calculate corrected SEs for OLS regression 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

# clustered SEs, clustered on "group"... can also cluster on "time" 
# adjust = T is a df adjustment N/(N-k)
clse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, method="arellano", type = "HC1", 
                         cluster = "group", adjust = T)))
  return(rob)
}

# corrected SEs for IV regressions
ivse = function(reg) {
  rob = robust.se(reg)[,2]
  return(rob)
}

#==============================================================================
#   2. Data: Mroz data on married women's wages
#==============================================================================

### Data from Mroz (1987). Using PSID 1975 data. From package sampleSelection.
# Mroz, T. A. (1987) The sensitivity of an empirical model of married women's 
# hours of work to economic and statistical assumptions. Econometrica 55, 765-799.

data( Mroz87 )

# dummy variable for kids
Mroz87$kids = ( Mroz87$kids5 + Mroz87$kids618 > 0 )

#==============================================================================
#   3. Analysis: Sample selection methods
#==============================================================================

# descriptive statistics
stargazer(Mroz87, type="text", median=TRUE,
          digits=2, title="Married women 1975")

### These are based on estimating an earnings equation for married women

  # OLS: log wage regression on LF participants only
  ols1 = lm(log(wage) ~ educ + exper + I( exper^2 ) + city, data=subset(Mroz87, lfp==1))
  
  # Two-step estimation with LFP selection equation
  heck1 = heckit( lfp ~ age + I( age^2 ) + kids + huswage + educ,
                   log(wage) ~ educ + exper + I( exper^2 ) + city, data=Mroz87 )
  
  # ML estimation of selection model
  ml1 = selection( lfp ~ age + I( age^2 ) + kids + huswage + educ,
                      log(wage) ~ educ + exper + I( exper^2 ) + city, data=Mroz87 ) 
  
  # Note: following is the same as above heckit model heck1
  # heck2 = selection( lfp ~ age + I( age^2 ) + kids + huswage + educ,
  #                   wage ~ educ + exper + I( exper^2 ) + city, data=Mroz87, method="2step" ) 

  # stargazer table
  stargazer(ols1, heck1, ml1,    
            se=list(cse(ols1),NULL,NULL), 
            title="Married women's wage regressions", type="text", 
            df=FALSE, digits=4)
  
  # To see the first-stage (selection equation) probit estimates:
  stargazer(heck1, ml1,    
            title="Married women's LFP regressions", type="text", 
            df=FALSE, digits=4, 
            selection.equation=TRUE)
  
  # Or to see both equations together in less attractive tables:
  summary(heck1)
  summary(ml1)
