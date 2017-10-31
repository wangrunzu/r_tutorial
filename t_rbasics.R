#==============================================================================
#   Data Analysis Tutorial: Script basics, import data
#==============================================================================

# original Bill Sundstrom 9/1/2014
# edits by Michael Kevane 12/28/2014
# edits Bill Sundstrom 8/14/2015
# edits by Michael Kevane 8/29/2015
# edits by Bill Sundstrom 9/1/2016
# Latest version: Michael Kevane 12/15/2016

# Description: Script format, set options and import 
# CA school district data and WDI data

#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# You should generally run all of the commands in this Section 1 
# at the start of every R session, with one exception noted below.

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR econ 42 folder)
# For windows - setwd("C:/Users/mkevane/courses/econ42/data")
# For Mac - setwd("/Users/mkevane/courses/econ42/data")

# Load the packages 
# (they must have been installed with t_install_packages.R previously)
# Library commands have to be run every new session
library(countrycode)
library(doBy)
library(dplyr)
library(foreign)
library(gdata)
library(ggplot2)
library(knitr)
library(lmtest)
library(readstata13)
library(reshape)
library(sandwich)
library(stargazer)
library(WDI)
library(XML)
# These two may be problematic for some Mac users. Seek help!
library(AER)
library(car)

# turn off scientific notation except for big numbers
options(scipen = 9)
# function to calculate corrected SEs for regression 
cse <- function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}


#==============================================================================
#   2. Data section
#==============================================================================

# Data entry "by hand": Not the usual way!
# Data can be entered directly in your script
# Simple example: each variable is created as a vector
age <- c(25, 30, 56)
gender <- c("male", "female", "male")
weight <- c(160, 110, 220) 
# Assemble the variables into a data set
mydata <- data.frame(age,gender,weight)
# take a look at the data
mydata

# Simulate data
set.seed(1534) # For replication 
N = 1000 # Population size
X0 = runif(N) # Value of intercept value  
X1 = runif(N) # Value of explanatory value 
Xerr = runif(N) # Value of error term value 
Y = X0 + 4*X1 +Xerr # Value of outcome Y 
samp = data.frame(X0,X1,Y) # data that might "observe"

# Read data from csv file
caschool = read.csv("caschool.csv",header=TRUE,sep=",")

# Read data from a website
caschool <- read.dta("http://wps.aw.com/wps/media/objects/11422/11696965/datasets3e/datasets/caschool.dta")

# new variable for "small" average class sizes: student teacher ratio < 20
# Note "address" for a variable is the data frame name, then the dollar sign $, then the variable name
# Note this variable is a "factor" variable, not a numeric variable
caschool$smallclass <- caschool$str<20

# new variable using mathematical expression (similar to excel)
caschool$strsquared <- caschool$str^2

### Read data from a database on the Internet

wdilist <- c("NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2005 intl $)
             "SP.POP.GROW", # Population growth (annual %)
             "SP.POP.TOTL", # Population, total
             "SP.POP.TOTL.FE.ZS", # Population, female (% of total)
             "SP.URB.TOTL.IN.ZS", # Urban population (% of total)
             "SP.POP.BRTH.MF", # Sex ratio at birth   # (females per 1000 males)
             "SP.DYN.LE00.IN", # Life expect at birth, total  # (years)
             "SP.DYN.LE00.FE.IN", # Life expect, female (years)
             "SP.DYN.LE00.MA.IN", # Life expect, male (years),
             "SP.DYN.IMRT.IN", # Infant mortality rate
             "SP.DYN.TFRT.IN" ) # Fertility rate,(births per woman) 

# Extract latest version of desired 
# variables from WDI.
# This may take a few minutes, 
# depending on connection speed

wdim <- WDI(country="all", indicator = wdilist, 
           extra = TRUE, start = 2015, end = 2015)

# Rename the variables
wdim <- rename.vars(wdim,c("NY.GDP.PCAP.PP.KD", "SP.POP.TOTL"), c("GDPpcUSDreal","population"))
wdim <- rename.vars(wdim, c("SP.POP.TOTL.FE.ZS", "SP.URB.TOTL.IN.ZS"),c("femaleperc","urbanperc"))
wdim <- rename.vars(wdim, c("SP.POP.BRTH.MF", "SP.DYN.LE00.IN"), c("sexratiobirth","lifeexp"))
wdim <- rename.vars(wdim, c("SP.POP.GROW"),  c("popgrow"))
wdim <- rename.vars(wdim, c("SP.DYN.LE00.FE.IN", "SP.DYN.LE00.MA.IN"), c("lifexpfem","lifeexpmale"))
wdim <- rename.vars(wdim, c("SP.DYN.IMRT.IN", "SP.DYN.TFRT.IN"), c("infmort","fertility"))
levels(wdim$region)

# Take out the entries that are aggregates 
# (eg East Asia) and not countries
wdim <- subset(wdim, !( region=="Aggregates")) 
# See how many observations in each region
table(wdim$region)
# Create a new data set that is just the African 
# countries in wdim
wdi_africa <- subset(wdim, region=="Sub-Saharan Africa (all income levels)")
# Create a new data set that is just two variables in wdi, along with country
wdim_income <- wdim[c("country", "GDPpcUSDreal", "femaleperc")]

#==============================================================================
#   3. Analysis section
#==============================================================================

### See subsequent tutorials...

# But wait, let us at least do a table of descriptive statistics
stargazer(wdim, type="text", digits=2, title="WDI dataset")
stargazer(wdi_africa, type="text", digits=2, title="WDI dataset")

# Look at basic stats for all the numeric variable sin the dataset
stargazer(caschool, type="text", digits=2, title="CA schools dataset")


