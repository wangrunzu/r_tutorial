
#==============================================================================
#   Data Analysis Tutorial: Multiple regression
#==============================================================================

# original Bill Sundstrom 9/1/2014
# edits by Michael Kevane 12/28/2014
# edits Bill Sundstrom 4/16/2015
# edits by Michael Kevane 8/29/2015
# edits by Bill Sundstrom 9/1/2016

# Description: Run multiple regressions in R, replicate Table 7.1
# from Stock and Watson textbook, predictions and tests
# Use WDI data to run regressions

#==============================================================================
#   1. Settings, packages, and options (run these every R session)
#==============================================================================

  # Clear the working space
  rm(list = ls())
  
  # Set working directory (edit for YOUR econ 42 folder)
  setwd("/Users/mkevane/econ_42/files42/data")
  
  # Load the packages 
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
  # For this tutorial to use lht the package car is necessary
  library(car)
  
  # turn off scientific notation except for big numbers
  options(scipen = 9)
  # function to calculate corrected SEs for regression 
  cse = function(reg) {
    rob = sqrt(diag(vcovHC(reg, type = "HC1")))
    return(rob)
  }

#==============================================================================
#   2. Data section
#==============================================================================

### Read data 
  
  # Data input using read.dta (from Stock & Watson textbook site)
  caschool <- read.dta("http://wps.aw.com/wps/media/objects/11422/11696965/datasets3e/datasets/caschool.dta")
  
  # new variable for "small" average class sizes: student teacher ratio < 20
  # Note "address" for a variable is the data frame name, then the dollar sign $, then the variable name
  # Note this variable is a "factor" variable, not a numeric variable
  caschool$smallclass <- caschool$str<20
  
### Read WDI data from a database on the Internet

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
               "SP.DYN.TFRT.IN" )# Fertility rate,(births per woman) 
  
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
  
  # Take out the entries that are aggregates 
  # (eg East Asia) and not countries
  wdim <- subset(wdim, !( region=="Aggregates")) 
  
  # Transform some of the variables
  wdim$GDPpcUSDreal <- wdim$GDPpcUSDreal/1000
  wdim$population <- wdim$population/1000000
  wdim$latitude <- as.numeric(wdim$latitude)

#==============================================================================
#   3. Analysis section
#==============================================================================

### Run the five regressions for Table 7.1

  # The first we have already looked at (Tutorial #3) 
  reg1 <- lm(testscr ~ str, data=caschool)
  
  # Here is the regression for column (2) in the table
  # for multiple regression, just add more regressors with a "+" sign
  reg2 <- lm(testscr ~ str + el_pct, data=caschool)
  
  # The remaining regressions:
  reg3 <- lm(testscr ~ str + el_pct + meal_pct, data=caschool)
  reg4 <- lm(testscr ~ str + el_pct + calw_pct, data=caschool)
  reg5 <- lm(testscr ~ str + el_pct + meal_pct + calw_pct, data=caschool)
  
  ### Make the table with all regressions as separate columns
  # Remember to include all regressions in the se=list() 
  stargazer(reg1, reg2, reg3, reg4, reg5, 
            se=list(cse(reg1),cse(reg2),cse(reg3),cse(reg4),cse(reg5)), 
            title="Regression Results", type="text", 
            df=FALSE, digits=3)
  
  # Add column labels to help the reader
  stargazer(reg1, reg2, reg3, reg4, reg5, 
            se=list(cse(reg1),cse(reg2),cse(reg3),cse(reg4),cse(reg5)), 
            title="Regression Results", type="text", 
            column.labels=c("Simple", "ELL", "Lunch", "CalWorks", "Full"),
            df=FALSE, digits=3)

### Prediction using the predict function

  # We can use the regression coefficients to predict the test score for specific values of Xs
  # First create newdata with desired values of Xs
  newdata <- data.frame(str = 20, el_pct = 20, meal_pct = 50)
  # Then predict for this district using regression reg3
  predict(reg3, newdata)
  
  # We can put confidence intervals around our predictions
  predict(reg3, newdata, interval="confidence")  # 95% CI for E(Y | X)
  predict(reg3, newdata, interval="prediction")  # 95% CI for individual Y given X

### Calculate standardized coefficients

  # retrieve the coefficient and multiply by sd(X)/sd(Y)
  coef(summary(reg5))["str","Estimate"]*sd(caschool$str)/sd(caschool$testscr)
  coef(summary(reg5))["el_pct","Estimate"]*sd(caschool$el_pct)/sd(caschool$testscr)
  
  # would need na.rm=TRUE option in sd if any variable has missing values... not a problem here
  coef(summary(reg5))["str","Estimate"]*sd(caschool$str, na.rm=TRUE)/sd(caschool$testscr, na.rm=TRUE)

### F-tests for various hypotheses using the function lht

  # H0: coefficients on meal_pct and calw_pct are BOTH = 0
  # note: white.adjust means heteroskedasticity-corrected
  lht(reg5, c("meal_pct = 0", "calw_pct = 0"), white.adjust = "hc1") 
  
  # H0: coefficients on el_pct and calw_pct are equal to each other
  lht(reg5, c("el_pct = calw_pct"), white.adjust = "hc1")
  
  # H0: slope on str is = -1
  lht(reg5, c("str = -1.0"), white.adjust = "hc1")  
  
  # H0: all the slope coefficients are zero
  lht(reg5, c("str = 0", "el_pct = 0", "meal_pct = 0", "calw_pct = 0"), white.adjust = "hc1")

### Multiple regression with WDI data

  regwdi1 <- lm(femaleperc ~ GDPpcUSDreal+population+ latitude, data= wdim)
  
  regwdi2 <- lm(femaleperc ~ GDPpcUSDreal+population+ infmort
            + fertility+latitude, data= wdim)
  stargazer(regwdi1,regwdi2, 
            se=list(cse(regwdi1),cse(regwdi2)), 
            title="Regression of percent female on various 
            correlates", type="text", df=FALSE, digits=3)
  
  lht(regwdi2, c("population = 0", "infmort = 0"), white.adjust = "hc1") 

