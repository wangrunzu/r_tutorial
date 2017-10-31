
#==============================================================================
#   Data Analysis Tutorial: Nonlinear regression specifications
#==============================================================================

# original Bill Sundstrom 9/1/2014
# edits by Michael Kevane 12/28/2014
# edits Bill Sundstrom 4/16/2015
# edits by Bill Sundstrom 9/1/2016
# edits by Michael Kevane 12/15/2016

# Description: Nonlinear regressions, replication of Table 8.1, and
# diagnosing and treating perfect multicollinearity

#==============================================================================
#   1. Settings, packages, and options (run these every R session)
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR econ 42 folder)
setwd("/Users/mkevane/files42/data")

# Load the packages 
library(countrycode)
library(doBy)
library(dplyr)
library(foreign)
library(ggplot2)
library(knitr)
library(lmtest)
library(readstata13)
library(reshape)
library(sandwich)
library(stargazer)

# turn off scientific notation except for big numbers
options(scipen = 9)
# function to calculate corrected SEs for regression 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
  }

#==============================================================
#   Data section for CA school analysis
#==============================================================

### Read data 
  
  # Data input using read.dta (from Stock & Watson textbook site)
  caschool <- read.dta("http://wps.aw.com/wps/media/objects/11422/11696965/datasets3e/datasets/caschool.dta")


#==============================================================
#   Analysis section for CA school analysis
#==============================================================

### plot testscr and average income with a regression line

  ggplot(caschool, aes(x=avginc, y=testscr)) + 
    labs(y = "Test score",x = "Average income" ,title = "CA test scores") +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm, se=F) 
  
### Run nonlinear regressions

### Polynomials
  # Linear
  reg1 <- lm(testscr ~ avginc, data=caschool)
  # Quadratic
  # We can create new variables within the regression formula.
  # Note we create the square of avginc using I(avginc^2)
  # R evaluates the formula inside the I(...)  
  reg2 <- lm(testscr ~ avginc + I(avginc^2), data=caschool)
  # Cubic
  reg3 <- lm(testscr ~ avginc + I(avginc^2) + I(avginc^3), data=caschool)
  
  # table of polynomial specifications
  stargazer(reg1, reg2, reg3,  
            se=list(cse(reg1),cse(reg2),cse(reg3)), 
            title="Regression Results", type="text", 
            df=FALSE, digits=4)
  
### Logs
  # linear-log
  reg4 <- lm(testscr ~ I(log(avginc)), data=caschool)
  # log-linear
  reg5 <- lm(I(log(testscr)) ~ avginc, data=caschool)
  # log-log
  reg6 <- lm(I(log(testscr)) ~ I(log(avginc)), data=caschool)
  
  # Table: Note we can add more informative column labels
  stargazer(reg4, reg5, reg6,  
            se=list(cse(reg4),cse(reg5),cse(reg6)), 
            title="Regression Results", type="text", 
            column.labels=c("lin-log", "log-lin", "log-log"), 
            df=FALSE, digits=3)

#==============================================================
#   Data section for Table 8.1 from the SW textbook
#==============================================================

### Replicate Table 8.1 

### Read the data 

  # read the data from csv file
  # the data are 2008 earnings data from the CPS
  # vars: ahe yrseduc female age northeast midwest south west 
  
  data8.1   <- read.dta("http://wps.aw.com/wps/media/objects/11422/11696965/datasets3e/datasets/ch8_cps.dta")
  
  # table 8.1 in book restricts the sample to people 
  # aged at least 30 (and everyone is under 65)
  t8.1 <- subset(data8.1, age>=30)
  
  # descriptive statistics
  stargazer(t8.1, type="text", median=TRUE,
            digits=2, title="Earnings data")

  ### Create some new variables
  
  # natural log of earnings is the dependent variable
  t8.1$logahe <- log(t8.1$ahe)
  # potential experience is years since finishing school
  t8.1$exper <- t8.1$age - t8.1$yrseduc - 6


#==============================================================
#   Analysis section for Table 8.1 from the SW textbook
#==============================================================

### run the regressions for each column in the table

  # col (1): log earnings as a function of years of education 
  col1 <- lm(logahe ~ yrseduc, data=t8.1)
  # col (2): add a dummy (binary) variable for female
  col2 <- lm(logahe ~ yrseduc + female, data=t8.1)
  # col (3): add an interaction term, the product of yrseduc and female
  # this can be done in various ways in R.
  # One obvious way would be to create the product of the variables
  # in the regression:
  col3alt <- lm(logahe ~ yrseduc + female + I(yrseduc*female), data=t8.1)
  # Here is the way we will usually do it using ":"
  col3 <- lm(logahe ~ yrseduc + female + yrseduc:female, data=t8.1)
  
  # we could also run separate regressions for female and male
  col3f <- lm(logahe ~ yrseduc, data=subset(t8.1, female==1))
  col3m <- lm(logahe ~ yrseduc, data=subset(t8.1, female==0))
  
  # Compare these regressions 
  stargazer(col3, col3alt, col3f, col3m,  
            se=list(cse(col3),cse(col3alt),cse(col3f),cse(col3m)), 
            title="Earnings regressions", type="text", 
            column.labels=c("All", "All", "Females", "Males"), 
            df=FALSE, digits=3)
  # Did all of these give the same results? Explain how the separate male and female
  # regression results are identical to the others.
  
  # col (4): add experience, experience-squared, and regional dummy variables
  # use I() to create experience squared
  col4 <- lm(logahe ~ yrseduc + female + yrseduc:female + 
               exper + I(exper^2) + midwest + south + west, data=t8.1)
  
  # replicate Table 8.1
  stargazer(col1, col2, col3, col4,  
            se=list(cse(col1),cse(col2),cse(col3),cse(col4)), 
            title="Table 8.1", type="text", 
            df=FALSE, digits=3)

### Perfect multicollinearity: diagnosis

  # Run a regression that suffers from the dummy variable trap
  bad_reg <- lm(logahe ~ yrseduc + female + exper + I(exper^2) 
            + northeast + midwest + south + west, data=t8.1)
  stargazer(bad_reg,  
            se=list(cse(bad_reg)), 
            title="Dummy variable trap", type="text", 
            df=FALSE, digits=3)
  # What do you notice about the results?  Which region was dropped? Why?
