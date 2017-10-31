
#==============================================================================
#   Data Analysis Tutorial: Regression basics
#==============================================================================

# original Bill Sundstrom 9/1/2014
# edits by Michael Kevane 12/28/2014
# edits Bill Sundstrom 4/16/2015
# edits by Michael Kevane 8/29/2015
# edits by Bill Sundstrom 9/1/2016
# edits by Michael Kevane 12/15/2016

# Description: Run regressions in R, with heteroskedasticity-robust
# standard errors, and present the results in a table

#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# You should generally run all of the commands in Section 1 
# at the start of every R session
  
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

#==============================================================================
#   3. Analysis section
#==============================================================================

# Let's plot test scores against student-teacher ratio again
  ggplot(data=caschool, aes(x=str, y=testscr)) +
    geom_point(shape=0)+
    labs(title="CA test scores") + 
    labs(x="Student-teacher ratio", y="Test score")

### Run the regression

  # we give the regression results a name, in this case reg1 
  reg1 <- lm(testscr ~ str, data=caschool)
  
  # summary of results... we generally will skip this and go straight to stargazer table!
  summary(reg1)    
  
  # Report regression in a nice table, 
  # with corrected standard errors, using stargazer
  stargazer(reg1, 
            se=list(cse(reg1)), 
            title="CA school district regression", type="text", 
            df=FALSE, digits=3)
  
  # Obtain and plot the regression residuals
  caschool$resid1 <- resid(reg1)
    ggplot(data=caschool, aes(x=str, y=resid1)) +
    geom_point(shape=0)+
    labs(title="Residual against ST") + 
    labs(x="Student-teacher ratio", y="Regression residual")
  
  # Run another regression and include both regs in the same table
  reg2 <- lm(testscr ~ el_pct, data=caschool)
  
  stargazer(reg1, reg2, 
            se=list(cse(reg1),cse(reg2)), 
            title="CA school district regressions", type="text", 
            df=FALSE, digits=3)
  
  # Run another regression and include all three regs in the same table
  reg3 <- lm(testscr ~ meal_pct, data=caschool)
  
  stargazer(reg1, reg2, reg3, 
            se=list(cse(reg1),cse(reg2),cse(reg3)), 
            title="CA school district regressions", type="text", 
            df=FALSE, digits=3)
  
  # Run a regression using the WDI data
  regwdi1 <- lm(femaleperc ~ GDPpcUSDreal, data= wdim)
  regwdi2 <- lm(femaleperc ~ lifexpfem, data= wdim)
 
  stargazer(regwdi1, regwdi2,
            se=list(cse(regwdi1), cse(regwdi2)), 
            title="Regressions explaining percent female", 
            type="text", 
            df=FALSE, digits=5)
  
