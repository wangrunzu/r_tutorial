
#==============================================================================
#   Data Analysis Tutorial: Descriptive statistics
#==============================================================================

# original Bill Sundstrom 9/1/2014
# edits by Michael Kevane 12/28/2014
# edits by Bill Sundstrom 8/14/2015
# edits by Michael Kevane 8/29/2015
# edits by Bill Sundstrom 9/1/2016

# Description: Create tables of descriptive statistics for 
# CA school district data and WDI data etc.

#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# You should generally run all of the commands in Section 1 
# at the start of every R session.

  # Clear the working space
  rm(list = ls())
  
  # Set working directory (edit for YOUR econ 42 folder)
  #setwd("/Users/wsundstrom/econ_42/data")
  #or 
  #setwd("C:/Users/wsundstrom/econ_42/data")

  # Load the packages 
  library(countrycode)
  library(doBy)
  library(dplyr)
  library(foreign)
  library(gdata)
  library(ggplot2)
  library(knitr)
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
               "SP.DYN.TFRT.IN" )# Fertility rate,(births per woman) 
  
  # Extract latest version of desired 
  # variables from WDI.
  # This may take a few minutes, 
  # depending on connection speed
  
  wdim = WDI(country="all", indicator = wdilist, 
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

### Read CPS earnings data used in SW Table 3.1 from SW web site
  earn <- read.dta("http://wps.aw.com/wps/media/objects/11422/11696965/empirical/empex_tb/cps92_08.dta")
# Read CPS data from your hard drive (make sure it is in your working directory)
  earn2 = read.csv("cps92_08.csv", header=TRUE, sep=",")

  # create new variable that adjusts 1992 values to 2008$ using CPI
  earn$realahe <- ifelse(earn$year==2008, earn$ahe, earn$ahe*215.2/140.3)

#==============================================================================
#   3. Analysis section
#==============================================================================

### Descriptive statistics for CA schools data

  # Standard descriptive statistics for all numerical variables in the data
  stargazer(caschool, type="text", median=TRUE,
            digits=2, title="CA school data set")
  
  # Descriptive statistics for selected variables
  stargazer(caschool[c("str","testscr","el_pct")], type="text", 
            digits=2, title="CA school data set")
  
  # Descriptive statistics for a subset of observations 
  stargazer(subset(caschool, smallclass==1), type="text", digits=2, 
            title="Schools with student-teacher ratio less than 20")
  
  stargazer(subset(caschool, smallclass==0), type="text", digits=2, 
            title="Schools with student-teacher ratio >= 20")
  
  # frequency tables by county, various permutations
  table(caschool$county)
  table(caschool$county, caschool$smallclass)
  table(caschool$county, caschool$smallclass, useNA="ifany")
  
  # Table of means by a factor variable (a crosstab)
  summaryBy(testscr ~ smallclass,  data=caschool , FUN=c(mean),na.rm=TRUE)
  
  # T-test for difference in means
  t.test(testscr~smallclass, data=caschool , 
         FUN=c(mean), na.rm=TRUE)

### Descriptive statistics for WDI data
  
  stargazer(wdim, type="text", median=TRUE,
            digits=2, title="WDI data set")
  
  stargazer(subset(wdim, levels(wdim$region)==
            "Middle East & North Africa (all income levels)"), 
            type="text", digits=2, 
            title="WDI data for Middle East")
  
  # Table by region of % female
  # options(digits=3)
  summaryBy(femaleperc ~ region,  data=wdim , 
            FUN=c(mean),na.rm=TRUE)

### t-tests with the cps92_08.csv dataset

  # descriptive statistics
  stargazer(earn, type="text", median=TRUE,
            digits=2, title="Earnings Data")
  
  # two-sample tests of various null hypotheses (Ho)
  
  # Ho: mean real earnings were equal in 1992 and 2008
  t.test(earn$realahe ~ earn$year)  
  
  # Ho: mean real earnings were equal for men and women in 2008
  # create data frame for just 2008 observations
  earn08 = subset(earn, year==2008)
  # run the test
  t.test(earn08$realahe ~ earn08$female)  
