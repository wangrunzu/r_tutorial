
#==============================================================================
#   Data Analysis Tutorial: Merging datasets
#==============================================================================

# original Bill Sundstrom 9/1/2014
# edits by Michael Kevane 12/28/2014
# edits by Michael Kevane 8/29/15

# Description: Merging two datasets

#==============================================================================
#   1. Settings, packages, and options (run these every R session)
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR econ 42 folder)
setwd("/Users/mkevane/files42/data")

# Load the packages (must have been installed: see tutorial_2)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(ggplot2)
library(countrycode)
library(reshape)
library(WDI)
library(gdata)
library(doBy)

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

### Read the WDI data

# Create a list of variables to import create a list
wdilist <- c("NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2005 intl $)
             "SP.POP.GROW", # Population growth (annual %)
             "SP.POP.TOTL", # Population, total
             "SP.POP.TOTL.FE.ZS", # Population, female (% of total)
             "SP.URB.TOTL.IN.ZS", # Urban population (% of total)
             "SP.POP.BRTH.MF", # Sex ratio at birth (females per 1,000 males)
             "SP.DYN.LE00.IN", # Life expectancy at birth, total (years)
             "SP.DYN.LE00.FE.IN", # Life expectancy at birth, female (years)
             "SP.DYN.LE00.MA.IN", # Life expectancy at birth, male (years),
             "SP.DYN.IMRT.IN", # Infant mortality rate
             "SP.DYN.TFRT.IN") # Fertility rate, total (births per woman)

# Extract latest version of desired variables from WDI.
# This takes a long time, so below save as csv file and the import rather than download from WDI each time
wdim <- WDI(country="all", indicator = wdilist, extra = TRUE, start = 2015, end = 2015)

# Rename the variables
wdim <- rename.vars(wdim, c("NY.GDP.PCAP.PP.KD","SP.POP.TOTL"), c("GDPpcUSDreal","population"))
wdim <- rename.vars(wdim, c("SP.POP.TOTL.FE.ZS","SP.URB.TOTL.IN.ZS"), c("femaleperc","urbanperc"))
wdim <- rename.vars(wdim, c("SP.POP.BRTH.MF","SP.DYN.LE00.IN"), c("sexratiobirth","lifeexp"))
wdim <- rename.vars(wdim, c("SP.POP.GROW"), c("popgrow"))
wdim <- rename.vars(wdim, c("SP.DYN.LE00.FE.IN","SP.DYN.LE00.MA.IN"), c("lifexpfem","lifeexpmale"))
wdim <- rename.vars(wdim, c("SP.DYN.IMRT.IN","SP.DYN.TFRT.IN"), c("infmort","fertility"))

# Take out the entries that are aggregates (eg East Asia) and not countries
wdim = subset(wdim, !( region=="Aggregates") ) 
table(wdim$region)

### Read the COW religions data
religion = read.csv("http://www.correlatesofwar.org/data-sets/world-religion-data/wrp-national-data-1/at_download/file", 
                    header=TRUE, sep=",", strip.white=TRUE, 
                    stringsAsFactors=FALSE)
religion = subset(religion, 
                  religion$year=="2010" )
religion = subset(religion, select = 
                    c(islmgenpct,budgenpct, hindgenpct, 
                      chrstcatpct, chrstprotpct, name ) )

# convert country code in data frame religion from correlates 
# of war code- cowc- to World Bank code- iso3c.
# using package "countrycode" that have previously installed
religion$iso3c=countrycode(religion$name, "cowc", "iso3c")
# rename to be country
religion <- rename(religion,c('name'='country'))

### Merge two data sets using a common variable to match observations

# If we want to keep only the countries that have # observations from BOTH data sets:
common = merge(religion, wdim, by="iso3c")

# If we want to keep all the data from both 
# data sets:
combine = merge(religion, wdim, by="iso3c", 
                all.x = TRUE, all.y = TRUE)

#==============================================================================
#   3. Analysis section
#==============================================================================

# Describe the merged data sets
stargazer(common, type="text", median=TRUE,
          digits=2, title="WDI and Religion data set - Common")
stargazer(combine, type="text", median=TRUE,
          digits=2, title="WDI and Religion data set - Combined")

### Regressions with the combined data
regwdi1 = lm(GDPpcUSDreal ~ chrstcatpct + chrstprotpct + islmgenpct + budgenpct + hindgenpct, data=common)

regwdi2 = lm(femaleperc ~ GDPpcUSDreal+chrstcatpct + chrstprotpct + islmgenpct + budgenpct + hindgenpct, data=combine)

regwdi3 = lm(fertility ~ GDPpcUSDreal+ chrstcatpct + chrstprotpct + islmgenpct + budgenpct + hindgenpct, data=combine)

stargazer(regwdi1, regwdi2,regwdi3,
          se=list(cse(regwdi1), cse(regwdi2), cse(regwdi3)), 
          title="Religion and GDP per capita", type="text", 
          df=FALSE, digits=3)

### A reminder of how to save and retrieve

# Save the merged data set to your computer 
save(combine, file="wdi_religion_combine.Rdata")
# Retrieve the data set
load("wdi_religion_combine.Rdata")

