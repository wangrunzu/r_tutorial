
#==============================================================================
#   Data Analysis Tutorial: Panel data models
#==============================================================================

# Original: Bill Sundstrom 6/20/2015
# edits: Bill Sundstrom 6/29/2016
# edits: Michael Kevane 12/15/2016 minor

# Description: Panel data using examples from S&W ch. 10
#             Also clustering SEs 

#==============================================================================
#   1. Settings, packages, and options (run these every R session)
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR econ 42 folder)
setwd("/Users/yournamehere/files42/data")

# Load the packages (must have been installed: see tutorial_2)
# install.packages(c("plm", "ivpack"))
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

# turn off scientific notation except for big numbers
options(scipen = 9)

### functions for correct SEs in regression tables

  # function to calculate corrected SEs for OLS regression 
  cse = function(reg) {
    rob = sqrt(diag(vcovHC(reg, type = "HC1")))
    return(rob)
  }
  
  # clustered SEs, clustered on "group"... could also cluster on "time" 
  # compute Stata-like degrees of freedom adjustment for number of groups
  # See http://www.richard-bluhm.com/clustered-ses-in-r-and-stata-2/
  
  clse = function(reg) { 
    # index(reg, "id") returns the id or entity variable vector 
    G = length(unique(index(reg,"id")))
    N = length(index(reg,"id"))
    dfa = (G/(G - 1))   # note Bluhm multiplies this by finite-sample df adjustment
    rob = sqrt(diag(dfa*vcovHC(reg, method="arellano", type = "HC1", 
                               cluster = "group")))
    return(rob)
  }
  
  # corrected SEs for IV regressions
  ivse = function(reg) {
    rob = robust.se(reg)[,2]
    return(rob)
  }


#==============================================================================
#   2. Data section for panel regressions
#==============================================================================

### Read the data set (AER package)

  data("Fatalities")
  
  # units used in SW
  # vehicle fatality rate (vehicle fatalities/ population)
  Fatalities$mrall=Fatalities$fatal/Fatalities$pop
  # fatality rate per 10000
  Fatalities$fatality_rate = Fatalities$mrall*10000
  

#==============================================================================
#   3. Analysis: Panel data and fixed effects estimation
#==============================================================================

# descriptive statistics
  stargazer(Fatalities, type="text", median=TRUE,
          digits=2, title="Traffic Fatalities Data")

### (a) Simple cross-section regressions for comparison
  
  # Let's plot the data for 1982 and 1988 cross sections, with regression lines
  ggplot(subset(Fatalities, year==1982), aes(x=beertax, y=fatality_rate)) + 
    labs(y="Fatality rate", x="Beer tax", title = "Traffic fatalities 1982") +
    geom_point(shape=1) +    
    geom_smooth(method=lm, se=F) 
  ggplot(subset(Fatalities, year==1988), aes(x=beertax, y=fatality_rate)) + 
    labs(y="Fatality rate", x="Beer tax", title = "Traffic fatalities 1988") +
    geom_point(shape=1) +    
    geom_smooth(method=lm, se=F)  
  
  # Simple regressions for 1982 and 1988 cross sections, as in SW
  reg1982 = lm(fatality_rate ~ beertax,
               data = subset(Fatalities, year==1982))
  reg1988 = lm(fatality_rate ~ beertax,
               data = subset(Fatalities, year==1988))
  stargazer(reg1982, reg1988,   
            se=list(cse(reg1982),cse(reg1988)), 
            title="Simple cross-section regressions", type="text", 
            column.labels=c("1982", "1988"), 
            df=FALSE, digits=4)
 
### (b) Panel regressions using plm(...)
  
  # Now we use the full data set, all seven years 1982-1988
  # We'll estimate pooled, fixed-effect (FE), and first-difference (FD) regressions 

  # In place of lm(...) we use plm(...)
  # In each plm command index = c("state","year") means 
  # the first var (state) is the entity and second var (year) is the time variable

### reg1: OLS (pooled), and equivalent to lm(fatality_rate ~ beertax, ...)
  
  reg1 = plm(fatality_rate ~ beertax,
             data = Fatalities, index = c("state","year"), model="pooling")

### reg2: state FEs
  
  # In this command, model="within" is fixed effects (demeaned model), 
  # effect="individual" means just entity (state) FEs
  reg2 = plm(fatality_rate ~ beertax,
             data = Fatalities, index = c("state","year"), 
             model="within", effect="individual")
  
  # What is this FE regression doing? Basically it is including a full set of 
  # state dummy variables. We can see this by including state (a factor variable)
  # as a regressor in OLS
  reg2a = lm(fatality_rate ~ beertax + state,
             data = Fatalities)
  
  # compare coefficient (slope) on beertax for FE regression vs dummy-variable version
  stargazer(reg2, reg2a, 
            title="State FEs", type="text", 
            df=FALSE, digits=4)

### reg3: We can include year FEs as well as state FEs 
  
  # effect="twoways" means both entity and time FEs
  reg3 = plm(fatality_rate ~ beertax,
             data = Fatalities, index = c("state","year"), 
             model="within", effect="twoways" )

  # Now put regs 1,2,3 in a table with SEs clustered by state
  # We cluster the SEs by state using the clse function
  stargazer(reg1, reg2, reg3,  
            se=list(clse(reg1),clse(reg2),clse(reg3)), 
            title="Panel regressions, clustered SEs", type="text", 
            column.labels=c("Pooled OLS", "State FE", "St-Yr FE"), 
            df=FALSE, digits=4)
  
  # compare unadjusted and clustered SEs for reg2: clustering matters!
  stargazer(reg2a, reg2, 
            se=list(cse(reg2a),clse(reg2)),           
            title="FE regressions", type="text", 
            column.labels=c("no cluster", "clustered"), 
            keep=c("beertax"),  # the keep option shows only selected coefficients in table 
            df=FALSE, digits=4)

### reg4: We can also run the model in first differences 
  
  reg4 = plm(fatality_rate ~ beertax,
             data = Fatalities, index = c("state","year"), model="fd")
  
  # This should compare with reg2 (state FEs)
  stargazer(reg2, reg4,  
            se=list(clse(reg2),clse(reg4)),     
            title="FE vs FD", type="text", 
            column.labels=c("FE", "FD"), 
            df=FALSE, digits=4)
  
  # Note that the results are not very similar! This can indicate a problem.
  
  # You could add year effects to fd, but need to create the year dummies "by hand"
  Fatalities <- mutate(Fatalities,
                       y83 = as.numeric(year==1983),
                       y84 = as.numeric(year==1984),
                       y85 = as.numeric(year==1985),
                       y86 = as.numeric(year==1986),
                       y87 = as.numeric(year==1987),
                       y88 = as.numeric(year==1988))

  reg4a = plm(fatality_rate ~ beertax + y84+y85+y86+y87+y88,
             data = Fatalities, index = c("state","year"), 
             model="fd" )

  stargazer(reg4, reg4a,  
            se=list(clse(reg4),clse(reg4a)),     
            title="FD with and without year effects", type="text", 
            column.labels=c("without", "with"), 
            df=FALSE, digits=4)


### Adding more regressors
  
  # Note we can run models with more variables, such as spirits (spirits consumption)
  # They must be variables that are time-varying within state
  reg3a = plm(fatality_rate ~ beertax + spirits,
              data = Fatalities, index = c("state","year"), 
              effect="twoways", model="within")
  stargazer(reg3, reg3a,  
            se=list(clse(reg3),clse(reg3a)),     
            title="Fixed-effect regressions", type="text", 
            df=FALSE, digits=4)
  
### Extract the fixed effects (as deviations from intercept)
  
  # Sometimes we would like to retrieve the fixed effects coefficients, 
  # as if we had run the regression with dummy variables for state and time
  
  # this can be done after plm with the function fixef
  fixef(reg3a,effect="individual")
  fixef(reg3a,effect="time")

  # save the fixed effects and examine selected values
  ife <- fixef(reg3a,effect="individual")
  ife[c("al","ny")]