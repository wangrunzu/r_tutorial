
#==============================================================================
#   Data Analysis Tutorial: Binary dependent variables
#==============================================================================

# original Michael Kevane 2/15/2015
# Latest version: Bill Sundstrom 4/16/2015

# Description: Replicate Stock and Watson regression probit table 11.2 in Chapter 11 

#==============================================================================
#   1. Settings, packages, and options (run these every R session)
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR econ 42 folder)

# Load the packages (must have been installed: see tutorial_2)
# install.packages("erer")
library(AER)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(ggplot2)
library(gdata)
library(doBy)
library(erer)

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

# read the data from AER package
data("HMDA")

# Create a factor variable of levels
HMDA$lvratcat = factor(ifelse(HMDA$lvrat < 0.8, "low",
                  ifelse(HMDA$lvrat >= 0.8 & HMDA$lvrat <= 0.95, "medium", "high")),
                  levels = c("low", "medium", "high"))

# Turn two factor variables into numeric variables
HMDA$mhist = as.numeric(HMDA$mhist)
HMDA$chist = as.numeric(HMDA$chist)

# Truncate the two ratios for better boxplot
HMDA$pirat2 = ifelse(HMDA$pirat >1, 1, HMDA$pirat) 
HMDA$hirat2 = ifelse(HMDA$hirat >1, 1, HMDA$hirat) 

#==============================================================================
#   3. Analysis section
#==============================================================================

# Standard descriptive statistics for all numerical variables in the data
stargazer(HMDA, type="text", median=TRUE,
          digits=2, title="HMDA data set")

# Boxplots - a straightforward way to do it... we truncate the outliers with the subset
ggplot(data=subset(HMDA, pirat<1), aes(y=pirat, x=deny, fill=deny)) +
      geom_boxplot() + 
      labs(title="P/I ratio by whether denied a loan") + 
      labs(x="Was denied a loan?", y="P/I ratio")
# A scatter plot is not very informative- note convert outcome deny to numeric 0-1
ggplot(data=subset(HMDA, pirat<1), aes(y=(as.numeric(deny)-1), x=pirat)) +
      geom_point() + 
      labs(title="Denial by P/I ratio excluding outliers") + 
      labs(x="P/I ratio", y="Deny")
# But the relationship is positive...  note linear model is questionable
ggplot(data=subset(HMDA, pirat<1), aes(y=(as.numeric(deny)-1), x=pirat)) +
      geom_point() + 
      labs(title="Denial by P/I ratio excluding outliers") + 
      labs(x="P/I ratio", y="Deny")+
      geom_smooth(method=lm) 

### Regressions for SW Table 11.2

# Run LPM regression - note convert outcome deny to numeric in the regression command
fm1 = lm(I(as.numeric(deny) - 1) ~ afam + pirat + hirat + lvratcat + chist + mhist +
            phist + insurance + selfemp, data = HMDA)

# Run logit regression
fm2 = glm(deny ~ afam + pirat + hirat + lvratcat + chist + mhist + 
          phist + insurance + selfemp, 
          family = binomial, x=TRUE, data = HMDA)

# Run probit regressions
fm3 = glm(deny ~ afam + pirat + hirat + lvratcat + chist + mhist + 
          phist + insurance + selfemp, 
          family = binomial(link = "probit"), x=TRUE, data = HMDA)
fm4 = glm(deny ~ afam + pirat + hirat + lvratcat + chist + mhist + phist + insurance +
          selfemp + single + hschool + unemp, family = binomial(link = "probit"), 
          x=TRUE, data = HMDA)
fm5 = glm(deny ~ afam + pirat + hirat + lvratcat + chist + mhist + phist + insurance +
             selfemp + single + hschool + unemp + condomin + 
             I(mhist==3) + I(mhist==4) + I(chist==3) + I(chist==4) + I(chist==5) + I(chist==6), 
           family = binomial(link = "probit"), x=TRUE, data = HMDA)
fm6 = glm(deny ~ afam * (pirat + hirat) + lvratcat + chist + mhist + phist + insurance +
          selfemp + single + hschool + unemp, family = binomial(link = "probit"), 
          x=TRUE, data = HMDA)

# Present results of regressions in a single table as in S&W T 11.2
# Here we use the adjusted SE for LPM and the default (NULL) for logit and probit 
stargazer(fm1, fm2, fm3, fm4, fm5, fm6,
          se=list(cse(fm1),NULL,NULL,NULL,NULL,NULL), 
          title="Regression Results", type="text", 
          df=FALSE, digits=5)

# F tests of joint hypotheses can be conducted using lht. For example:
lht(fm4, c("pirat = 0", "hirat = 0")) 

### Interpreting the probit results

# Marginal effects using package erer
# x.mean=TRUE would calculate margins at the means of each X
# x.mean=FALSE calculates margin for each obs and then averages
# need option x=TRUE in glm estimation

fm3a = maBina(fm3, x.mean = TRUE, rev.dum = TRUE, digits = 3)
fm3b = maBina(fm3, x.mean = FALSE, rev.dum = TRUE, digits = 3)

stargazer(fm3a, fm3b,
          title="Probit margins", type="text", 
          df=FALSE, digits=3)

# Plot the predicted prob of denial by P/I ratio by race
preplot = maTrend(fm3b, n = 300, nam.c = "pirat", nam.d = "afamyes", simu.c = TRUE)
plot(preplot)

# If you want to see directly how to calculate marginal probabilities 
# with given values for the X's
# Use the following code

# Retrieve coefficients estimated from regression (3) to calculate probabilty of denial
a= (coef(summary(fm3))["afamyes","Estimate"])
b= (coef(summary(fm3))["pirat","Estimate"])
c= (coef(summary(fm3))["hirat","Estimate"])
d= (coef(summary(fm3))["lvratcatmedium","Estimate"])
e= (coef(summary(fm3))["lvratcathigh","Estimate"])
f= (coef(summary(fm3))["chist","Estimate"])
g= (coef(summary(fm3))["mhist","Estimate"])
h= (coef(summary(fm3))["phistyes","Estimate"])
i= (coef(summary(fm3))["insuranceyes","Estimate"])
j= (coef(summary(fm3))["selfempyes","Estimate"])
k= (coef(summary(fm3))["(Intercept)","Estimate"])

# Create a prob of denial curve for different levels of PI ratio if *is not* African American
curve(pnorm(a*0+ b*x + c*mean(HMDA$hirat) +d*1 + e*0 +f*mean(HMDA$chist)+g*mean(HMDA$mhist)+
              h*1+i*0+j*0+k),  xlim = c(0, 3),lty = 8, col = "red", 
      ylab="", xlab="P/I ratio", main = "Probability of mortgage denial")

# Create a prob of denial curve for different levels of PI ratio if *is* African American
curve(pnorm(a*1+ b*x + c*mean(HMDA$hirat) +d*1 + e*0 +f*mean(HMDA$chist)+g*mean(HMDA$mhist)+
              h*1+i*0+j*0+k),  xlim = c(0, 3), add=TRUE, col = "blue")

# Add a legend to the plot
legend(1.2, .3, c("Afam", "non-Afam"), col = c("blue", "red"),
       text.col = "black", lty = c(1, 2), pch = c(NA, NA),
       merge = TRUE, bg = "white")

# Calculate prob of denial if *is not* African American, at mean values
pnorm(a*0+ b*mean(HMDA$pirat) + c*mean(HMDA$hirat) +d*1 + e*0 +f*mean(HMDA$chist)+g*mean(HMDA$mhist)+
        h*1+i*0+j*0+k)
# Calculate prob of denial if *is* African American, at mean values
pnorm(a*1+ b*mean(HMDA$pirat) + c*mean(HMDA$hirat) +d*1 + e*0 +f*mean(HMDA$chist)+g*mean(HMDA$mhist)+
        h*1+i*0+j*0+k)

