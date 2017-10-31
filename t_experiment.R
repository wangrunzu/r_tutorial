
#==============================================================================
#   Data Analysis Tutorial #11
#==============================================================================

# Original Michael Kevane  12/14/2015
# Adapted from http://egap.org/methods-guides/10-types-treatment-effect-you-should-know-about
# Description: Field experiments (RCT), estimating ATE, non-compliance and attrition

#==============================================================================
#   1. Settings, packages, and options (run these every R session)
#==============================================================================

# Clear the working space
rm(list = ls())

# Set the working directory

#install.packages(c("CausalGAM", "sm", "quantreg"))   
library(lmtest) 
library(sandwich) 
library(AER) 
library(car)
library(stargazer)
library(ggplot2)
library(gdata)
library(doBy)
library(plm)
library(ivpack)
#library(quantreg) 
#library(CausalGAM) 
#library(sm)

# turn off scientific notation except for big numbers
options(scipen = 9)
# function to calculate corrected SEs for OLS regression 
cse = function(reg) {
  rob = sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
  }

#==============================================================================
#   2. Data section 
#==============================================================================

# Generate a dataset 
  set.seed(1234) # For replication 
  N = 2000 # Population size 
  b0 <- -1.6
  b1 <- 3
  b2 = 7.3
  b3 = -4.3
  x1 <- runif(n=N, min=18, max=60)
  x2 <- runif(n=N, min=5, max=25)
  x3 <- runif(n=N, min=0, max=10)
  err <- rnorm(n=N, mean=0, sd=20) 
  Y0 = runif(N) # Potential outcome under control condition 
  D1 = sample((1:N)%%2) # Treatment - ITT: 1 if treated, 0 otherwise 

# Outcome in population when no compliance or attrition
# If treated their outcome is ten times higher than their Y0
# mean Y0 = .5 and uniformly distributed between 0-1
  Y1 = b0 + D1*(Y0*10) + (1-D1)*Y0 + b1*x1 + b2*x2 + b3*x3 + err 

# Now create two different indicators under two non-compliance 
# scenarios, called 2 and 3
# Scenario 2- some of those assigned to treatment do not comply
  pD2 <- pnorm(-3+rnorm(N,mean=5*Y0)) # Prob of complying varies positive with Y0
  D2<-rbinom(N,1,pD2) # Compliance in scenario 2
  C2=D1*D2 # Those who are observed as treated
# Scenario 3- some of the treated and some of the control do not comply
# Prob of complying varies + with Y0 and treatment, 
# some who do not get treatment do not comply and take treatment
  pD3 <- pnorm(-3+rnorm(N,mean=10*Y0+2*D1)) 
  D3<-rbinom(N,1,pD3) 
  C3 = D1*D3 + (1-D1)*(1-D3) # Those who are observed as treated

# Now we create the outcome that is observed
# in case 1 above treatment effect is 5  (10 times the mean of Y0 which is .5)
# in case 2 treatment effect is 5  (10 times the mean of Y0 which is .5)
# in case 3 treatment effect is 10 (20 times the mean of Y0 which is .5)- just to be different
# Note the treatment effects here are heterogeneous, they depend on peoeple's Y0, so not the usual constant treatment effect for everyone
  Y2 = b0 + D1*D2*(Y0*10) + D1*(1-D2)*(Y0) + (1-D1)*Y0 + b1*x1 + b2*x2 + b3*x3 + err # Outcome in population 
  Y3 = b0 + D1*D3*(Y0*20) + D1*(1-D3)*(Y0) + (1-D1)*D3*(Y0) + (1-D1)*(1-D3)*(Y0*20) + b1*x1 + b2*x2 + b3*x3 + err # Outcome in population 

# Now we add some attrition into the mix
# Suppose the worse performing members of treatment group drop out
# and the better performing members of control group drop out
# will look like treatment is more effective
  pDa1 <- pnorm(2+-5*Y0+rnorm(N,mean=0)) # Prob of dropping out varies neg with Y0
  # with treatment
  summary(lm(pDa1~Y0)) # Prob of dropping varies neg with Y0
  droptreat <- (D1)*rbinom(N,1,pDa1) # 
  table(droptreat, D1)
  pDa2 <- pnorm(-4+ 5*Y0+rnorm(N,mean=0)) # Prob of dropping out varies pos with Y0
# with treatment
  summary(lm(pDa2~Y0)) # Prob of staying varies + with Y0
  dropcont <- (1-D1)*(rbinom(N,1,pDa2)) # 
  table(dropcont, D1)

  Y1attrit=Y1*(1-dropcont)*(1-D1) + Y1*(1-droptreat)*D1
  Y2attrit=Y2*(1-dropcont)*(1-D1) + Y2*(1-droptreat)*D1
  Y3attrit=Y3*(1-dropcont)*(1-D1) + Y3*(1-droptreat)*D1
  
  Y1attrit[Y1attrit==0] <- NA 
  Y2attrit[Y2attrit==0] <- NA 
  Y3attrit[Y3attrit==0] <- NA 
  
  samp = data.frame(D1, D2, D3, Y1, Y2, Y3, Y0, x1, x2, x3, 
                    C2, C3, err, droptreat, dropcont, Y1attrit, Y2attrit, Y3attrit) 
  
  samp$Y1attrit[samp$Y1attrit==0] <- NA 
  samp$Y2attrit[samp$Y2attrit==0] <- NA 
  samp$Y3attrit[samp$Y3attrit==0] <- NA 

## There are missing values in the 'samp' data because of attrition, so we remove these
## before calculating mean with 'na.rm=TRUE'. However the
## length function does not accept any such argument. Hence we get
## around this by defining our own summary function in which length is
## not supplied with this argument while mean (and others) are
# http://www.inside-r.org/packages/cran/doBy/docs/summaryBy
  sumfun <- function(x, ...){
    c(m=mean(x, ...), l=length(x))
    }
  options(digits = 3)
  summaryBy(Y0~droptreat,  data=samp , FUN=sumfun,na.rm=TRUE)
  summaryBy(Y0~dropcont,  data=samp , FUN=sumfun,na.rm=TRUE)
  summaryBy(pDa1+pDa2~D1,  data=samp , FUN=sumfun,na.rm=TRUE)
  summaryBy(droptreat~dropcont,  data=samp , FUN=sumfun,na.rm=TRUE)
  summaryBy(dropcont~droptreat,  data=samp , FUN=sumfun,na.rm=TRUE)
  summaryBy(Y0~C2,  data=samp , FUN=sumfun,na.rm=TRUE)
  summaryBy(Y0~C3,  data=samp , FUN=sumfun,na.rm=TRUE)

#==============================================================================
#   3. Analysis section
#==============================================================================

# Standard descriptive statistics for all numerical variables in the data
  stargazer(samp, type="text", median=TRUE,
            digits=2, title="RCT field experiment data set")
  
  summaryBy(D1+ D2+ D3+ Y1+ Y2+ Y3+ Y0+ x1+ x2+ x3+ C2 +C3 ~ D1,  data=samp , FUN=c(mean),na.rm=TRUE)

# Estimating ATE by difference in means
# Table of means by a categorical variable (a crosstab)
  summaryBy(Y1 ~ D1,  data=samp , FUN=c(mean),na.rm=TRUE)
  summaryBy(Y2 ~ D1,  data=samp , FUN=c(mean),na.rm=TRUE)
  summaryBy(Y3 ~ D1,  data=samp , FUN=c(mean),na.rm=TRUE)
# Another way to estimate the ATE
  ATE1 = with(samp,mean(Y1[D1==1])-mean(Y1[D1==0])) 
  ATE2 = with(samp,mean(Y2[D1==1])-mean(Y2[D1==0])) 
  ATE3 = with(samp,mean(Y3[D1==1])-mean(Y3[D1==0])) 
# An estimate of the SE of estimate of ATE 
  varestATE1<-with(samp, sqrt( (var(Y1[D1==1]))/(N/2) + (var(Y1[D1==0]))/(N/2)))
# t-statistic for hypothesis of no difference in means, ATE=0
  tstat=ATE1/varestATE1
# Direct t-test of no difference in means
  t.test(Y1~D1, data=samp , FUN=c(mean), na.rm=TRUE)
  t.test(Y1~D1, data=samp ,  na.rm=TRUE)
  
# Estimating ATE with more precision by controlling for covariates
  reg1 = lm(Y1~D1,data=samp)
  reg2 = lm(Y1~D1+x1+x2,data=samp)
  reg3 = lm(Y1~D1+x1+x2+x3,data=samp)

  stargazer(reg1, reg2, reg3,
          se=list(cse(reg1), cse(reg2),cse(reg3)), 
          title="Standard error of treatment coefficient falls with controls", type="text", 
          df=FALSE, digits=3)

# Correcting for non-compliance using instrumental variables
  summary(lm(C2~D1))

  reg1 = lm(Y2~D1+x1+x2+x3,data=samp)
  reg2 = lm(Y2~C2+x1+x2+x3,data=samp)
  reg3 = ivreg(Y2 ~ x1+x2+x3+ C2 | x1+x2+x3+D1, data = samp)
  reg4 = lm(Y3~D1+x1+x2+x3,data=samp)
  reg5 = lm(Y3~C3+x1+x2+x3,data=samp)
  reg6 = ivreg(Y3 ~ x1+x2+x3+ C3 | x1+x2+x3+D1, data = samp)
  
  stargazer(reg1, reg2, reg3, reg4,reg5, reg6,
            se=list(cse(reg1),cse(reg2),cse(reg3),cse(reg4),cse(reg5),cse(reg6)), 
            title="Regression Results", type="text", 
            df=FALSE, digits=3)

# Dealing with attrition

# Calculate some preliminary numbers
  maxY1trea=max(samp$Y1attrit[samp$D1==1], na.rm = TRUE)
  maxY1cont=max(samp$Y1attrit[samp$D1==0], na.rm = TRUE)
  
  meanY1trea=mean(samp$Y1attrit[samp$D1==1], na.rm = TRUE)
  meanY1cont=mean(samp$Y1attrit[samp$D1==0], na.rm = TRUE)
  
  minY1cont=min(samp$Y1attrit[samp$D1==0], na.rm = TRUE)
  miny1trea=min(samp$Y1attrit[samp$D1==1], na.rm = TRUE)
  
  # How many missing in each group
  mssgY1trea=sum(is.na(samp$Y1attrit[samp$D1==1]))
  mssgY1cont=sum(is.na(samp$Y1attrit[samp$D1==0]))
  
  # How many obs in each group
  counttreat=sum(D1 %in% c(1))
  countcontr=sum(D1 %in% c(0))
  
  # Percent non-missing
  perctreanonmsg=(counttreat-mssgY1trea)/counttreat
  perccontnonmsg=(countcontr-mssgY1cont)/countcontr
  
  # Without taking into account attrition
  summaryBy(Y1attrit ~ D1,  data=samp , FUN=c(mean),na.rm=TRUE)
  summaryBy(Y1 ~ D1,  data=samp , FUN=c(mean),na.rm=TRUE)
  
  # First calculate Manksi bounds
  # the upper bound assumes all missing in treatment have highest possible Y1, those missing in control have lowest outcome
  # the lower bound assumes all missing in treatment have lowest possible Y1, those missing in control have highest outcome
  # Often bounds too wide to be informative
  upperbound=(meanY1trea*perctreanonmsg+ maxY1trea*(1-perctreanonmsg)) - (meanY1cont*perccontnonmsg+minY1cont*(1-perccontnonmsg))
  lowerbound=(meanY1trea*perctreanonmsg+ miny1trea*(1-perctreanonmsg)) - (meanY1cont*perccontnonmsg+maxY1cont*(1-perccontnonmsg))
  
  # assign 75th percentile and 25th percentile
  uppermedbound =(meanY1trea*perctreanonmsg+ (quantile(samp$Y1attrit[samp$D1==1], c(.75), na.rm = TRUE))*(1-perctreanonmsg)) - (meanY1cont*perccontnonmsg+(quantile(samp$Y1attrit[samp$D1==0], c(.25), na.rm = TRUE))*(1-perccontnonmsg))
  lowermedbound =(meanY1trea*perctreanonmsg+ (quantile(samp$Y1attrit[samp$D1==1], c(.25), na.rm = TRUE))*(1-perctreanonmsg)) - (meanY1cont*perccontnonmsg+(quantile(samp$Y1attrit[samp$D1==0], c(.75), na.rm = TRUE))*(1-perccontnonmsg))
  
  # Lee bounds - estimate ATE for the "always respond"
  # Since there are more control respondents than treatment, need to trim control
  # Need to remove:
  percremove= (perccontnonmsg-perctreanonmsg)/perccontnonmsg
  ll = quantile(samp$Y1attrit[samp$D1==0], c(percremove), na.rm = TRUE) # trim the lowest control outcomes
  ul = quantile(samp$Y1attrit[samp$D1==0], c(1-percremove), na.rm = TRUE)
  upperbdtrim=meanY1trea - mean(samp$Y1attrit[samp$D1==0 & samp$Y1attrit<ul], na.rm = TRUE)
  lowerbdtrim=meanY1trea - mean(samp$Y1attrit[samp$D1==0 & samp$Y1attrit>ll], na.rm = TRUE)
  



