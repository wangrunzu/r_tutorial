
#==============================================================================
#   Data Analysis Tutorial: Graphs and simple t-tests
#==============================================================================

# original Bill Sundstrom 9/1/2014
# edits by Michael Kevane 12/28/2014
# edits by Bill Sundstrom 8/14/2015
# edits by Michael Kevane 8/29/2015
# edits by Bill Sundstrom 9/1/2016
# edits by Michael Kevane 12/15/2016 (use ggplot, not qplot)

# Description: Create graphs - histograms, box plots and scatter plots


#==============================================================================
#   1. Settings, packages, and options
#==============================================================================

# You should generally run all of the commands in Section 1 
# at the start of every R session

  # Clear the working space
  rm(list = ls())
  
  # Set working directory (edit for YOUR econ 42 folder)
  setwd("/Users/mkevane/econ_42/files42/data")
  
  # Load the packages (must have been installed)
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
  cse <- function(reg) {
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

### Create some plots

### Histogram
  ggplot(caschool, aes(testscr)) + 
  geom_histogram()+
  labs(title="Histogram of test score") +
  labs(x="Test score", y="Count")
  
  ggplot(data=caschool, aes(testscr)) + 
    geom_histogram(breaks=seq(600, 700, by = 10), 
         col="red", fill="green", alpha = .2) + 
    labs(title="Histogram of test score") +
    labs(x="Test score", y="Count")
  
  # alternative
  hist(caschool$testscr, 
       main="Histogram of test scores", 
       xlab="Test score")


### Boxplots
  # Compare test scores by small vs. large classes
  # Note that for the side-by-side boxplot, the first variable
  # (here smallclass) must be a factor variable.
  # You can turn a numeric variable into a factor variable very easily
  # with the as.factor function
  ggplot( data=caschool, aes(smallclass, testscr, fill=smallclass)) +
        geom_boxplot() + 
        labs(title="Test score by small class size") + 
        labs(x="Average class size under 20", y="Test score")
  
  # Create a box plot with the WDI data
  # note suppress the x-axis labels and ticks to make it look nicer
  ggplot( data=wdim, aes(x=region, y=femaleperc, fill=region)) +
        geom_boxplot() + 
        labs(title="Percent female by region") + 
        labs(x="Region", y="Percent female")

  # Shorten the names of the regions
  levels(wdim$region)[levels(wdim$region)=="Europe & Central Asia (all income levels)"] <- "Europe"
  levels(wdim$region)[levels(wdim$region)=="Middle East & North Africa (all income levels)"] <- "M East"
  levels(wdim$region)[levels(wdim$region)=="East Asia & Pacific (all income levels)"] <- "East Asia"
  levels(wdim$region)[levels(wdim$region)=="Latin America & Caribbean (all income levels)"] <- "Latam"
  levels(wdim$region)[levels(wdim$region)=="Sub-Saharan Africa (all income levels)"] <- "Africa"
  levels(wdim$region)[levels(wdim$region)=="South Asia"] <- "S Asia"
  levels(wdim$region)[levels(wdim$region)=="North America"] <- "N America"
  
  # A better box plot?
  # Change scale of y-axis and drop legend since now have shorter names
  chart0 =  ggplot( data=wdim, aes(x=region, y=femaleperc, fill=region)) +
        geom_boxplot() + 
        labs(title="Percent female by region") + 
        labs(x=" ", y="Percent female")+
        theme(legend.position="none")
  # scale y limits 
  chart1 = chart0 + coord_cartesian(ylim = c(40,54))
  # display the resulting chart, which is called chart1
  chart1

### Scatter plots (X-Y scatters)

  # Test score by class size (str) 
  ggplot(data=caschool, aes(x=str, y=testscr)) +
    geom_point(shape=0)+
    labs(title="CA test scores") + 
    labs(x="Student-teacher ratio", y="Test score")
  # Test score by average income in district
  ggplot(data=caschool, aes(x=avginc, y=testscr)) +
    geom_point(shape=0)+
    labs(title="CA test scores") + 
    labs(x="Average district income", y="Test score")
  # Test score by class size, with dot size reflecting district income 
  ggplot(data=caschool, aes(x=str, y=testscr, size=avginc)) +
    geom_point(shape=1)+
    labs(title="CA test scores") + 
    labs(x="Student-teacher ratio", y="Test score")
  
  # add a linear regression line to the plot
  # method="lm" means linear regression
  ggplot(caschool, aes(x=str, y=testscr)) + 
    labs(y = "Test score",x = "Student-teacher ratio" ,title = "CA test scores") +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=lm) 
  
  # add a "loess" smoothed curve through the plot
  ggplot(caschool, aes(x=str, y=testscr)) + 
    labs(y = "Test score", x = "Student-teacher ratio" ,title = "CA test scores") +
    geom_point(shape=1) +    # Use hollow circles
    geom_smooth(method=loess) 
  
  # a different way to do the plot and best-fitting line
  plot(testscr ~ str,  data=caschool, xlab="Student-teacher ratio", ylab="Test score", 
       col= "blue",
       main="CA test scores")     
  abline(lm(testscr ~ str , data=caschool), col= "red")
  
  # scatter plot comparison for Los Angeles and Santa Clara counties...
  # differentiate the counties by the shape and color of the points 
  # in the subset command the vertical line "|" means "OR"
  
  ggplot(data=subset(caschool, county=="Santa Clara" | county=="Los Angeles"), 
         aes(x=str, y=testscr, shape=county, color=county, size=1)) +
    geom_point()+
    labs(title="CA test scores") + 
    labs(x="Student-teacher ratio", y="Test score")+
    theme(legend.position="none")
    
  # Scatter plot of income per capita and percent female using the WDI data
  ggplot(data=wdim, aes(x=GDPpcUSDreal, y=femaleperc)) +
    geom_point()+
    labs(title="Income and percent female") + 
    labs(x="GDP per capita", y="Percent female")+
    theme(legend.position="none")
    
  ggplot(data=subset(wdim,GDPpcUSDreal<5000) , aes(x=GDPpcUSDreal, y=femaleperc)) +
    geom_point()+
    labs(title="Income and percent female, for low-income only") + 
    labs(x="GDP per capita", y="Percent female")+
    theme(legend.position="none")
