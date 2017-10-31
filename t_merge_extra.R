
#==============================================================================
#   Data Analysis Tutorial: Merging datasets
#==============================================================================

# original Michael Kevane 8/29/15
# edits by Michael Kevane 12/15/2016 

# Description: Merging two datasets

#==============================================================================
#   1. Settings, packages, and options (run these every R session)
#==============================================================================

# Clear the working space
rm(list = ls())

# Set working directory (edit for YOUR econ 42 folder)
setwd("/Users/yournamehere/files42/data")

# Load the packages (must have been installed: see tutorial_2)
library(XML)
library(plyr)
library(dplyr)
library(sandwich)
library(lmtest)
library(car)
library(stargazer)
library(ggplot2)
library(gdata)
library(doBy)
library(stringr)
library(data.table)
library(tidyr)

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

### Scrape the salary data from the Transparent California website
# Looping through 50 pages takes a few minutes
# Looping through 500 pages to get more complete data take 15-20 minutes
sal <- list()
for (i in 2:50) {
  url = paste0("http://transparentcalifornia.com/salaries/santa-clara-county/", "?page=", i)
  tt <- readHTMLTable(url)
  n.rows <- unlist(lapply(tt, function(t) dim(t)[1]))
  sal[[i]] <- ldply(tt, data.frame)
}
sccountysal <- rbind.fill(sal) 

# remove some data sets from your workspace
rm(list = c("tt", "sal", "i", "n.rows", "url"))

# Clean up some of the data and transform
sccountysal$totalpay = str_replace(sccountysal$Total.pay...benefits,"[$]", "")
sccountysal$totalpay1 = str_replace(sccountysal$totalpay,"[,]", "")
sccountysal$totalpay2 = as.numeric(sccountysal$totalpay1)

sccountysal$name = as.character(sccountysal$Name)
sccountysal$namelc = tolower(sccountysal$name)

sccountysal$firstname <- sapply(strsplit(sccountysal$namelc, " "), '[', 1)
sccountysal$lastname <- sapply(strsplit(sccountysal$namelc, " "), '[', 1)
                               
# Read in a csv file of the gender of names of people born in California
f = read.table("http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/areas/nlp/corpora/names/female.txt", 
               sep="\t", col.names=c( "name"), fill=FALSE, strip.white=TRUE)
f$female=1
m = read.table("http://www.cs.cmu.edu/afs/cs/project/ai-repository/ai/areas/nlp/corpora/names/male.txt", 
               sep="\t", col.names=c( "name"), fill=FALSE, strip.white=TRUE)
m$female=0
names <- rbind(f,m)
names$name = as.character(names$name)
names$firstname =tolower(names$name)
names <- unique(names, by = "name")

### Merge two data sets using a common variable to match observations
# merge the datasets using join in library(plyr)
# set type='left' here so that  get all of the first data frame, 
# but only the relevant rows from the names dataset.
sccsal = join(sccountysal, names, by='firstname', type='left', match='all')

# Create a factor var for female, for box plot
sccsal$femalefac=as.factor(sccsal$female)

#==============================================================================
#   3. Analysis section
#==============================================================================

# Describe the merged data set
# Standard descriptive statistics for all numerical variables in the data
stargazer(sccsal, type="text", median=TRUE,
          digits=2, title="Salary data set")

# Many of the names do not match to the list of names and so are not assigned gender
table(sccsal$femalefac, useNA="ifany")

# Boxplots -  change scale of y-axis and drop outliers
p0 = ggplot(aes(y = totalpay2, x = femalefac), data = sccsal) + geom_boxplot(outlier.size=NA)+ 
  labs(title="Total pay according to gender", x="Female?", y="Annual pay, outliers excluded")
# compute lower and upper whiskers
ylim1 = boxplot.stats(sccsal$totalpay2)$stats[c(1, 5)]
# scale y limits based on ylim1
p1 = p0 + coord_cartesian(ylim = ylim1*1.05)
p1

### Regressions with the combined data
reg1 = lm(totalpay2 ~ female, data=sccsal)

stargazer(reg1, 
          se=list(cse(reg1)), 
          title="Pay and gender", type="text", 
          df=FALSE, digits=3)

# Save a data set  
# save(sccountysal, file="sccountysal.Rdata")
# Retrieve the data set
# load("sccountysal.Rdata")

# Save the data set to a csv file
# write.csv(sccountysal, file = "salaries santa clara cty.csv")

#### Reshaping
# to reshape a data set from wide to long or long to wide
ts1 <- runif(20)+2
ts2 <- runif(20)+3
ts3 <- runif(20)+4
ts4 <- runif(20)+6
ts5 <- runif(20)+7
id <- seq(1, 20, by=1)
scores <- data.frame(ts1,ts2,ts3,ts4,ts5,id)
# syntax makes use of the "pipe" operator (%>%) from dplyr
scores_l <- scores %>% gather(test, score, ts1:ts5)

scores_l$testnum=substr(scores_l$test,3,3)
ggplot(data=scores_l, aes(x = testnum, 
                          y = score, color=id)) +
  geom_point(size = 1.5,alpha = 0.75)
ggplot(data=scores_l, aes(x = testnum, y = score))+ geom_boxplot()


