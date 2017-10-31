#==============================================================================
#   Data Analysis Tutorial: Install "most used" packages for R
#==============================================================================

# by Bill Sundstrom and Michael Kevane
# for Econ 41-42
# This version; Sept 2017

#==============================================================================
#   Install packages
#==============================================================================

# You should only need to run the following "install" commands once:
# Install the packages for this course
# Note if you change computers or wipe your computer you will have to reinstall
install.packages(c("countrycode", "doBy", "dplyr", "foreign"))  
install.packages(c("ggplot2", "knitr", "lmtest", "openintro", "OIdata"))
install.packages(c("readstata13", "reshape", "sandwich", "stargazer"))
install.packages(c("WDI", "XML", "gdata", "data.table", "magrittr"))
install.packages(c("tidyr", "erer", "plm", "ivpack", "sampleSelection"))

# Some Mac users with older OS have trouble installing these two packages
# If they do not install then just ignore for now, class uses them around week 4
# See help, but not urgent
install.packages("car")
install.packages("AER")