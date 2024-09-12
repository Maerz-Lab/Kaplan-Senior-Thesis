#Vanessa Terrell
## 12 Sept 2024
## working to subset data

#import data set
dat.Metamorphosis20092012<-read.csv("L. capito_UGA master rearing data_2009-2012.csv")

#use subset() function to create a new dataframe for 2009-2012 data

master.met <- subset(dat.Metamorphosis20092012, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments)) 

#pulling in 2013 data
dat.meta2013<-read.csv("L. capito_UGA rearing data_2013.csv")

#using subset() function to create new dataframe for 2013 data
master.met2013 <- subset(dat.meta2013, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments)) 

#merge master datasets to one Master file with the same column names 
library(dplyr)
merged.Master <- rbind(master.met, master.met2013)

#This works to subset and merge dataframes with the same column names at the same time.
test.merged.Master <- rbind(dat.meta2013[c("Year", "Date.metamorphosed", "Mass.g.metamorphosed", "Fate.comments")], dat.Metamorphosis20092012[c("Year", "Date.metamorphosed", "Mass.g.metamorphosed", "Fate.comments")])


## TO DO
# Go into each file (2015-2023) and update column headings to match those used on line 23
# Use code on line 23 to make a master captive rearing dataset (all years)