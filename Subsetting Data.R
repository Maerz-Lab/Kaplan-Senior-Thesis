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

#Read the 2024 data file
library(readr)
Rcapito2024 <- read_csv("Rcapito2024.csv")

#Run lubridate
library(lubridate)

# Convert the DateTime column to a POSIXct object
# Parse the Date and Time column
Rcapito2024$`Date and Time` <- mdy_hms(Rcapito2024$`Date and Time`, tz = "EST")


Rcapito2024 <- Rcapito2024 %>%
  mutate(
    Date = format(`Date and Time`, "%Y-%m-%d"),  # Extract date in YYYY-MM-DD format
    Time = format(`Date and Time`, "%H:%M:%S")   # Extract time in HH:MM:SS format
  )

# Print the result
print(Rcapito2024)

# converting the date column to a date from a character
Rcapito2024$Date <- as.Date(Rcapito2024$Date, format = "%m/%d/%Y")
str(df)





## Notes for Vanessa:

## The survey123 used AM/PM time instead of military and we ran into parsing 
## issues. Line 34 fixed this by including the time zone and converting to military time


## TO DO
# Go into each file (2015-2023) and update column headings to match those used on line 23
# Use code on line 23 to make a master captive rearing dataset (all years)

## Notes for Morgan:
## Year
## Date.metamorphosed
## Mass.g.metamorphosed
## Fate.comments