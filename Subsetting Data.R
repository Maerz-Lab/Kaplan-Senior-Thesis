## Primary Investigators: Vanessa Terrell and Morgan Kaplan
## Last Edited: 9/27/24
## Goal of this code: working to subset data from 2009-2024 and compile into a master datafile


# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)

# Import 2009-2012 data set
dat.Metamorphosis20092012<-read.csv("L. capito_UGA master rearing data_2009-2012.csv")

# Use subset() function to create a new dataframe for 2009-2012 data
master.met2009 <- subset(dat.Metamorphosis20092012, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments)) 

# Import 2013 data set
dat.meta2013<-read.csv("L. capito_UGA rearing data_2013.csv")

# Use subset() function to create new dataframe for 2013 data
master.met2013 <- subset(dat.meta2013, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments)) 

# Import and subset 2015 data
dat.meta2015 <- read_csv("Rcapito_MetamorphLog_2015.csv")
master.met2015 <- subset(dat.meta2015, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments))

# Import and subset 2016 data
dat.meta2016 <- read_csv("Rcapito_MetamorphLog_2016.csv")
master.met2016 <- subset(dat.meta2016, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments))

# Import and subset 2017 data
dat.meta2017 <- read_csv("Rcapito_MetamorphLog_2017.csv")
master.met2017 <- subset(dat.meta2017, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments))

# Read 2018 data
dat.meta2018 <- read_csv("Rcapito_MetamorphLog_2018.csv")

# Populate 2018 year columns shown as N/A
dat.meta2018.cleaned <- dat.meta2018 %>%
  mutate(Year = "2018")

# Save the modified dataframe back to a CSV file
write.csv(dat.meta2018.cleaned, "dat.meta2018.cleaned.csv", row.names = FALSE)

# Merge into master file
master.met2018 <- subset(dat.meta2018.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments))

# Read 2019 data
dat.meta2019 <- read_csv("Rcapito_MetamorphLog_2019.csv")

# Populate 2019 year columns shown as N/A
dat.meta2019.cleaned <- dat.meta2019 %>%
  mutate(Year = "2019")

# Save the modified dataframe back to a CSV file
write.csv(dat.meta2019.cleaned, "dat.meta2019.cleaned.csv", row.names = FALSE)

# Merge into master file
master.met2019 <- subset(dat.meta2019.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments))

# Read 2021 data
dat.meta2021 <- read_csv("Rcapito_MetamorphLog_2021.csv")

# Populate 2021 year columns shown as N/A
dat.meta2021.cleaned <- dat.meta2021 %>%
  mutate(Year = "2021")

# Save the modified dataframe back to a CSV file
write.csv(dat.meta2021.cleaned, "dat.meta2021.cleaned.csv", row.names = FALSE)

# Merge into master file
master.met2021 <- subset(dat.meta2021.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments))

# Read 2022 data
dat.meta2022 <- read_csv("Rcapito_MetamorphLog_2022.csv")

# Populate 2022 year columns shown as N/A
dat.meta2022.cleaned <- dat.meta2022 %>%
  mutate(Year = "2022")

# Save the modified dataframe back to a CSV file
write.csv(dat.meta2022.cleaned, "dat.meta2022.cleaned.csv", row.names = FALSE)

# Merge into master file
master.met2022 <- subset(dat.meta2022.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments))

# Read and merge 2023 data 
dat.meta2023 <- read_csv("Rcapito_MetamorphLog_2023.csv")
master.met2023 <- subset(dat.meta2023, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments))

# Read and merge 2024 data
dat.meta2024 <- read_csv("Rcapito2024.cleaned.csv")
master.met2024 <- subset(dat.meta2024, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments))

# Create a master dataset with all subsetted data files
merged.Master <- rbind(master.met2009, master.met2013, master.met2015, master.met2016, master.met2017, master.met2018,
                       master.met2019, master.met2021, master.met2022, master.met2023, master.met2024)



# Separating Date, Time, and Year columns from one "Date and Time" column in 2024 dataset

# Read the CSV file
Rcapito2024 <- read_csv("Rcapito2024.csv")

# Convert the Date and Time column to a POSIXct object
Rcapito2024$`Date and Time` <- mdy_hms(Rcapito2024$`Date and Time`, tz = "EST")

# Extract Date, Time, and Year
Rcapito2024.cleaned <- Rcapito2024 %>%
  mutate(
    Date = as.Date(`Date and Time`),                 # Extract date in YYYY-MM-DD format
    Time = format(`Date and Time`, "%H:%M:%S"),      # Extract time in HH:MM:SS format
    Year = year(`Date and Time`)                     # Extract year
  )

# Print the result
print(Rcapito2024)

# Save to csv
write.csv(Rcapito2024.cleaned, "RCapito2024.cleaned.csv", row.names = FALSE)
## Notes for Vanessa:

## The survey123 used AM/PM time instead of military and we ran into parsing 
## issues. Line 34 fixed this by including the time zone and converting to military time


## TO DO
## finish merging datasets after changing date formats
## average mass per year, dates etc
## summaries up next- find 2012 data