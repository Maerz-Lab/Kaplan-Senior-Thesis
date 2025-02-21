## Primary Investigators: Vanessa Terrell and Morgan Kaplan
## Last Edited: 02/13/24
## Goal of this code: working to subset data from 2009-2024 and compile into a master datafile


# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)


# Import 2009-2011 data set
dat.meta20092011 <- read_csv("Rcapito_MetamorphLog_2009_2011_Final.csv")

# Use subset() function to create a new dataframe for 2009-2011 data
master.met20092011 <- subset(dat.meta20092011, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                          Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis)) 

# Import 2012 data set
dat.meta2012 <- read_csv("Rcapito_MetamorphLog_2012.csv")

# Use subset() function to create a new dataframe for 2012 data
master.met2012 <- subset(dat.meta2012, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis)) 

# Import 2013 data set
dat.meta2013 <- read_csv("Rcapito_MetamorphLog_2013.csv")

# Use subset() function to create new dataframe for 2013 data
master.met2013 <- subset(dat.meta2013, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis)) 

# Import and subset 2015 data
dat.meta2015 <- read_csv("Rcapito_MetamorphLog_2015.csv")
master.met2015 <- subset(dat.meta2015, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                         Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Import and subset 2016 data
dat.meta2016 <- read_csv("Rcapito_MetamorphLog_2016.csv")
master.met2016 <- subset(dat.meta2016, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Import and subset 2017 data
dat.meta2017 <- read_csv("Rcapito_MetamorphLog_2017.csv")
master.met2017 <- subset(dat.meta2017, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Read 2018 data
dat.meta2018 <- read_csv("Rcapito_MetamorphLog_2018.csv")

# Populate 2018 year columns shown as N/A
dat.meta2018.cleaned <- dat.meta2018 %>%
  mutate(Year = "2018")

# Save the modified dataframe back to a CSV file
write.csv(dat.meta2018.cleaned, "dat.meta2018.cleaned.csv", row.names = FALSE)

# Merge into master file
master.met2018 <- subset(dat.meta2018.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                         Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Read 2019 data
dat.meta2019 <- read_csv("Rcapito_MetamorphLog_2019.csv")

# Populate 2019 year columns shown as N/A
dat.meta2019.cleaned <- dat.meta2019 %>%
  mutate(Year = "2019")

# Save the modified dataframe back to a CSV file
write.csv(dat.meta2019.cleaned, "dat.meta2019.cleaned.csv", row.names = FALSE)

# Merge into master file
master.met2019 <- subset(dat.meta2019.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                          Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))
# Import and subset 2020 data
dat.meta2020 <- read_csv("Rcapito_MetamorphLog_2020.csv")
master.met2020 <- subset(dat.meta2020, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))
# Read 2021 data
dat.meta2021 <- read_csv("Rcapito_MetamorphLog_2021.csv")

# Populate 2021 year columns shown as N/A
dat.meta2021.cleaned <- dat.meta2021 %>%
  mutate(Year = "2021")

# Save the modified dataframe back to a CSV file
write.csv(dat.meta2021.cleaned, "dat.meta2021.cleaned.csv", row.names = FALSE)

# Merge into master file
master.met2021 <- subset(dat.meta2021.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                          Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Read 2022 data
dat.meta2022 <- read_csv("Rcapito_MetamorphLog_2022.csv")

# Populate 2022 year columns shown as N/A
dat.meta2022.cleaned <- dat.meta2022 %>%
  mutate(Year = "2022")

# Save the modified dataframe back to a CSV file
write.csv(dat.meta2022.cleaned, "dat.meta2022.cleaned.csv", row.names = FALSE)

# Merge into master file
master.met2022 <- subset(dat.meta2022.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                          Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Read and merge 2023 data 
dat.meta2023 <- read_csv("Rcapito_MetamorphLog_2023.csv")
master.met2023 <- subset(dat.meta2023, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Read and merge 2024 data
dat.meta2024 <- read_csv("Rcapito_MetamorphLog_2024.csv")
master.met2024 <- subset(dat.meta2024, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Create a master dataset with all subsetted data files
merged.Master.met <- rbind(master.met20092011, master.met2012, master.met2013, master.met2015, master.met2016, master.met2017, master.met2018,
                       master.met2019, master.met2021, master.met2022, master.met2023, master.met2024)

# Saving full merged master dataframe to source
write.csv(merged.Master.met, "merged.Master.met.csv", row.names = FALSE) 

# Read and merge 2009 weather data
dat.weather2009 <- read_csv ("Weather_2009.csv", skip = 6)
dat.weather2009 <- dat.weather2009 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2009 <- subset(dat.weather2009, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2010 weather data
dat.weather2010 <- read_csv ("Weather_2010.csv", skip = 6)
dat.weather2010 <- dat.weather2010 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2010 <- subset(dat.weather2010, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2011 weather data
dat.weather2011 <- read_csv ("Weather_2011.csv", skip = 6)
dat.weather2011 <- dat.weather2011 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2011 <- subset(dat.weather2011, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2012 weather data
dat.weather2012 <- read_csv ("Weather_2012.csv", skip = 6)
dat.weather2012 <- dat.weather2012 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2012 <- subset(dat.weather2012, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2013 weather data
dat.weather2013 <- read_csv ("Weather_2013.csv", skip = 6)
dat.weather2013 <- dat.weather2013 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2013 <- subset(dat.weather2013, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2015 weather data
dat.weather2015 <- read_csv ("Weather_2015.csv", skip = 6)
dat.weather2015 <- dat.weather2015 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2015 <- subset(dat.weather2015, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2016 weather data
dat.weather2016 <- read_csv ("Weather_2016.csv", skip = 6)
dat.weather2016 <- dat.weather2016 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2016 <- subset(dat.weather2016, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2017 weather data
dat.weather2017 <- read_csv ("Weather_2017.csv", skip = 6)
dat.weather2017 <- dat.weather2017 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2017 <- subset(dat.weather2017, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2018 weather data
dat.weather2018 <- read_csv ("Weather_2018.csv", skip = 6)
dat.weather2018 <- dat.weather2018 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2018 <- subset(dat.weather2018, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2019 weather data
dat.weather2019 <- read_csv ("Weather_2019.csv", skip = 6)
dat.weather2019 <- dat.weather2019 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2019 <- subset(dat.weather2019, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2020 weather data
dat.weather2020 <- read_csv ("Weather_2020.csv", skip = 6)
dat.weather2020 <- dat.weather2020 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2020 <- subset(dat.weather2020, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2021 weather data
dat.weather2021 <- read_csv ("Weather_2021.csv", skip = 6)
dat.weather2021 <- dat.weather2021 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2021 <- subset(dat.weather2021, select = c(year, yday, prcp, tmax, tmin, vp))

# Read and merge 2022 weather data
dat.weather2022 <- read_csv ("Weather_2022.csv", skip = 6)
dat.weather2022 <- dat.weather2022 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2022 <- subset(dat.weather2022, select = c(year, yday,  prcp, tmax, tmin, vp))

# Read and merge 2023 weather data
dat.weather2023 <- read_csv ("Weather_2023.csv", skip = 6)
dat.weather2023 <- dat.weather2023 %>%
  rename(tmax = 'tmax (deg c)', tmin = 'tmin (deg c)', prcp = 'prcp (mm/day)', vp = 'vp (Pa)')
master.weather2023 <- subset(dat.weather2023, select = c(year, yday,  prcp, tmax, tmin, vp))

# Create a master dataset with all subsetted weather data files
merged.Master.weather <- rbind(master.weather2009, master.weather2010, master.weather2011, master.weather2012,
                               master.weather2013, master.weather2015, master.weather2016, master.weather2017,
                               master.weather2018, master.weather2019, master.weather2020, master.weather2021,
                               master.weather2022, master.weather2023)

# Saving full merged master dataframe to source
write.csv(merged.Master.weather, "merged.Master.weather.csv", row.names = FALSE) 

# summary statistics for weather
library(dplyr)

weather_summary <- merged.Master.weather %>%
  mutate(temp_mean = (tmin + tmax) / 2) %>%  # Create average temperature column
  group_by(year) %>%
  summarise(
    # Temperature statistics (from temp_mean)
    temp_min = min(temp_mean, na.rm = TRUE),
    temp_median = median(temp_mean, na.rm = TRUE),
    temp_mean = mean(temp_mean, na.rm = TRUE),
    temp_max = max(temp_mean, na.rm = TRUE),
    
    # Precipitation statistics
    prcp_cumulative = sum(prcp, na.rm = TRUE),
    prcp_mean = mean(prcp, na.rm = TRUE),
    prcp_median = median(prcp, na.rm = TRUE),
    
    # Vapor pressure statistics
    vp_min = min(vp, na.rm = TRUE),
    vp_median = median(vp, na.rm = TRUE),
    vp_mean = mean(vp, na.rm = TRUE),
    vp_max = max(vp, na.rm = TRUE)
  )


library(ggplot2)

ggplot(weather_summary, aes(x = year, y = temp_mean)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Mean Temperature per Year",
    x = "Year",
    y = "Mean Temperature (°C)"
  ) +
  theme_minimal()


library(ggplot2)

ggplot(weather_summary) +
  # Mean temperature line and points (steelblue)
  geom_line(aes(x = year, y = temp_mean), color = "steelblue", size = 1) +
  geom_point(aes(x = year, y = temp_mean), color = "darkblue", size = 2) +
  
  # Max temperature line and points (red)
  geom_line(aes(x = year, y = temp_max), color = "red", size = 1) +
  geom_point(aes(x = year, y = temp_max), color = "red", size = 2) +
  
  # Min temperature line and points (black)
  geom_line(aes(x = year, y = temp_min), color = "black", size = 1) +
  geom_point(aes(x = year, y = temp_min), color = "black", size = 2) +
  
  labs(
    title = "Temperature Statistics per Year",
    x = "Year",
    y = "Temperature (°C)"
  ) +
  theme_minimal()


## how do i use left join function to pull weather summary into meta summary"
## TO DO


## summary statistics for avg days to metamorphosis per year bc longer metamorphosis is 
## typically correlated to lower survivorship and fitness
## min, mean, max size at metamorphosis per year
## graph for each year with points of different masses but also showed density of tanks (box plots??)
## ability to cite multiple research papers here
## look into Sara Diamond's senior thesis for density data
## doing a model for what predicts days to metamorphosis in tanks
## does it matter if we combine years where density is not the same?



##### Code that I used last semester but do not need currently #####

# Separating Date, Time, and Year columns from one "Date and Time" column in 2024 dataset
# Read the CSV file
# Rcapito2024 <- read_csv("Rcapito2024.csv")

# Convert the Date and Time column to a POSIXct object
#Rcapito2024$`Date and Time` <- mdy_hms(Rcapito2024$`Date and Time`, tz = "EST")

# Extract Date, Time, and Year
#Rcapito2024.cleaned <- Rcapito2024 %>%
#  mutate(
#    Date = as.Date(`Date and Time`),                 # Extract date in YYYY-MM-DD format
 #   Time = format(`Date and Time`, "%H:%M:%S"),      # Extract time in HH:MM:SS format
  #  Year = year(`Date and Time`)                     # Extract year
#  )

# Print the result
#print(Rcapito2024)

# Save to csv
#write.csv(Rcapito2024.cleaned, "RCapito2024.cleaned.csv", row.names = FALSE)

# Saving full merged master dataframe to source
#write.csv(merged.Master, "merged.Master.csv", row.names = FALSE) 

