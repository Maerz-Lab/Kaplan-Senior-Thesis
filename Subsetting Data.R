## Primary Investigators: Vanessa Terrell and Morgan Kaplan
## Last Edited: 02/21/25
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

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta20092011 <- dat.meta20092011 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta20092011 <- dat.meta20092011 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

# Import 2012 data set
dat.meta2012 <- read_csv("Rcapito_MetamorphLog_2012.csv")

# Use subset() function to create a new dataframe for 2012 data
master.met2012 <- subset(dat.meta2012, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis)) 
# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2012 <- dat.meta2012 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2012 <- dat.meta2012 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

# Import 2013 data set
dat.meta2013 <- read_csv("Rcapito_MetamorphLog_2013.csv")

# Use subset() function to create new dataframe for 2013 data
master.met2013 <- subset(dat.meta2013, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis)) 
# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2013 <- dat.meta2013 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2013 <- dat.meta2013 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

# Import and subset 2015 data
dat.meta2015 <- read_csv("Rcapito_MetamorphLog_2015.csv")

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2015 <- dat.meta2015 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2015 <- dat.meta2015 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

master.met2015 <- subset(dat.meta2015, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                         Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Import 2016 data
dat.meta2016 <- read_csv("Rcapito_MetamorphLog_2016.csv")

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2016 <- dat.meta2016 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2016 <- dat.meta2016 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

# Subset 2016 data
master.met2016 <- subset(dat.meta2016, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Import 2017 data
dat.meta2017 <- read_csv("Rcapito_MetamorphLog_2017.csv")

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2017 <- dat.meta2017 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2017 <- dat.meta2017 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

# Subset 2017 data
master.met2017 <- subset(dat.meta2017, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Read 2018 data
dat.meta2018 <- read_csv("Rcapito_MetamorphLog_2018.csv")

# Populate 2018 year columns shown as N/A
dat.meta2018.cleaned <- dat.meta2018 %>%
  mutate(Year = "2018")

# Save the modified dataframe back to a CSV file
write.csv(dat.meta2018.cleaned, "dat.meta2018.cleaned.csv", row.names = FALSE)

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2018.cleaned <- dat.meta2018.cleaned %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2018.cleaned <- dat.meta2018.cleaned %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

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

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2019.cleaned <- dat.meta2019.cleaned %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2019.cleaned <- dat.meta2019.cleaned %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

# Merge into master file
master.met2019 <- subset(dat.meta2019.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                          Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))
# Import 2020 data
dat.meta2020 <- read_csv("Rcapito_MetamorphLog_2020.csv")

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2020 <- dat.meta2020 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2020 <- dat.meta2020 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

# Subset and merge into master file
master.met2020 <- subset(dat.meta2020, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))
# Read 2021 data
dat.meta2021 <- read_csv("Rcapito_MetamorphLog_2021.csv")

# Populate 2021 year columns shown as N/A
dat.meta2021.cleaned <- dat.meta2021 %>%
  mutate(Year = "2021")

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2021.cleaned <- dat.meta2021.cleaned %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2021.cleaned <- dat.meta2021.cleaned %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

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

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2022.cleaned <- dat.meta2022.cleaned %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2022.cleaned <- dat.meta2022.cleaned %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

# Merge into master file
master.met2022 <- subset(dat.meta2022.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                          Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Read and merge 2023 data 
dat.meta2023 <- read_csv("Rcapito_MetamorphLog_2023.csv")

# Convert to Date format (if it's not already)
library(lubridate)

# Set all years to 2023
dat.meta2023$Year <- as.numeric(dat.meta2023$Year)
dat.meta2023$Date.metamorphosed <- as.Date(dat.meta2023$Date.metamorphosed, format = "%m/%d/%Y")
year(dat.meta2023$Date.metamorphosed) <- 2023
dat.meta2023$Date.metamorphosed <- as.Date(dat.meta2023$Date.metamorphosed)
write.csv(dat.meta2023, "dat.meta2023.csv", row.names = FALSE)


# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2023 <- dat.meta2023 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2023 <- dat.meta2023 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

master.met2023 <- subset(dat.meta2023, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis))

# Read and merge 2024 data
dat.meta2024 <- read_csv("Rcapito_MetamorphLog_2024.csv")

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2024 <- dat.meta2024 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2024 <- dat.meta2024 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

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
    # Temperature statistics
    temp_min = min(tmin, na.rm = TRUE),
    temp_median = median(temp_mean, na.rm = TRUE),
    temp_mean = mean(temp_mean, na.rm = TRUE),
    temp_max = max(tmax, na.rm = TRUE),  # Ensure max is calculated from tmax
    
    # Precipitation statistics
    prcp_cumulative = sum(prcp, na.rm = TRUE),  # Total precipitation for the year
    prcp_mean = mean(prcp, na.rm = TRUE),       # Average precipitation for the year
    prcp_median = median(prcp, na.rm = TRUE),   # Median precipitation for the year
    
    # Vapor pressure statistics
    vp_min = min(vp, na.rm = TRUE),  # Minimum vapor pressure
    vp_median = median(vp, na.rm = TRUE),  # Median vapor pressure
    vp_mean = mean(vp, na.rm = TRUE),  # Mean vapor pressure
    vp_max = max(vp, na.rm = TRUE)  # Maximum vapor pressure
  )
library (ggplot2)
# Graphing temp stats per year
ggplot(weather_summary, aes(x = year)) +
  # Plot temp_max first
  geom_line(aes(y = temp_max, color = "Max"), size = 1, na.rm = TRUE) +
  geom_point(aes(y = temp_max, color = "Max"), size = 2, na.rm = TRUE) +
  
  # Plot temp_min second
  geom_line(aes(y = temp_min, color = "Min"), size = 1, na.rm = TRUE) +
  geom_point(aes(y = temp_min, color = "Min"), size = 2, na.rm = TRUE) +
  
  # Plot temp_mean last
  geom_line(aes(y = temp_mean, color = "Mean"), size = 1, na.rm = TRUE) +
  geom_point(aes(y = temp_mean, color = "Mean"), size = 2, na.rm = TRUE) +
  
  # Manual color scale
  scale_color_manual(values = c("Mean" = "steelblue", "Max" = "red", "Min" = "black")) +
  
  labs(
    title = "Temperature Statistics per Year",
    x = "Year",
    y = "Temperature (°C)",
    color = "Temperature Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif")
  )


# Summarize Days.to.metamorphosis by year
met_summary <- merged.Master.met %>%
  group_by(Year) %>%
  summarise(
    # Handle cases where there are no valid data points
    days_min = ifelse(all(is.na(Days.to.metamorphosis)), NA, min(Days.to.metamorphosis, na.rm = TRUE)),
    days_median = ifelse(all(is.na(Days.to.metamorphosis)), NA, median(Days.to.metamorphosis, na.rm = TRUE)),
    days_mean = ifelse(all(is.na(Days.to.metamorphosis)), NA, mean(Days.to.metamorphosis, na.rm = TRUE)),
    days_max = ifelse(all(is.na(Days.to.metamorphosis)), NA, max(Days.to.metamorphosis, na.rm = TRUE))
  )


# Plot the summarized data
ggplot(met_summary, aes(x = Year)) +
  geom_line(aes(y = days_mean, color = "Mean"), size = 1) +
  geom_point(aes(y = days_mean, color = "Mean"), size = 2) +
  geom_line(aes(y = days_max, color = "Max"), size = 1) +
  geom_point(aes(y = days_max, color = "Max"), size = 2) +
  geom_line(aes(y = days_min, color = "Min"), size = 1) +
  geom_point(aes(y = days_min, color = "Min"), size = 2) +
  geom_line(aes(y = days_median, color = "Median"), size = 1) +
  geom_point(aes(y = days_median, color = "Median"), size = 2) +
  scale_color_manual(values = c("Mean" = "steelblue", "Max" = "red", "Min" = "black", "Median" = "green")) +
  labs(
    title = "Days to Metamorphosis per Year",
    x = "Year",
    y = "Days to Metamorphosis",
    color = "Statistic"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif")
  )


# Left join weather_summary into met_summary based on 'year'
# Change the column name 'year' to 'Year' in the weather_summary data frame
# Convert 'Year' in weather_summary to character
merged.Master.met$Year <- as.character(merged.Master.met$Year)
weather_summary$year <- as.character(weather_summary$year)
weather_summary <- weather_summary %>%
  rename(Year = year)
# Now perform the left join
# Left join weather_summary into merged.Master.met based on 'Year'
merged_with_weather <- left_join(merged.Master.met, weather_summary, by = "Year")
# Compute the average mean days to metamorphosis per year
merged_with_weather <- merged_with_weather %>%
  group_by(Year) %>%
  mutate(days_mean = mean(Days.to.metamorphosis, na.rm = TRUE)) %>%
  ungroup()  # Ungroup to remove the grouping after the calculation


merged_with_weather$Year <- as.numeric(merged_with_weather$Year)
merged_with_weather$Mass.g.metamorphosed <- as.numeric(merged_with_weather$Mass.g.metamorphosed)
merged_with_weather$Tank.ID <- as.numeric(merged_with_weather$Tank.ID)
merged_with_weather$Stocking.density <- as.numeric(merged_with_weather$Stocking.density)

# Plotting mean days to metamorphosis and mean temp per year
ggplot(merged_with_weather, aes(x = Year)) +
  geom_line(aes(y = temp_mean, color = "Mean Temp (°C)"), size = 1) +
  geom_point(aes(y = temp_mean, color = "Mean Temp (°C)"), size = 2) +
  geom_line(aes(y = days_mean, color = "Mean Days to Metamorphosis"), size = 1) +
  geom_point(aes(y = days_mean, color = "Mean Days to Metamorphosis"), size = 2) +
  scale_color_manual(values = c("Mean Temp (°C)" = "steelblue", 
                                "Mean Days to Metamorphosis" = "darkorange")) +
  labs(
    title = "Mean Days to Metamorphosis and Mean Temperature per Year",
    x = "Year",
    y = "Value",
    color = "Statistic"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif")
  )

colnames(merged_with_weather)


# Remove rows where either days_mean or temp_mean is NA
merged_with_weather <- merged_with_weather %>%
  filter(!is.na(days_mean) & !is.na(temp_mean))
# Remove rows with unreasonable values
merged_with_weather <- merged_with_weather %>%
  filter(temp_mean >= -50 & temp_mean <= 250,  # Adjust the range as needed
         days_mean > 0)

# Compute the average days to metamorphosis per year
merged_with_weather <- merged_with_weather %>%
  group_by(Year) %>%
  mutate(days_mean = mean(Days.to.metamorphosis, na.rm = TRUE)) %>%
  ungroup()  # Remove grouping after calculation

ggplot(merged_with_weather, aes(x = Year)) +
  geom_line(aes(y = temp_mean, color = "Mean Temp (°C)"), size = 1) +
  geom_point(aes(y = temp_mean, color = "Mean Temp (°C)"), size = 2) +
  geom_line(aes(y = days_mean, color = "Average Days to Metamorphosis"), size = 1) +
  geom_point(aes(y = days_mean, color = "Average Days to Metamorphosis"), size = 2) +
  scale_color_manual(values = c("Mean Temp (°C)" = "steelblue", 
                                "Average Days to Metamorphosis" = "darkorange")) +
  labs(
    title = "Mean Days to Metamorphosis and Mean Temperature per Year",
    x = "Year",
    y = "Value",
    color = "Statistic"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif")
  )

ggplot(merged_with_weather, aes(x = Year)) +
  geom_line(aes(y = temp_mean, color = "Mean Temp (°C)"), size = 1) +
  geom_point(aes(y = temp_mean, color = "Mean Temp (°C)"), size = 2) +
  geom_line(aes(y = days_mean, color = "Average Days to Metamorphosis"), size = 1) +
  geom_point(aes(y = days_mean, color = "Average Days to Metamorphosis"), size = 2) +
  scale_color_manual(values = c("Mean Temp (°C)" = "steelblue", 
                                "Average Days to Metamorphosis" = "darkorange")) +
  labs(
    title = "Mean Days to Metamorphosis and Mean Temperature per Year",
    x = "Year",
    y = "Value",
    color = "Statistic"
  ) +
  ylim(0, 150) +  # Limit y-axis values up to 150
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif")
  )

# LME model
library(lme4)

# Model for days to metamorphosis
days_model <- lmer(Days.to.metamorphosis ~ temp_min + temp_max + prcp_mean + Stocking.density + (1 | Year) + (1|Tank.ID), data = merged_with_weather)
summary(days_model)

library(sjPlot)

p1 <- plot_model(days_model, type = "pred", terms = c("temp_max", "prcp_mean[3.683709]", "Stocking.density[30,50,80,110]"))
p1 <- p1 +
  geom_point(data = merged_with_weather, aes(x=temp_max, y = Days.to.metamorphosis), shape=1, color="black", size=1)
p1

# Model for mass at metamorphosis
mass_model <- lmer(Mass.g.metamorphosed ~ temp_min + temp_max + prcp_mean + Stocking.density + (1 | Year) + (1|Tank.ID), data = merged_with_weather)
summary(mass_model)

plot_model(mass_model)

library(ggplot2)

plot1 <- ggplot(data = merged_with_weather, aes(x = temp_min, y = temp_max)) +
  geom_point()

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

