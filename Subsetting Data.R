## Primary Investigators: Vanessa Terrell and Morgan Kaplan
## Last Edited: 02/21/25
## Goal of this code: working to subset data from 2009-2024 and compile into a master datafile



# Load necessary libraries
library(readr)
library(dplyr)
library(lubridate)

             ###### IMPORT AND CLEAN META DATA ######
# Import 2009-2011 data set
dat.meta20092011 <- read_csv("Rcapito_MetamorphLog_2009_2011_Final.csv")

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta20092011 <- dat.meta20092011 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta20092011 <- dat.meta20092011 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

## Temp Surv.prop column
dat.meta20092011 <- dat.meta20092011 %>%
  mutate(Surv.prop = NA)

# Use subset() function to create a new dataframe for 2009-2011 data
library(dplyr)

master.met20092011 <- dat.meta20092011 %>%
  select(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
         Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)

# Import 2012 data set
dat.meta2012 <- read_csv("Rcapito_MetamorphLog_2012.csv")

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2012 <- dat.meta2012 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2012 <- dat.meta2012 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2012 %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),  # Use the stocking density per tank
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),  # Successfully metamorphosed
    Surv.prop = n_metamorphosed / n_stocked  # Survivorship ratio
  )

# Add survivorship to dat.meta2012
dat.meta2012 <- dat.meta2012 %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2012 data
master.met2012 <- subset(dat.meta2012, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)) 

# Import 2013 data set
dat.meta2013 <- read_csv("Rcapito_MetamorphLog_2013.csv")

# Convert Tank.ID to numeric
dat.meta2013 <- dat.meta2013 %>%
  mutate(Tank.ID = as.numeric(Tank.ID))

# Convert Stocking.density to numeric + convert invalid values to NA
dat.meta2013 <- dat.meta2013 %>%
  mutate(Stocking.density = suppressWarnings(as.numeric(Stocking.density)))

# Convert dates to Date format if the format is m/d/y
dat.meta2013 <- dat.meta2013 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),
    Date.metamorphosed = mdy(Date.metamorphosed)
  )

# Subtract date stocked from date metamorphosed to get days to metamorphosis
dat.meta2013 <- dat.meta2013 %>%
  mutate(
    Date.stocked = mdy(Date.stocked),  # Change to dmy() or ymd() if needed
    Date.metamorphosed = mdy(Date.metamorphosed)
  )
dat.meta2013 <- dat.meta2013 %>%
  mutate(Days.to.metamorphosis = as.numeric(Date.metamorphosed - Date.stocked))

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2013 %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),
    Surv.prop = ifelse(is.na(n_stocked) | n_stocked == 0, NA, n_metamorphosed / n_stocked)
  )


# Add survivorship to dat.meta2013
dat.meta2013 <- dat.meta2013 %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2013 data
master.met2013 <- subset(dat.meta2013, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)) 
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

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2015 %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),  # Use the stocking density per tank
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),  # Successfully metamorphosed
    Surv.prop = n_metamorphosed / n_stocked  # Survivorship ratio
  )

# Add survivorship to dat.meta2015
dat.meta2015 <- dat.meta2015 %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2015 data
master.met2015 <- subset(dat.meta2015, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)) 
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

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2016 %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),  # Use the stocking density per tank
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),  # Successfully metamorphosed
    Surv.prop = n_metamorphosed / n_stocked  # Survivorship ratio
  )

# Add survivorship to dat.meta2016
dat.meta2016 <- dat.meta2016 %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2012 data
master.met2016 <- subset(dat.meta2016, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)) 
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

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2017 %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),  # Use the stocking density per tank
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),  # Successfully metamorphosed
    Surv.prop = n_metamorphosed / n_stocked  # Survivorship ratio
  )

# Add survivorship to dat.meta2017
dat.meta2017 <- dat.meta2017 %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2017 data
master.met2017 <- subset(dat.meta2017, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)) 
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

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2018.cleaned %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),  # Use the stocking density per tank
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),  # Successfully metamorphosed
    Surv.prop = n_metamorphosed / n_stocked  # Survivorship ratio
  )

# Add survivorship to dat.meta2018
dat.meta2018.cleaned <- dat.meta2018.cleaned %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2018 data
master.met2018 <- subset(dat.meta2018.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)) 
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

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2019.cleaned %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),  # Use the stocking density per tank
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),  # Successfully metamorphosed
    Surv.prop = n_metamorphosed / n_stocked  # Survivorship ratio
  )

# Add survivorship to dat.meta2019.cleaned
dat.meta2019.cleaned <- dat.meta2019.cleaned %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2019 data
master.met2019 <- subset(dat.meta2019.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)) 
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

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2020 %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),  # Use the stocking density per tank
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),  # Successfully metamorphosed
    Surv.prop = n_metamorphosed / n_stocked  # Survivorship ratio
  )

# Add survivorship to dat.meta2020
dat.meta2020 <- dat.meta2020 %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2012 data
master.met2020 <- subset(dat.meta2020, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)) 
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

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2021.cleaned %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),  # Use the stocking density per tank
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),  # Successfully metamorphosed
    Surv.prop = n_metamorphosed / n_stocked  # Survivorship ratio
  )

# Add survivorship to dat.meta2021.cleaned
dat.meta2021.cleaned <- dat.meta2021.cleaned %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2012 data
master.met2021 <- subset(dat.meta2021.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop))

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

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2022.cleaned %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),  # Use the stocking density per tank
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),  # Successfully metamorphosed
    Surv.prop = n_metamorphosed / n_stocked  # Survivorship ratio
  )

# Add survivorship to dat.meta2022.cleaned
dat.meta2022.cleaned <- dat.meta2022.cleaned %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2012 data
master.met2022 <- subset(dat.meta2022.cleaned, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)) 
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

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2023 %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),  # Use the stocking density per tank
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),  # Successfully metamorphosed
    Surv.prop = n_metamorphosed / n_stocked  # Survivorship ratio
  )

# Add survivorship to dat.meta2023
dat.meta2023 <- dat.meta2023 %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2012 data
master.met2023 <- subset(dat.meta2023, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)) 
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

# Calculate survivorship proportion per tank
Surv.prop <- dat.meta2024 %>%
  group_by(Tank.ID) %>%
  summarise(
    n_stocked = first(Stocking.density),  # Use the stocking density per tank
    n_metamorphosed = sum(!is.na(Date.metamorphosed)),  # Successfully metamorphosed
    Surv.prop = n_metamorphosed / n_stocked  # Survivorship ratio
  )

# Add survivorship to dat.meta2024
dat.meta2024 <- dat.meta2024 %>%
  left_join(Surv.prop, by = "Tank.ID")

# Use subset() function to create a new dataframe for 2024 data
master.met2024 <- subset(dat.meta2024, select = c(Year, Date.metamorphosed, Mass.g.metamorphosed, Fate.comments, Clutch.ID,
                                                  Date.stocked, Date.eggs.hatched, Stocking.density, Tank.ID, Days.to.metamorphosis, Surv.prop)) 

# Create a master dataset with all subsetted data files 
merged.Master.met <- rbind(master.met20092011, master.met2012, master.met2013, master.met2015, master.met2016, master.met2017, master.met2018,
                       master.met2019, master.met2021, master.met2022, master.met2023, master.met2024)

# Saving full merged master dataframe to source
write.csv(merged.Master.met, "merged.Master.met.csv", row.names = FALSE) 

                      ###### SURVIVORSHIP CODE ######


weather_summary$std_temp_min <- scale(weather_summary$temp_min, center = TRUE, scale = TRUE)
weather_summary$std_temp_median <- scale(weather_summary$temp_median, center = TRUE, scale = TRUE)
weather_summary$std_temp_max <- scale(weather_summary$temp_max, center = TRUE, scale = TRUE)
weather_summary$std_temp_mean <- scale(weather_summary$temp_mean, center = TRUE, scale = TRUE)
weather_summary$std_prcp_cumulative <- scale(weather_summary$prcp_cumulative, center = TRUE, scale = TRUE)
weather_summary$std_prcp_mean <- scale(weather_summary$prcp_mean, center = TRUE, scale = TRUE)
weather_summary$std_prcp_median <- scale(weather_summary$prcp_median, center = TRUE, scale = TRUE)
weather_summary$std_vp_min <- scale(weather_summary$vp_min, center = TRUE, scale = TRUE)
weather_summary$std_vp_median <- scale(weather_summary$vp_median, center = TRUE, scale = TRUE)
weather_summary$std_vp_mean <- scale(weather_summary$vp_mean, center = TRUE, scale = TRUE)
weather_summary$std_vp_max <- scale(weather_summary$vp_max, center = TRUE, scale = TRUE)



library(dplyr)


# Create a new dataframe with Tank.ID count by Year
Ind.per.tank <- merged.Master.met %>%
  group_by(Year, Tank.ID) %>%
  summarise(Ind.per.tank = n(), .groups = 'drop')

# Join Stocking.density to the tank_counts_by_year dataframe
Ind.per.tank <- Ind.per.tank %>%
  left_join(merged.Master.met %>%
              select(Year, Tank.ID, Stocking.density) %>%
              distinct(), 
            by = c("Year", "Tank.ID"))

# View the resulting dataframe
head(Ind.per.tank)

Ind.per.tank$No.dead <- Ind.per.tank$Stocking.density - Ind.per.tank$Ind.per.tank

merged.Master.met$Fate <- 1

all.tadpoles.livedead <- merged_with_weather

library(tidyr)

# Ensure No.dead has valid values
Ind.per.tank$No.dead <- ifelse(Ind.per.tank$Stocking.density - Ind.per.tank$Ind.per.tank >= 0,
                               Ind.per.tank$Stocking.density - Ind.per.tank$Ind.per.tank, 
                               NA)

library(dplyr)
library(tidyr)

# Step 1: Ensure No.dead is not NA or negative
dead_individuals <- Ind.per.tank %>%
  filter(!is.na(No.dead) & No.dead >= 0) %>%  # Keep only rows with valid No.dead
  uncount(No.dead) %>%  # Replicates each row No.dead times
  select(Year, Tank.ID) %>%
  mutate(Fate = 0)  # Assign Fate = 0 for dead individuals

# View the first few rows of the data after removing dead_individuals
head(dead_individuals)



# Perform the left join and select the desired covariates
library(dplyr)

# Ensure that 'merged_with_weather_clean' has unique rows based on 'Year' and 'Tank.ID'
merged.master.met_unique <- merged.Master.met %>%
  distinct(Year, Tank.ID, .keep_all = TRUE)

# Perform the left join to add the values from 'merged_with_weather_clean' to 'dead_individuals'
dead_individuals <- dead_individuals %>%
  left_join(merged.master.met_unique %>%
              select(Year, Tank.ID, Clutch.ID, Date.stocked, Stocking.density, 
                     temp_min, temp_median, temp_max, temp_mean, 
                     prcp_cumulative, prcp_mean, prcp_median, 
                     vp_min, vp_max, vp_median, vp_mean, std_temp_min, std_temp_median, std_temp_max, std_temp_mean, 
                     std_prcp_cumulative, std_prcp_mean, std_prcp_median, 
                     std_vp_min, std_vp_max, std_vp_median, std_vp_mean),
            by = c("Year" = "Year", "Tank.ID" = "Tank.ID"))


# Step 1: Add the 'Fate' column to 'merged.Master.met' with a value of 1
merged.Master.met <- merged.Master.met %>%
  mutate(Fate = 1)

dead_individuals <- dead_individuals %>%
  mutate(Fate = 0)

dead_individuals <- dead_individuals %>%
  mutate(Date.metamorphosed = "NA")

dead_individuals <- dead_individuals %>%
  mutate(Mass.g.metamorphosed = "NA")

# Step 3: Ensure column order is the same for both dataframes
column_order <- c("Year", "Tank.ID", "Clutch.ID", "Date.stocked", "Stocking.density", 
                  "temp_min", "temp_median", "temp_max", "temp_mean", 
                  "prcp_cumulative", "prcp_mean", "prcp_median", 
                  "vp_min", "vp_max", "vp_median", "vp_mean", "std_temp_min", "std_temp_median", "std_temp_max", "std_temp_mean", 
                  "std_prcp_cumulative", "std_prcp_mean", "std_prcp_median", 
                  "std_vp_min", "std_vp_max", "std_vp_median", "std_vp_mean", 
                  "Date.metamorphosed", "Mass.g.metamorphosed", "Fate")

merged.Master.met <- merged.Master.met %>%
  select(all_of(column_order))

dead_individuals <- dead_individuals %>%
  select(all_of(column_order))

# Convert date columns to Date class in both data frames
merged.Master.met$Date.stocked <- as.Date(merged.Master.met$Date.stocked, format = "%Y-%m-%d")  # Adjust format as needed
merged.Master.met$Date.metamorphosed <- as.Date(merged.Master.met$Date.metamorphosed, format = "%Y-%m-%d")  # Adjust format as needed

dead_individuals$Date.stocked <- as.Date(dead_individuals$Date.stocked, format = "%Y-%m-%d")  # Adjust format as needed
dead_individuals$Date.metamorphosed <- as.Date(dead_individuals$Date.metamorphosed, format = "%Y-%m-%d")  # Adjust format as needed

# Now combine the two dataframes
all.tadpoles <- rbind(merged.Master.met, dead_individuals)

all.tadpoles$std_Stocking.density <- scale(all.tadpoles$Stocking.density, center = TRUE, scale = TRUE)

# Step 5: Check the result
nrow(all.tadpoles)  # Should now have the correct number of rows
head(all.tadpoles)  # Check the first few rows






                    ##### WEATHER DATA ADDITION + MERGING ######

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

                      ####### ANALYZE WEATHER DATA #######

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

            ####### GRAPHING WEATHER DATA ########

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
    color = "Temperature Type") +
  theme_minimal() + theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"))

# Summarize Days.to.metamorphosis by year
met_summary <- merged.Master.met %>%
  group_by(Year) %>%
  summarise(
    # Handle cases where there are no valid data points
    days_min = ifelse(all(is.na(Days.to.metamorphosis)), NA, min(Days.to.metamorphosis, na.rm = TRUE)),
    days_median = ifelse(all(is.na(Days.to.metamorphosis)), NA, median(Days.to.metamorphosis, na.rm = TRUE)),
    days_mean = ifelse(all(is.na(Days.to.metamorphosis)), NA, mean(Days.to.metamorphosis, na.rm = TRUE)),
    days_max = ifelse(all(is.na(Days.to.metamorphosis)), NA, max(Days.to.metamorphosis, na.rm = TRUE)))

# Summarized days to metamorphosis per year graph
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
    color = "Statistic") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"))

# Change variable types to characters
merged.Master.met$Year <- as.character(merged.Master.met$Year)
weather_summary$year <- as.character(weather_summary$year)
weather_summary <- weather_summary %>%
  rename(Year = year)

# Left join weather_summary into merged.Master.met based on 'Year'
merged.Master.met <- left_join(merged.Master.met, weather_summary, by = "Year")

# Compute the average mean days to metamorphosis per year
merged_with_weather <- merged_with_weather %>%
  group_by(Year) %>%
  mutate(days_mean = mean(Days.to.metamorphosis, na.rm = TRUE)) %>%
  ungroup() 

# Change variables to numeric
merged_with_weather$Year <- as.numeric(merged_with_weather$Year)
merged_with_weather$Mass.g.metamorphosed <- as.numeric(merged_with_weather$Mass.g.metamorphosed)
merged_with_weather$Tank.ID <- as.numeric(merged_with_weather$Tank.ID)
merged_with_weather$Stocking.density <- as.numeric(merged_with_weather$Stocking.density)

# GRAPH - mean days to metamorphosis and mean temp per year
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
    color = "Statistic") + theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, family = "serif"),
    text = element_text(family = "serif"))
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
  ungroup() 

# GRAPH - Mean Temp vs Mean Days to Meta per Year
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
                  ####### MODELS FOR VARIABLE COMPARISON ########
# Load libraries
library(lme4)
library(dplyr)
library(ggplot2)
library(sjPlot)

# Standardize variables of temp max, temp min, prcp mean, and stocking density
merged_with_weather <- merged_with_weather %>%
  mutate(
    Stocking.density.std = (Stocking.density - mean(Stocking.density, na.rm = TRUE)) / sd(Stocking.density, na.rm = TRUE),
    temp_max.std = (temp_max - mean(temp_max, na.rm = TRUE)) / sd(temp_max, na.rm = TRUE),
    temp_min.std = (temp_min - mean(temp_min, na.rm = TRUE)) / sd(temp_min, na.rm = TRUE),
    prcp_mean.std = (prcp_mean - mean(prcp_mean, na.rm = TRUE)) / sd(prcp_mean, na.rm = TRUE))

# Remove NA values from days and mass
merged_with_weather_clean <- merged_with_weather %>%
  filter(!is.na(Days.to.metamorphosis), !is.na(Mass.g.metamorphosed))


# MODEL - Estimated effects of standardized variables on days to meta
days_model <- lmer(Days.to.metamorphosis ~ temp_min.std + temp_max.std + prcp_mean.std + Stocking.density.std + (1 | Year) + (1|Tank.ID), data = merged_with_weather_clean)
plot_model(days_model, 
           title = "Effects of Weather and Stocking Density on Days to Metamorphosis") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
summary(days_model)

plot_model(days_model, type = "pred", c("std_Stocking.density"))

# MODEL - Estimated effects of standardized variables on mass at meta
mass_model <- lmer(Mass.g.metamorphosed ~ temp_min.std + temp_max.std + prcp_mean.std + Stocking.density.std + (1 | Year) + (1|Tank.ID), data = merged_with_weather_clean)
plot_model(mass_model, 
           title = "Effects of Weather and Stocking Density on Mass at Metamorphosis") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

plot_model(mass_model, type = "pred", c("std_Stocking.density"))





fate_model <- glmer(Fate ~ std_temp_min + std_temp_max + std_prcp_mean+ std_Stocking.density + 
                      (1 | Year) + (1 | Tank.ID), 
                    data = all.tadpoles, 
                    family = binomial)

plot_model(fate_model)

plot_model(fate_model, type = "pred", c("std_Stocking.density"))



                    ##### CONFIDENCE INTERVAL PLOTS #######

# Install packages
install.packages("broom.mixed")
library(broom.mixed)
library(tidyr)
library(ggplot2)

# GRAPH - Regression Coefficients with 95% CI for days to meta
model_coefs <- tidy(days_model, conf.int = TRUE)
ggplot(model_coefs, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Regression Coefficients with 95% CI", 
       x = "Predictors", y = "Estimate")

# Calculate R² for the mass model
install.packages("MuMIn")
library(MuMIn)
library(performance)
r2(days_model)

# GRAPH - Regression Coefficients with 95% CI for mass
model_coefs <- tidy(mass_model, conf.int = TRUE)
ggplot(model_coefs, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_pointrange() +
  coord_flip() +
  theme_minimal() +
  labs(title = "Regression Coefficients with 95% CI", 
       x = "Predictors", y = "Estimate")

# Calculate R² for the mass model
install.packages("MuMIn")
library(MuMIn)
library(performance)
r2(mass_model)

# Find P-values for model
install.packages("lmerTest")  # Install lmerTest if not already installed
library(lmerTest)

            ####### MODELS FOR PREDICTING THE EFFECT OF VARIABLES ON DAYS TO META AND MASS ##########

# GRAPH - Effects of weather and SD on days to metamorphosis
p1 <- plot_model(days_model, type = "pred", terms = c("temp_max.std", "prcp_mean.std[3.683709]", "Stocking.density.std[-2, 0, 2]"))
p1 <- p1 +
  geom_point(data = merged_with_weather_clean, aes(x = temp_max.std, y = Days.to.metamorphosis), shape = 1, color = "black", size = 1)
p1

# GRAPH - variation of above graph showing on one graph with different lines for SD
# Use a fixed value for temp_min.std (e.g., the mean of temp_min.std)
mean_temp_min <- mean(merged_with_weather_clean$temp_min.std, na.rm = TRUE)

# Generate predictions manually for the plot
predict_data <- expand.grid(
  temp_min.std = mean_temp_min,  # Use the mean of temp_min.std
  temp_max.std = seq(min(merged_with_weather_clean$temp_max.std), 
                     max(merged_with_weather_clean$temp_max.std), length.out = 100),
  prcp_mean.std = 3.683709,  # Set the value of prcp_mean.std
  Stocking.density.std = c(-2, 0, 2)  # Different levels of Stocking.density.std
)

# Generate predicted Days.to.metamorphosis values from the model, ignoring random effects
predict_data$Days.to.metamorphosis <- predict(days_model, newdata = predict_data, re.form = ~0)

# Plot the predictions with different color lines for Stocking.density.std
p1 <- ggplot(predict_data, aes(x = temp_max.std, y = Days.to.metamorphosis, color = factor(Stocking.density.std))) +
  geom_line(size = 1) +  # Draw lines for each level of Stocking.density.std
  scale_color_manual(values = c("red", "blue", "green")) +  # Customize the colors if needed
  geom_point(data = merged_with_weather_clean, aes(x = temp_max.std, y = Days.to.metamorphosis), 
             shape = 1, color = "black", size = 1) +  # Scatter plot points
  labs(color = "Stocking Density") +  # Add a legend for color
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("Effects of Weather and Stocking Density on Days to Metamorphosis")

# Print the plot
print(p1)


 ## SAME CODE, ADDED CONFIDENCE INTERVALS ##
# Use a fixed value for temp_min.std (e.g., the mean of temp_min.std)
mean_temp_min <- mean(merged_with_weather_clean$temp_min.std, na.rm = TRUE)

# Generate predictions manually for the plot
predict_data <- expand.grid(
  temp_min.std = mean_temp_min,  # Use the mean of temp_min.std
  temp_max.std = seq(min(merged_with_weather_clean$temp_max.std), 
                     max(merged_with_weather_clean$temp_max.std), length.out = 100),
  prcp_mean.std = 3.683709,  # Set the value of prcp_mean.std
  Stocking.density.std = c(-2, 0, 2)  # Different levels of Stocking.density.std
)

# Generate predicted values and standard errors, ignoring random effects
predictions <- predict(days_model, newdata = predict_data, re.form = ~0, se.fit = TRUE)

# Add the fitted values and standard errors to the prediction data
predict_data$Days.to.metamorphosis <- predictions$fit
predict_data$se <- predictions$se.fit

# Calculate the confidence intervals (95% by default)
predict_data$CI_lower <- predict_data$Days.to.metamorphosis - 1.96 * predict_data$se
predict_data$CI_upper <- predict_data$Days.to.metamorphosis + 1.96 * predict_data$se

# Plot the predictions with confidence intervals
p1 <- ggplot(predict_data, aes(x = temp_max.std, y = Days.to.metamorphosis, color = factor(Stocking.density.std))) +
  geom_line(size = 1) +  # Draw lines for each level of Stocking.density.std
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper, fill = factor(Stocking.density.std)), alpha = 0.2) +  # Add confidence intervals as shaded areas
  scale_color_manual(values = c("red", "blue", "green")) +  # Customize the colors if needed
  scale_fill_manual(values = c("red", "blue", "green")) +  # Same colors for the ribbons
  geom_point(data = merged_with_weather_clean, aes(x = temp_max.std, y = Days.to.metamorphosis), 
             shape = 1, color = "black", size = 1) +  # Scatter plot points
  labs(color = "Stocking Density", fill = "Stocking Density") +  # Add a legend for color and fill
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  ggtitle("Effects of Weather and Stocking Density on Days to Metamorphosis")

# Print the plot
print(p1)





## Predict effects on mass at metamorphosis
p2 <- plot_model(mass_model, type = "pred", terms = c("temp_max.std", "prcp_mean.std[3.683709]", "Stocking.density.std[-2, 0, 2]"))
p2 <- p2 +
  geom_point(data = merged_with_weather_clean, aes(x = temp_max.std, y = Mass.g.metamorphosed), shape = 1, color = "black", size = 1)
p2


 ##  Plots for Stocking Density Ranges vs Days to Meta ##

# 30, 50, 70, 100 #
# Create density range categories
subset_density_days <- merged_with_weather %>%
  mutate(density_range = case_when(
    Stocking.density >= 0 & Stocking.density <= 30 ~ "0-30",
    Stocking.density >= 31 & Stocking.density <= 60 ~ "31-60",
    Stocking.density >= 61 & Stocking.density <= 90 ~ "61-90",
    Stocking.density >= 91 ~ "91+"
  ))
# Summary statistics by density range
summary_stats <- subset_density_days %>%
  group_by(density_range) %>%
  summarize(
    min_days = min(Days.to.metamorphosis, na.rm = TRUE),
    max_days = max(Days.to.metamorphosis, na.rm = TRUE),
    avg_days = mean(Days.to.metamorphosis, na.rm = TRUE)
  ) %>%
  arrange(density_range)
print(summary_stats)

ggplot(subset_density_days, aes(x = density_range, y = Days.to.metamorphosis)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(
    title = "Days to Metamorphosis by Stocking Density Range",
    x = "Stocking Density Range",
    y = "Days to Metamorphosis"
  ) +
  theme_minimal()

  ## Plot for SD ranges vs Mass at meta ##
subset_density_mass <- merged_with_weather %>%
  mutate(density_range = case_when(
    Stocking.density >= 0 & Stocking.density <= 30 ~ "0-30",
    Stocking.density >= 31 & Stocking.density <= 60 ~ "31-60",
    Stocking.density >= 61 & Stocking.density <= 90 ~ "61-90",
    Stocking.density >= 91 ~ "91+"
  ))
# Summary statistics by density range
summary_stats <- subset_density_mass %>%
  group_by(density_range) %>%
  summarize(
    min_mass = min(Mass.g.metamorphosed, na.rm = TRUE),
    max_mass = max(Mass.g.metamorphosed, na.rm = TRUE),
    avg_mass = mean(Mass.g.metamorphosed, na.rm = TRUE)
  ) %>%
  arrange(density_range)
print(summary_stats)

ggplot(subset_density_days, aes(x = density_range, y = Mass.g.metamorphosed)) +
  geom_boxplot(fill = "pink", color = "black") +
  labs(
    title = "Mass at Metamorphosis by Stocking Density Range",
    x = "Stocking Density Range",
    y = "Mass at Metamorphosis (g)"
  ) +
  theme_minimal()

plot1 <- ggplot(data = merged_with_weather, aes(x = temp_min, y = temp_max)) +
  geom_point()




## SD vs Survivorship
library(ggplot2)
# Create a scatter plot using Merged.master.met dataset
ggplot(merged.Master.met, aes(x = Stocking.density, y = Surv.prop)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a regression line
  labs(
    title = "Survivorship vs. Stocking Density",
    x = "Stocking Density",
    y = "Survivorship Proportion"
  ) +
  theme_minimal()










sum(is.na(merged.Master.met$Surv.prop))  # Count NAs in Surv.prop





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

