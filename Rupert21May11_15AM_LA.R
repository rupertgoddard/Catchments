# Load necessary libraries
install.packages("lubridate")
install.packages("zoo")
install.packages("dplyr")
install.packages("ggplot2")

library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(tidyverse)

setwd("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/R")

HeliganRain <- read.csv("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/Mevagissey/Heligan-Gardens-rainfall-15min-Qualified26Nov18to26Nov23.csv")
HeliganRain <- read.csv("Heligan-Gardens-rainfall-15min-Qualified26Nov18to26Nov23.csv")
head(HeliganRain)
str(HeliganRain)
sum(is.na(HeliganRain$timeseries))
sum(is.na(HeliganRain$value.mm.))

# Ensure the timestamp column is in datetime format
HeliganRain$dateTime <- gsub("T", " ",HeliganRain$dateTime)
HeliganRain$timeseries <- as.POSIXct(HeliganRain$dateTime, format="%Y-%m-%d %H:%M:%S")
HeliganRain <- HeliganRain %>% rename(Rainfall_mm = value.mm.)
HeliganRain <- HeliganRain[!is.na(HeliganRain$timeseries), ]
# Create a complete sequence of timestamps # FAILS AT FULL_INDEX
full_index <- seq(from = min(HeliganRain$timeseries), 
                  to = max(HeliganRain$timeseries), 
                  by = "15 min")
str(HeliganRain)
str(full_index)

sum(is.na(HeliganRain$timeseries)) # Check for NAs
sum(is.na(full_index)) # Check for NAs
sum(is.na(HeliganRain$value.mm.)) # Check for NAs

head(HeliganRain$timeseries)
head(full_index)

# Merge with the full index to identify missing timestamps - Have I got this
#the right way round?
df_full <- data.frame(timeseries = full_index) %>%
  left_join(HeliganRain, df_full, by = "timeseries")
str(df_full)
##EVERYTHING WORKING UPTO HERE.
#-------------------------------------------------------------------------------
#sum(is.na(df_full$Rainfall_mm))
#write.csv(df_full, "C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/R/df_full.csv")

#df_full_corrected <- read.csv ("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/R/df_full_corrected.csv")
# I have taken df_full and filled the NA date column from the full list to 
#produce df_full_corrected.csv. We now need to fill the missing rainfall by mean inputation.

#-------------------------------------------------------------------------------
#Asked ChatGPT 'In R can you interpolate missing values using the mean 
#of values associated with multiple time series data in previous years?
#Returned the following code which works....Although looking at the .csv, 
#there are still 13 NAs in $value!

# Load necessary libraries
install.packages("dplyr")
install.packages("lubridate")
install.packages("zoo")

library(dplyr)
library(lubridate)
library(zoo)

# Create sample data frame with a time series and some missing values
set.seed(42)
date_seq <- seq(from = as.POSIXct("2020-01-01 00:00"), to = as.POSIXct("2023-01-01 00:00"), by = "15 min")
values <- sin(seq_along(date_seq) * pi / (24 * 4)) + rnorm(length(date_seq), sd = 0.2)
values[sample(1:length(values), 5000)] <- NA  # Introduce some NAs randomly

df <- data.frame(
  dateTime = date_seq,
  value = values
)
head(df,20)
tail(df,20)
# Extract the year, month, day, and hour components
df <- df %>%
  mutate(
    year = year(dateTime),
    month = month(dateTime),
    day = day(dateTime),
    hour = hour(dateTime),
    minute = minute(dateTime)
  )

# Calculate the mean value for each (month, day, hour, minute) combination
mean_values <- df %>%
  group_by(month, day, hour, minute) %>%
  summarize(mean_value = mean(value, na.rm = TRUE), .groups = 'drop')

# Merge the mean values with the original data frame
## LA edit - you don't need the pipe here you can just use the left_join function,
## I gave the dataframe a new name so you can check against the old one just in case
## this was not what you wanted
df_new <- left_join(df, mean_values, by = c("month", "day", "hour", "minute"))

# Fill in the missing values with the mean values from previous years
df_new <- df_new %>%
  mutate(value = ifelse(is.na(value), mean_value, value)) %>%
  select(dateTime, value)  # Optionally, remove the additional columns

# Display the first few rows of the data frame after filling missing values
head(df_new,20)
tail(df_new,20)
#===============================================================================
#Code copied below and substituted with my data
# Create sample data frame with a time series and some missing values
#df_full_correctv2 <- read.csv ("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/R/df_full_correctv2.csv")
date_seq <- df_full$timeseries
values <- df_full$Rainfall_mm
length(df_full$timeseries)
length(df_full$Rainfall_mm)

df <- data.frame(
  timeseries = date_seq,
  Rainfall_mm = values
)

length(date_seq)
length(values)

head(df,20)
tail(df,20)

sum(is.na(df_full$Rainfall_mm)) # Check for NAs
sum(is.na(df_full$timeseries))
# Extract the year, month, day, and hour components
df <- df %>%
  mutate(
    year = year(timeseries),
    month = month(timeseries),
    day = day(timeseries),
    hour = hour(timeseries),
    minute = minute(timeseries)
  )

# Calculate the mean value for each (month, day, hour, minute) combination
mean_values <- df %>%
  group_by(month, day, hour, minute) %>%
  summarize(mean_value = mean(Rainfall_mm, na.rm = TRUE), .groups = 'drop')

# Merge the mean values with the original data frame
df <- df %>%
  left_join(mean_values, by = c("month", "day", "hour", "minute"))

# Fill in the missing values with the mean values from previous years
df <- df %>%
  mutate(value = ifelse(is.na(Rainfall_mm), mean_value, Rainfall_mm)) %>%
  select(timeseries, Rainfall_mm)  # Optionally, remove the additional columns

# Display the first few rows of the data frame after filling missing values
head(df,20)
tail(df,20)

write.csv(df, "C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/R/df.csv")

#Looking at the .csv, I still have NAs for both date and Rainfall_mm.
#I think I need the modelled mean values of rainfall so i can statistically
#show that they are a 'fitting' substitute for NA values.

#===============================================================================