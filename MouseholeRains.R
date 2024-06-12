setwd("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/R")
#install.packages("ggplot2")
library (ggplot2)
library(dplyr)
library(scales)


#NEED TO LOOK AT DATE FORMATTING

precip.mousehole <- read.csv("C:/Users/rgoddard2/OneDrive - University of Plymouth/Resilient catchments/Mousehole/Trengwainton-rainfall-15min-Qualified.csv")
head(precip.mousehole)
tail(precip.mousehole)
str(precip.mousehole)
names(precip.mousehole)[names(precip.mousehole)=="value"] <- "Rainfall_mm"
hist(precip.mousehole$Rainfall_mm)
new_df <- subset(precip.mousehole, select = -qcode)
  #creates dataframe without the qcode empty column.
str(new_df)
sum(is.na(new_df$Rainfall_mm)) # Returns the number of 'NA' in the rainfall_mm column

# aggregate the Precipitation (PRECIP) data by DATE
precip.mousehole_daily <-aggregate(new_df$Rainfall_mm,   # data to aggregate
                                   by=list(new_df$date),  # variable to aggregate by
                                   FUN=sum,   # take the sum (total) of the precip
                                   na.rm=TRUE)  # if there are NA values ignore them
# if this is FALSE any NA value will prevent a value be totalled


head(precip.mousehole_daily)
# rename the columns
names(precip.mousehole_daily)[names(precip.mousehole_daily)=="Group.1"] <- "Date"
names(precip.mousehole_daily)[names(precip.mousehole_daily)=="x"] <- "DailyRainfall_mm"
str(precip.mousehole_daily)

range(precip.mousehole_daily$Date) # Returns the date range of the data
range(precip.mousehole_daily$DailyRainfall_mm, na.rm = TRUE) # Returns the range of rainfall data


precip.mousehole_daily$Date <- as.Date(precip.mousehole_daily$Date, format = "%d/%m/%Y") #changes to Date format Y-m-d
str(precip.mousehole_daily)
#precip.mousehole_daily$Date <- strftime (precip.mousehole_daily$Date,format = "%d/%m/%Y")
# changes the date format from 2018-02-10 to 10/02/2018
str(precip.mousehole_daily)






# The following returns NA
#rainfall_mean <- mean(precip.mousehole$Rainfall_mm)
#rainfall_sd <- sd(precip.mousehole$Rainfall_mm)
#print(paste("15 min_Mean rainfall(mm):", rainfall_mean))
#print(paste("Standard deviation (mm):", rainfall_sd))

rainfall_daily_mean <- mean(precip.mousehole_daily$DailyRainfall_mm)
rainfall_daily_sd <- sd(precip.mousehole_daily$DailyRainfall_mm)
rainfall_daily_median <- median(precip.mousehole_daily$DailyRainfall_mm)
rainfall_daily_mean <- round(rainfall_daily_mean,3)
rainfall_daily_sd <- round(rainfall_daily_sd,3)
rainfall_daily_median <- round(rainfall_daily_median,3)
print(paste("Daily Mean rainfall(mm):", rainfall_daily_mean))
print(paste("Standard deviation (mm):", rainfall_daily_sd))
print(paste("Daily median rinfall(mm):", rainfall_daily_median))

# Convert dates to Date class (optional but recommended)
date_as_date <- as.Date(precip.mousehole$date)

#precip.mousehole_daily$date_as_date <- strftime (date_as_date,format = "%d/%m/%Y")
# Generate random data to use as discharge
random_data <- runif(nrow(precip.mousehole_daily), min = 0.0, max = 40)
random_data <- round(random_data,3)
# Add the random data and formated date as a new columns to the existing dataset
precip.mousehole_daily$madeupdischarge <- random_data
precip.mousehole_daily$date_as_date <- date_as_date

##Upto here works 

precip.mousehole_daily

# plot daily aggregated hourly data ##NOT WORKING - NEEDS CORRECTING
#precPlot_daily <- ggplot(data=precip.mousehole_daily,  # the data frame
                        # aes(x = date_as_date, y = DailyRainfall_mm)) +   # the variables of interest
  
  #geom_bar(stat="identity") +   # create a bar graph
  #xlab("Date") + ylab("Precipitation (mm)") +  # label the x & y axes
  #ggtitle("Daily Precipitation - Trengwainton gauge 10 Feb 2018 to 10 Feb 2023") + # add a title
  #geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +  # add vertical axis line
  #geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5)    # add horizontal axis line
 #scale_x_date(breaks = "12 month")
  
#precPlot_daily



##ATTEMPT TO PLOT DISCHARGE AND PRECIP ON THE SAME CHART  -from ChatGPT
# Assuming Mousehole.rain is your dataset with columns Rainfall_mm and River_Discharge
# Assuming both columns have corresponding timestamps





# Add the random data and formated date as a new columns to the existing dataset
precip.mousehole$madeupdischarge <- random_data
precip.mousehole$date_as_date <- date_as_date
str (precip.mousehole)
# Create a ggplot object
ggplot(data = precip.mousehole, aes(x = date_as_date)) +
  
  # Add rainfall data as a line plot
  geom_line(aes(y = Rainfall_mm, color = "Rainfall")) +
  
  # Add river discharge data as a line plot
  geom_line(aes(y = madeupdischarge, color = "River Discharge")) +
  
  # Customize labels and title
  labs(x = "Date", y = "Rainfall (mm)", color = "Data Type", title = "Rainfall and River Discharge") +
  
  # Customize legend
  scale_color_manual(values = c("Rainfall" = "blue", "River Discharge" = "red")) +
  
  # Add theme
  theme_minimal()




# Plot the chart
plot(date_chr, values, type = "l", xlab = "Date", ylab = "Values", main = "Values over Time")



#Minimum and maximum values
min_index <- which.min(precip.mousehole_daily$DailyRainfall_mm)
min_value <- precip.mousehole_daily$DailyRainfall_mm[min_index]

max_index <- which.max(precip.mousehole_daily$DailyRainfall_mm)
max_value <- precip.mousehole_daily$DailyRainfall_mm[max_index]

# Print or use the results as needed
print(paste( "Min Precipitation (mm):", min_value, collapse= " "))
print(paste( "Max Precipitation (mm):", max_value, collapse = " "))

head(precip.mousehole_daily)
